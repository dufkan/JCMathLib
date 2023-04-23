package opencrypto.jcmathlib;

import javacard.framework.ISOException;

/**
 * Based on BigNat library from <a href="https://ovchip.cs.ru.nl/OV-chip_2.0">OV-chip project.</a> by Radboud University Nijmegen
 *
 * @author Vasilios Mavroudis and Petr Svenda and Antonin Dufka
 */
public class BigNatInternal {
    protected final ResourceManager rm;
    private final boolean ALLOW_RUNTIME_REALLOCATION = false;
    private static final int DIGIT_MASK = 0xffff, DIGIT_LEN = 16, DOUBLE_DIGIT_LEN = 32, POSITIVE_DOUBLE_DIGIT_MASK = 0x7fffffff;

    private short[] value;
    private short size = -1; // The current size of internal representation in shorts.
    private short sizeBytes = -1; // The current size of internal representation in bytes.
    private byte allocatorType;

    /**
     * Construct a BigNat of a given size in bytes.
     */
    public BigNatInternal(short size, byte allocatorType, ResourceManager rm) {
        this.rm = rm;
        allocateStorageArray(size, allocatorType);
    }

    /**
     * Allocates required underlying storage array.
     *
     * @param maxSize maximum size of this BigNat in bytes
     * @param allocatorType type of allocator storage
     */
    private void allocateStorageArray(short maxSize, byte allocatorType) {
        this.size = (short) ((short) (maxSize + 1) / 2);
        this.sizeBytes = maxSize;
        this.allocatorType = allocatorType;
        this.value = rm.memAlloc.allocateShortArray(size, allocatorType);
    }

    /**
     * Set value of this from a byte array representation.
     *
     * @param source the byte array
     * @param sourceOffset offset in the byte array
     * @param length length of the value representation
     * @return number of bytes read
     */
    public short fromByteArray(byte[] source, short sourceOffset, short length) {
        setSize(length <= (short) (2 * value.length) ? length : (short) (2 * value.length));
        if ((short) (sizeBytes % 2) == 1) {
            value[0] = source[sourceOffset];
            --sourceOffset;
        }
        for (short i = (short) (sizeBytes % 2); i < size; ++i) {
            value[i] = (short) ((short) (source[(short) (sourceOffset + 2 * i)] & 0xff) << 8);
            value[i] |= (short) (source[(short) (sourceOffset + 2 * i + 1)] & 0xff);
        }
        return sizeBytes;
    }

    /**
     * Serialize this BigNat value into a provided byte array.
     *
     * @param dst the byte array
     * @param dstOffset offset in the byte array
     * @return number of bytes written
     */
    public short copyToByteArray(byte[] dst, short dstOffset) {
        if (sizeBytes % 2 == 1) {
            dst[dstOffset] = (byte) (value[0] & 0xff);
            --dstOffset;
        }
        for (short i = (short) (sizeBytes % 2); i < size; ++i) {
            dst[(short) (2 * i + dstOffset)] = (byte) ((short) (value[i] >> 8) & 0xff);
            dst[(short) (2 * i + 1 + dstOffset)] = (byte) (value[i] & 0xff);
        }
        return sizeBytes;
    }

    /**
     * Get size of this BigNat in bytes.
     *
     * @return size in bytes
     */
    public short length() {
        return sizeBytes;
    }

    /**
     * Sets the size of this BigNat in bytes.
     *
     * Previous value is kept so value is either non-destructively trimmed or enlarged.
     *
     * @param newSize the new size
     */
    public void setSize(short newSize) {
        if (newSize < 0 || newSize > (short) (2 * value.length)) {
            ISOException.throwIt(ReturnCodes.SW_BIGNAT_RESIZETOLONGER);
        }
        size = (short) ((short) (newSize + 1) / 2);
        sizeBytes = newSize;
    }

    /**
     * Set size of this BigNat to the maximum size given during object creation.
     *
     * @param erase flag indicating whether to set internal representation to zero
     */
    public void setSizeToMax(boolean erase) {
        setSize((short) (2 * value.length));
        if (erase) {
            erase();
        }
    }

    /**
     * Resize this BigNat value to given size in bytes. May result in truncation.
     *
     * @param newSize new size in bytes
     */
    public void resize(short newSize) {
        if (newSize > (short) (2 * value.length)) {
            if (!ALLOW_RUNTIME_REALLOCATION) {
                ISOException.throwIt(ReturnCodes.SW_BIGNAT_REALLOCATIONNOTALLOWED);
            }
            allocateStorageArray(newSize, allocatorType);
        }
        short newSizeShort = (short) ((newSize + 1) / 2);

        if (sizeBytes == newSize) {
            return;
        }
        if (sizeBytes >= newSize) {
            for (short i = (short) (size - newSizeShort), j = 0; j < newSizeShort; ++i, ++j) {
                value[j] = value[i];
            }
            for (short j = newSizeShort; j < value.length; ++j) {
                value[j] = (short) 0;
            }
        } else {
            short end = (short) (newSizeShort - size);
            for (short i = 0; i < size; ++i) {
                value[(short) (i + end)] = value[i];
            }
            for (short i = 0; i < end; ++i) {
                value[i] = 0;
            }
        }
        setSize(newSize);
    }

    /**
     * Append zeros to reach the defined byte length and store the result in an output buffer.
     *
     * @param targetLength required length including appended zeroes
     * @param outBuffer    output buffer for value with appended zeroes
     * @param outOffset    start offset inside outBuffer for write
     */
    public void appendZeros(short targetLength, byte[] outBuffer, short outOffset) {
        copyToByteArray(outBuffer, outOffset);
        for(short i = sizeBytes; i < targetLength; ++i) {
            outBuffer[(short) (outOffset + i)] = 0;
        }
    }

    /**
     * Prepend zeros to reach the defined byte length and store the result in an output buffer.
     *
     * @param targetLength required length including prepended zeroes
     * @param outBuffer    output buffer for value with prepended zeroes
     * @param outOffset    start offset inside outBuffer for write
     */
    public void prependZeros(short targetLength, byte[] outBuffer, short outOffset) {
        short start = (short) (targetLength - sizeBytes);
        for (short i = 0; i < start; ++i) {
            outBuffer[(short) (outOffset + i)] = 0;
        }
        copyToByteArray(outBuffer, (short) (outOffset + start));
    }

    /**
     * Remove leading zeroes from this BigNat and decrease its byte size accordingly.
     */
    public void shrink() {
        short i;
        for (i = 0; i < size; i++) { // Find first non-zero byte
            if (value[i] != 0) {
                break;
            }
        }

        short newSize = (short) (2 * (short) (size - i));
        if (newSize < 0) {
            ISOException.throwIt(ReturnCodes.SW_BIGNAT_INVALIDRESIZE);
        }
        resize(newSize);
    }

    /**
     * Set this BigNat value to zero. Previous size is kept.
     */
    public void zero() {
        for (short i = 0; i < size; ++i) {
            value[i] = 0;
        }
    }

    /**
     * Erase the internal array of this BigNat.
     */
    public void erase() {
        for (short i = 0; i < value.length; ++i) {
            value[i] = 0;
        }
    }

    /**
     * Set this BigNat to a given value. Previous size is kept.
     */
    public void setValue(byte newValue) {
        zero();
        value[(short) (size - 1)] = (short) (newValue & 0xff);
    }

    /**
     * Set this BigNat to a given value. Previous size is kept.
     */
    public void setValue(short newValue) {
        zero();
        value[(short) (size - 1)] = newValue;
    }

    /**
     * Copies a BigNat into this without changing size. May throw an exception if this is too small.
     */
    public void copy(BigNatInternal other) {
        short thisStart, otherStart, len;
        if (size >= other.size) {
            thisStart = (short) (size - other.size);
            otherStart = 0;
            len = other.size;

            if (thisStart > 0) {
                for (short i = 0; i < thisStart; ++i) {
                    value[i] = 0;
                }
            }
        } else {
            thisStart = 0;
            otherStart = (short) (other.size - size);
            len = size;
            // Verify here that other have leading zeroes up to otherStart
            for (short i = 0; i < otherStart; i++) {
                if (other.value[i] != 0) {
                    ISOException.throwIt(ReturnCodes.SW_BIGNAT_INVALIDCOPYOTHER);
                }
            }
        }
        for (short i = 0; i < len; ++i) {
            value[(short) (thisStart + i)] = other.value[(short) (otherStart + i)];
        }
    }

    /**
     * Copies a BigNat into this including its size. May require reallocation.
     */
    public void clone(BigNatInternal other) {
        if (other.sizeBytes > (short) (2 * value.length)) {
            if (!ALLOW_RUNTIME_REALLOCATION) {
                ISOException.throwIt(ReturnCodes.SW_BIGNAT_REALLOCATIONNOTALLOWED);
            }
            allocateStorageArray(other.sizeBytes, allocatorType);
        }

        for (short i = 0; i < (short) other.value.length; ++i) {
            value[i] = other.value[i];
        }

        for (short i = other.size; i < (short) (other.size + (short) value.length - (short) other.value.length); ++i) {
            value[i] = 0;
        }
        setSize(other.sizeBytes);
    }

    /**
     * Test equality with zero.
     */
    public boolean isZero() {
        for (short i = 0; i < size; i++) {
            if (value[i] != 0) {
                return false; // CTO
            }
        }
        return true;
    }

    /**
     * Test equality with one.
     */
    public boolean isOne() {
        for (short i = 0; i < (short) (size - 1); i++) {
            if (value[i] != 0) {
                return false; // CTO
            }
        }
        return value[(short) (size - 1)] == (byte) 0x01;
    }

    /**
     * Check if stored BigNat is odd.
     */
    public boolean isOdd() {
        return (byte) (value[(short) (size - 1)] & (byte) 1) != (byte) 0;
    }

    /**
     * Returns true if this BigNat is lesser than the other.
     */
    public boolean isLesser(BigNatInternal other) {
        return isLesser(other, (short) 0, (short) 0);
    }

    /**
     * Returns true if this is lesser than other shifted by a given number of digits.
     */
    private boolean isLesser(BigNatInternal other, short shift, short start) {
        short j = (short) (other.size + shift - size + start);

        for (short i = start; i < j; ++i) {
            if (other.value[i] != 0) {
                return true;
            }
        }

        for (short i = start; i < size; i++, j++) {
            int thisValue = value[i] & DIGIT_MASK;
            int otherValue = (j >= 0 && j < other.size) ? (other.value[j] & DIGIT_MASK) : 0;
            if (thisValue < otherValue) {
                return true; // CTO
            }
            if (thisValue > otherValue) {
                return false;
            }
        }
        return false;
    }

    /**
     * Value equality check.
     *
     * @param other BigNat to compare
     * @return true if this and other have the same value, false otherwise.
     */
    public boolean equals(BigNatInternal other) {
        short diff = (short) (size - other.size);

        if (diff == 0) {
            for (short i = 0; i < size; ++i) {
                if (value[i] != other.value[i]) {
                    return false;
                }
            }
            return true;
        }


        if (diff < 0) {
            short end = (short) -diff;
            for (short i = 0; i < end; ++i) {
                if (other.value[i] != (byte) 0) {
                    return false;
                }
            }
            for (short i = 0; i < size; ++i) {
                if (value[i] != other.value[(short) (end + i)]) {
                    return false;
                }
            }
            return true;
        }

        short end = diff;
        for (short i = (short) 0; i < end; ++i) {
            if (value[i] != (byte) 0) {
                return false;
            }
        }
        for (short i = 0; i < other.size; ++i) {
            if (value[(short) (end + i)] != other.value[i]) {
                return false;
            }
        }
        return true;
    }

    /**
     * Increment this BigNat.
     */
    public void increment() {
        for (short i = (short) (size - 1); i >= 0; i--) {
            int tmp = value[i] & 0xffff;
            value[i] = (short) (tmp + 1);
            if (tmp < 0xffff) {
                break; // CTO
            }
        }
    }

    /**
     * Decrement this BigNat.
     */
    public void decrement() {
        int tmp;
        for (short i = (short) (size - 1); i >= 0; i--) {
            tmp = value[i] & 0xffff;
            value[i] = (short) (tmp - 1);
            if (tmp != 0) {
                break; // CTO
            }
        }
    }

    /**
     * Add short value to this BigNat
     *
     * @param other short value to add
     */
    public short add(short other) {
        rm.BN_WORD.lock();
        rm.BN_WORD.setValue(other);
        short carry = add(rm.BN_WORD);
        rm.BN_WORD.unlock();
        return carry;
    }

    /**
     * Adds other to this. Outputs carry bit.
     *
     * @param other BigNat to add
     * @return true if carry occurs, false otherwise
     */
    public short add(BigNatInternal other) {
        return add(other, (short) 0, (short) 1);
    }

    /**
     * Computes other * multiplier, shifts the results by shift and adds it to this.
     * Multiplier must be in range [0; 2^8 - 1].
     * This must be large enough to fit the results.
     */
    private short add(BigNatInternal other, short shift, int multiplier) {
        int acc = 0;
        short i = (short) (other.size - 1);
        short j = (short) (size - 1 - shift);
        for (; i >= 0 && j >= 0; i--, j--) {
            acc += (value[j] & DIGIT_MASK) + multiplier * (other.value[i] & DIGIT_MASK);

            value[j] = (short) (acc & DIGIT_MASK);
            acc = (acc >> DIGIT_LEN) & DIGIT_MASK;
        }

        for (; acc > 0 && j >= 0; --j) {
            acc += value[j] & DIGIT_MASK;
            value[j] = (short) (acc & DIGIT_MASK);
            acc = (acc >> DIGIT_LEN) & DIGIT_MASK;
        }

        // output carry bit if present
        return (short) (((short) ((acc | -acc) >>> 31) & 0x01) << 15);
    }

    /**
     * Subtract provided other BigNat from this BigNat.
     *
     * @param other BigNat to be subtracted from this
     */
    public void subtract(BigNatInternal other) {
        subtract(other, (short) 0, (short) 1);
    }

    /**
     * Computes other * multiplier, shifts the results by shift and subtract it from this.
     * Multiplier must be in range [0; 2^8 - 1].
     */
    private void subtract(BigNatInternal other, short shift, int multiplier) {
        int acc = 0;
        short i = (short) (size - 1 - shift);
        short j = (short) (other.size - 1);
        for (; i >= 0 && j >= 0; i--, j--) {
            acc += multiplier * (other.value[j] & DIGIT_MASK);
            int tmp = (value[i] & DIGIT_MASK) - (acc & DIGIT_MASK);

            value[i] = (short) (tmp & DIGIT_MASK);
            acc = (acc >> DIGIT_LEN) & DIGIT_MASK;
            if (tmp < 0) {
                acc++;
            }
        }

        // deal with carry as long as there are digits left in this
        for (; i >= 0 && acc != 0; --i) {
            int tmp = (value[i] & DIGIT_MASK) - (acc & DIGIT_MASK);
            value[i] = (short) (tmp & DIGIT_MASK);
            acc = (acc >> DIGIT_LEN) & DIGIT_MASK;
            if (tmp < 0) {
                acc++;
            }
        }
    }

    /**
     * Multiplies x and y using software multiplications and stores results into this.
     *
     * @param x left operand
     * @param y right operand
     */
    public void mult(BigNatInternal x, BigNatInternal y) {
        for (short i = (short) (y.size - 1); i >= 0; i--) {
            add(x, (short) (y.size - 1 - i), y.value[i] & DIGIT_MASK);
        }
    }

    /**
     * Right bit shift with carry
     *
     * @param bits number of bits to shift by
     * @param carry XORed into the highest byte
     */
    protected void shiftRight(short bits, int carry) {
        // assumes 0 <= bits < 8
        int mask = (1 << bits) - 1; // lowest `bits` bits set to 1
        for (short i = 0; i < size; i++) {
            int current = value[i] & 0xffff;
            int previous = current;
            current >>= bits;
            value[i] = (short) (current | carry);
            carry = previous & mask;
            carry <<= 16 - bits;
        }
    }

    /**
     * Right bit shift
     *
     * @param bits number of bits to shift by
     */
    public void shiftRight(short bits) {
        shiftRight(bits, (short) 0);
    }

    /**
     * Divide this by divisor and store the remained in this and quotient in quotient.
     *
     * Quadratic complexity in digit difference of this and divisor.
     *
     * @param divisor non-zero number
     * @param quotient may be null
     */
    public void remainderDivide(BigNatInternal divisor, BigNatInternal quotient) {
        if (quotient != null) {
            quotient.zero();
        }

        short divisorIndex = 0;
        while (divisor.value[divisorIndex] == 0) {
            divisorIndex++;
        }

        short divisorShift = (short) (size - divisor.size + divisorIndex);
        short divisionRound = 0;
        int firstDivisorDigit = divisor.value[divisorIndex] & DIGIT_MASK;
        short divisorBitShift = (short) (highestOneBit((short) (firstDivisorDigit + 1)) - 1);
        short secondDivisorDigit = divisorIndex < (short) (divisor.size - 1) ? divisor.value[(short) (divisorIndex + 1)] : 0;
        short thirdDivisorDigit = divisorIndex < (short) (divisor.size - 2) ? divisor.value[(short) (divisorIndex + 2)] : 0;

        while (divisorShift >= 0) {
            while (!isLesser(divisor, divisorShift, (short) (divisionRound > 0 ? divisionRound - 1 : 0))) {
                int dividentDigits = divisionRound == 0 ? 0 : ((value[(short) (divisionRound - 1)]) << DIGIT_LEN);
                dividentDigits |= value[divisionRound] & DIGIT_MASK;

                int divisorDigit;
                if (dividentDigits < 0) {
                    dividentDigits = (dividentDigits >>> 1) & POSITIVE_DOUBLE_DIGIT_MASK;
                    divisorDigit = (firstDivisorDigit >>> 1) & POSITIVE_DOUBLE_DIGIT_MASK;
                } else {
                    short dividentBitShift = (short) (highestOneBit(dividentDigits) - 1);
                    short bitShift = dividentBitShift <= divisorBitShift ? dividentBitShift : divisorBitShift;

                    dividentDigits = shiftBits(
                            dividentDigits, divisionRound < (short) (size - 1) ? value[(short) (divisionRound + 1)] : 0,
                            divisionRound < (short) (size - 2) ? value[(short) (divisionRound + 2)] : 0,
                            bitShift
                    );
                    divisorDigit = shiftBits(firstDivisorDigit, secondDivisorDigit, thirdDivisorDigit, bitShift);
                }

                int multiple = dividentDigits / (divisorDigit + 1);
                if (multiple < 1) {
                    multiple = 1;
                }

                subtract(divisor, divisorShift, multiple);

                if (quotient != null) {
                    int quotientDigit = (quotient.value[(short) (quotient.size - 1 - divisorShift)] & DIGIT_MASK) + multiple;
                    quotient.value[(short) (quotient.size - 1 - divisorShift)] = (short) quotientDigit;
                }
            }
            divisionRound++;
            divisorShift--;
        }
    }

    /**
     * Get the index of the highest bit set to 1. Used in remainderDivide.
     */
    private static short highestOneBit(int x) {
        for (short i = 0; i < DOUBLE_DIGIT_LEN; ++i) {
            if (x < 0) {
                return i;
            }
            x <<= 1;
        }
        return DOUBLE_DIGIT_LEN;
    }

    /**
     * Shift to the left and fill. Used in remainderDivide.
     *
     * @param high most significant 16 bits
     * @param middle middle 8 bits
     * @param low least significant 8 bits
     * @param shift the left shift
     * @return most significant 16 bits as short
     */
    private static int shiftBits(int high, short middle, short low, short shift) {
        // shift high
        high <<= shift;

        // merge middle bits
        short mask = (short) (DIGIT_MASK << (shift >= DIGIT_LEN ? 0 : DIGIT_LEN - shift));
        int bits = (middle & mask) & DIGIT_MASK;
        if (shift > DIGIT_LEN) {
            bits <<= shift - DIGIT_LEN;
        } else {
            bits >>>= DIGIT_LEN - shift;
        }
        high |= bits;

        if (shift <= DIGIT_LEN) {
            return high;
        }

        // merge low bits
        mask = (short) (DIGIT_MASK << DOUBLE_DIGIT_LEN - shift);
        bits = ((low & mask) & DIGIT_MASK) >> DOUBLE_DIGIT_LEN - shift;
        high |= bits;

        return high;
    }

    /// [DependencyBegin:ObjectLocker]
    private boolean ERASE_ON_LOCK = false;
    private boolean ERASE_ON_UNLOCK = false;
    private boolean locked = false; // Logical flag to store info if this BigNat is currently used for some operation. Used as a prevention of unintentional parallel use of same temporary pre-allocated BigNat.

    /**
     * Lock/reserve this BigNat for subsequent use.
     * Used to protect corruption of pre-allocated temporary BigNat used in different,
     * potentially nested operations. Must be unlocked by unlock() later on.
     */
    public void lock() {
        if (locked) {
            ISOException.throwIt(ReturnCodes.SW_LOCK_ALREADYLOCKED);
        }
        locked = true;
        if (ERASE_ON_LOCK) {
            erase();
        }
    }

    /**
     * Unlock/release this BigNat from use. Used to protect corruption
     * of pre-allocated temporary BigNat used in different nested operations.
     * Must be locked before.
     */
    public void unlock() {
        if (!locked) {
            ISOException.throwIt(ReturnCodes.SW_LOCK_NOTLOCKED);
        }
        locked = false;
        if (ERASE_ON_UNLOCK) {
            erase();
        }
    }

    /**
     * Return current state of logical lock of this object
     *
     * @return true if object is logically locked (reserved), false otherwise
     */
    public boolean isLocked() {
        return locked;
    }
    /// [DependencyEnd:ObjectLocker]
}
