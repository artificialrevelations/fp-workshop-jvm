package org.artrev.workshop.fp.tools.iterators;

import java.util.Iterator;
import java.util.NoSuchElementException;

public final class ValueIterator<T> implements Iterator<T> {
    private final T[] mValues;
    private int mCurrentIndex;

    @SafeVarargs
    public ValueIterator(final T... values) {
        this.mValues = values;
    }

    @Override
    public boolean hasNext() {
        return mCurrentIndex < mValues.length;
    }

    @Override
    public T next() {
        if (mCurrentIndex >= mValues.length) {
            throw new NoSuchElementException("This iterator has only: " + mValues.length + " values!");
        }

        final T value = mValues[mCurrentIndex];
        mCurrentIndex++;
        return value;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}