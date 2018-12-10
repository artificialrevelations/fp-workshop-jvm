package io.github.ajoz.workshop.fp.java.tools;

import java.util.Iterator;
import java.util.NoSuchElementException;

public final class EmptyIterator<T> implements Iterator<T> {
    @Override
    public boolean hasNext() {
        return false;
    }

    @Override
    public T next() {
        throw new NoSuchElementException("Empty iterator has no values!");
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
