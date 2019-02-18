package io.github.ajoz.workshop.fp.tools;

public final class Pair<A, B> {
    private final A first;
    private final B second;

    public Pair(final A first,
                final B second) {
        this.first = first;
        this.second = second;
    }

    public A getFirst() {
        return first;
    }

    public B getSecond() {
        return second;
    }
}
