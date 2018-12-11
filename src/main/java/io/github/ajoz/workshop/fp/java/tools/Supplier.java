package io.github.ajoz.workshop.fp.java.tools;

import java.util.concurrent.atomic.AtomicReference;

@SuppressWarnings("unused")
public interface Supplier<A> {
    A get();

    default Supplier<A> memoized() {
        final AtomicReference<A> value = new AtomicReference<>();
        return () -> {
            synchronized (value) {
                if (value.get() == null) {
                    value.set(get());
                }
                return value.get();
            }
        };
    }

    static <A> Supplier<A> memoize(final Supplier<A> supplier) {
        return supplier.memoized();
    }

    default Supplier<A> before(final Effect effect) {
        final Supplier<A> self = this;
        return () -> {
            effect.perform();
            return self.get();
        };
    }

    default Supplier<A> after(final Consumer1<A> effect) {
        final Supplier<A> self = this;
        return () -> {
            final A result = self.get();
            effect.accept(result);
            return result;
        };
    }

    default <B> Supplier<B> map(final Function1<A, B> function) {
        return () -> function.apply(this.get());
    }

    default <B> Supplier<B> flatMap(final Function1<A, Supplier<B>> function) {
        return () -> function.apply(this.get()).get();
    }
}
