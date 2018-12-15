package io.github.ajoz.workshop.fp.java.tools;

import java.util.concurrent.atomic.AtomicReference;

@SuppressWarnings("unused")
public interface Supplier<A> {
    A get();

    default A getOrElse(final A defaultValue) {
        try {
            return get();
        } catch (final Exception exception) {
            return defaultValue;
        }
    }

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

    default Supplier<A> before(final Effect effect) {
        return () -> {
            effect.perform();
            return get();
        };
    }

    default Supplier<A> after(final Consumer1<A> effect) {
        return () -> {
            final A result = get();
            effect.accept(result);
            return result;
        };
    }

    default <B> Supplier<B> map(final Function1<A, B> function) {
        return () -> function.apply(get());
    }

    default <B> Supplier<B> flatMap(final Function1<A, Supplier<B>> function) {
        return () -> function.apply(get()).get();
    }

    static <A> Supplier<A> memoize(final Supplier<A> supplier) {
        return supplier.memoized();
    }
}
