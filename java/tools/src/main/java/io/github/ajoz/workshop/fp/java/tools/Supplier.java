package io.github.ajoz.workshop.fp.java.tools;

import io.github.ajoz.workshop.fp.java.tools.control.Maybe;
import io.github.ajoz.workshop.fp.java.tools.control.Try;

import java.util.concurrent.atomic.AtomicReference;

@SuppressWarnings("unused")
public interface Supplier<A> {
    default <B> Supplier<B> map(final Function1<A, B> function) {
        return () -> function.apply(get());
    }

    default <B> Supplier<B> flatMap(final Function1<A, Supplier<B>> function) {
        return () -> function.apply(get()).get();
    }

    A get();

    default A getOrElse(final A defaultValue) {
        try {
            return get();
        } catch (final Exception exception) {
            return defaultValue;
        }
    }

    default Try<A> tryGet() {
        try {
            return Try.success(get());
        } catch (final Exception e) {
            return Try.failure(e);
        }
    }

    default Maybe<A> maybeGet() {
        try {
            return Maybe.some(get());
        } catch (final Exception e) {
            return Maybe.none();
        }
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

    static <A> Supplier<A> of(final Supplier<A> supplier) {
        return supplier;
    }

    static <A> Supplier<A> ofChecked(final CheckedSupplier<A> supplier) {
        return () -> {
            try {
                return supplier.get();
            } catch (final Exception e) {
                throw new RuntimeException(e);
            }
        };
    }
}
