package io.github.ajoz.workshop.fp.java.tools;

import java.util.Iterator;
import java.util.NoSuchElementException;

@SuppressWarnings({"unused", "WeakerAccess"})
public abstract class Try<A> implements Iterable<A> {

    public abstract <B> Try<B> map(final Function1<? super A, ? extends B> function);
    public abstract <B> Try<B> flatMap(final Function1<? super A, ? extends Try<? extends B>> function);
    public abstract Try<A> recover(final Function1<? super Throwable, ? extends A> function);
    public abstract Try<A> recoverWith(final Function1<? super Throwable, Try<? extends A>> function);

    public abstract Try<A> orElse(final Try<A> defaultTry);
    public abstract Try<A> orElse(final Supplier<Try<A>> supplier);

    @Deprecated
    public abstract A getChecked() throws Throwable;
    public abstract A getOrElse(final A defaultValue);
    public A getOrNull() {
        return getOrElse(null);
    }
    public abstract A get();
    public abstract Throwable getCause();

    public abstract boolean isSuccess();
    public boolean isFailure() {
        return !isSuccess();
    }

    public abstract Try<A> ifSuccess(final Consumer1<? super A> action);
    public abstract Try<A> ifFailure(final Consumer1<Throwable> action);

    public abstract Try<A> filter(Predicate<? super A> predicate);

    @Override
    public Iterator<A> iterator() {
        return isSuccess()
                ? new ValueIterator<>(get())
                : new EmptyIterator<>();
    }

    public static <R> Try<R> of(final Supplier<R> supplier) {
        try {
            return Try.success(supplier.get());
        } catch (final Throwable t) {
            return Try.failure(t);
        }
    }

    public static <R> Try<R> ofChecked(final CheckedSupplier<R> supplier) {
        try {
            return Try.success(supplier.get());
        } catch (final Throwable t) {
            return Try.failure(t);
        }
    }

    public static <R> Try<R> ofNullable(final R value) {
        if (null != value) {
            return Try.success(value);
        }

        return Try.failure(new NullPointerException("Null value passed to fromNullable"));
    }

    public static <R> Try<R> failure(final Throwable throwable) {
        return new Failure<>(throwable);
    }

    public static <R> Try<R> success(final R value) {
        return new Success<>(value);
    }

    public static final class Success<A> extends Try<A> {
        private final A value;

        private Success(final A value) {
            this.value = value;
        }

        @Override

        public <B> Try<B> map(final Function1<? super A, ? extends B> function) {
            try {
                return Try.success(function.apply(value));
            } catch (final Throwable throwable) {
                return Try.failure(throwable);
            }
        }

        @SuppressWarnings("unchecked")
        @Override
        public <B> Try<B> flatMap(final Function1<? super A, ? extends Try<? extends B>> function) {
            try {
                return (Try<B>) function.apply(value);
            } catch (final Throwable throwable) {
                return Try.failure(throwable);
            }
        }

        @Override
        public A getOrElse(final A defaultValue) {
            return value;
        }

        @Override

        public Try<A> orElse(final Try<A> defaultTry) {
            return Try.success(value);
        }

        @Override

        public Try<A> orElse(final Supplier<Try<A>> supplier) {
            return Try.success(value);
        }

        @Override
        public A getChecked() {
            return value;
        }

        @Override
        public A get() {
            return value;
        }

        @Override
        public Throwable getCause() {
            throw new UnsupportedOperationException("Success is not a failure!");
        }

        @Override
        public boolean isSuccess() {
            return true;
        }

        @Override

        public Try<A> recover(final Function1<? super Throwable, ? extends A> ignored) {
            return Try.success(value);
        }

        @Override

        public Try<A> recoverWith(final Function1<? super Throwable, Try<? extends A>> ignored) {
            return Try.success(value);
        }

        @Override

        public Try<A> ifSuccess(final Consumer1<? super A> action) {
            action.accept(value);
            return Try.success(value);
        }

        @Override

        public Try<A> ifFailure(final Consumer1<Throwable> ignored) {
            return Try.success(value);
        }

        @Override
        public Try<A> filter(final Predicate<? super A> predicate) {
            try {
                if (predicate.test(value)) {
                    return Try.success(value);
                } else {
                    return Try.failure(new NoSuchElementException("Success does not satisfy the Predicate!"));
                }
            } catch (final Throwable t) {
                return Try.failure(t);
            }
        }

        @Override
        public boolean equals(final Object other) {
            if (other == this) {
                return true;
            }

            if (!(other instanceof Success)) {
                return false;
            }

            final Success<?> success = (Success<?>) other;
            return value == success.value || value.equals(success.value);
        }

        @Override
        public int hashCode() {
            return value.hashCode();
        }
    }

    public static final class Failure<A> extends Try<A> {
        private final Throwable cause;

        private Failure(final Throwable cause) {
            this.cause = cause;
        }

        @Override
        public <B> Try<B> map(final Function1<? super A, ? extends B> ignored) {
            return Try.failure(cause);
        }

        @Override
        public <B> Try<B> flatMap(final Function1<? super A, ? extends Try<? extends B>> ignored) {
            return Try.failure(cause);
        }

        @Override
        public A getOrElse(final A defaultValue) {
            return defaultValue;
        }

        @Override
        public Try<A> orElse(final Try<A> defaultTry) {
            return defaultTry;
        }

        @Override
        public Try<A> orElse(final Supplier<Try<A>> supplier) {
            try {
                return supplier.get();
            } catch (final Throwable t) {
                return Try.failure(t);
            }
        }

        @Override
        public A getChecked() throws Throwable {
            throw cause;
        }

        @Override
        public A get() {
            throw new RuntimeException(cause);
        }

        @Override
        public Throwable getCause() {
            return cause;
        }

        @Override
        public boolean isSuccess() {
            return false;
        }

        @Override
        public Try<A> recover(final Function1<? super Throwable, ? extends A> function) {
            try {
                return Try.success(function.apply(cause));
            } catch (final Throwable t) {
                return Try.failure(t);
            }
        }

        @SuppressWarnings("unchecked")
        @Override
        public Try<A> recoverWith(final Function1<? super Throwable, Try<? extends A>> function) {
            try {
                return (Try<A>) function.apply(cause);
            } catch (final Throwable t) {
                return Try.failure(t);
            }
        }

        @Override
        public Try<A> ifSuccess(final Consumer1<? super A> ignored) {
            return Try.failure(cause);
        }

        @Override
        public Try<A> ifFailure(final Consumer1<Throwable> action) {
            action.accept(cause);
            return Try.failure(cause);
        }

        @Override
        public Try<A> filter(final Predicate<? super A> predicate) {
            return this;
        }

        @Override
        public boolean equals(final Object other) {
            if (other == this) {
                return true;
            }

            if (!(other instanceof Failure)) {
                return false;
            }

            final Failure<?> failure = (Failure<?>) other;
            return cause == failure.cause || cause.equals(failure.cause);
        }

        @Override
        public int hashCode() {
            return cause.hashCode();
        }
    }

    protected Try() {
    }
}

