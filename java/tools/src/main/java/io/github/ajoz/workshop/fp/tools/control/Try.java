package io.github.ajoz.workshop.fp.tools.control;

import io.github.ajoz.workshop.fp.tools.CheckedSupplier;
import io.github.ajoz.workshop.fp.tools.Consumer1;
import io.github.ajoz.workshop.fp.tools.Function1;
import io.github.ajoz.workshop.fp.tools.Predicate;
import io.github.ajoz.workshop.fp.tools.Supplier;
import io.github.ajoz.workshop.fp.tools.iterators.EmptyIterator;
import io.github.ajoz.workshop.fp.tools.iterators.ValueIterator;

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
    public abstract Throwable getError();

    public abstract boolean isSuccess();
    public boolean isFailure() {
        return !isSuccess();
    }

    public abstract Try<A> ifSuccess(final Consumer1<? super A> action);
    public abstract Try<A> ifFailure(final Consumer1<Throwable> action);

    public abstract <B> B match(final Function1<? super A, ? extends B> ifSuccess,
                                final Function1<? super Throwable, ? extends B> ifFailure);

    public abstract Try<A> filter(Predicate<? super A> predicate);

    @SuppressWarnings("NullableProblems")
    @Override
    public Iterator<A> iterator() {
        return isSuccess()
                ? new ValueIterator<>(get())
                : new EmptyIterator<>();
    }

    public static <A> Try<A> of(final Supplier<A> supplier) {
        try {
            return Try.success(supplier.get());
        } catch (final Throwable t) {
            return Try.failure(t);
        }
    }

    public static <A> Try<A> ofChecked(final CheckedSupplier<A> supplier) {
        try {
            return Try.success(supplier.get());
        } catch (final Throwable t) {
            return Try.failure(t);
        }
    }

    public static <A> Try<A> ofNullable(final A value) {
        if (null != value) {
            return Try.success(value);
        }

        return Try.failure(new NullPointerException("Null value passed to ofNullable"));
    }

    public static <A, B> Function1<Try<A>, Try<B>> lift(final Function1<A, B> function) {
        return ta -> ta.map(function);
    }

    public static <A, B> Function1<A, Try<B>> liftP(final Function1<A, B> function) {
        return a -> Try.of(() -> function.apply(a));
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
        public Throwable getError() {
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
        public <B> B match(final Function1<? super A, ? extends B> ifSuccess,
                           final Function1<? super Throwable, ? extends B> ifFailure) {
            return ifSuccess.apply(value);
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

        @Override
        public String toString() {
            return String.format("Try.Success(value = %s)", value);
        }
    }

    public static final class Failure<A> extends Try<A> {
        private final Throwable error;

        private Failure(final Throwable cause) {
            this.error = cause;
        }

        @Override
        public <B> Try<B> map(final Function1<? super A, ? extends B> ignored) {
            return Try.failure(error);
        }

        @Override
        public <B> Try<B> flatMap(final Function1<? super A, ? extends Try<? extends B>> ignored) {
            return Try.failure(error);
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
            throw error;
        }

        @Override
        public A get() {
            throw new RuntimeException(error);
        }

        @Override
        public Throwable getError() {
            return error;
        }

        @Override
        public boolean isSuccess() {
            return false;
        }

        @Override
        public Try<A> recover(final Function1<? super Throwable, ? extends A> function) {
            try {
                return Try.success(function.apply(error));
            } catch (final Throwable t) {
                return Try.failure(t);
            }
        }

        @SuppressWarnings("unchecked")
        @Override
        public Try<A> recoverWith(final Function1<? super Throwable, Try<? extends A>> function) {
            try {
                return (Try<A>) function.apply(error);
            } catch (final Throwable t) {
                return Try.failure(t);
            }
        }

        @Override
        public Try<A> ifSuccess(final Consumer1<? super A> ignored) {
            return Try.failure(error);
        }

        @Override
        public Try<A> ifFailure(final Consumer1<Throwable> action) {
            action.accept(error);
            return Try.failure(error);
        }

        @Override
        public <B> B match(final Function1<? super A, ? extends B> ifSuccess,
                           final Function1<? super Throwable, ? extends B> ifFailure) {
            return ifFailure.apply(error);
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
            return error == failure.error || error.equals(failure.error);
        }

        @Override
        public int hashCode() {
            return error.hashCode();
        }

        @Override
        public String toString() {
            return String.format("Try.Failure(error = %s)", error);
        }
    }

    protected Try() {
    }
}

