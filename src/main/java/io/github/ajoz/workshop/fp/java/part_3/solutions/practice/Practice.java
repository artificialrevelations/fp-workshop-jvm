package io.github.ajoz.workshop.fp.java.part_3.solutions.practice;

import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.util.NoSuchElementException;

@SuppressWarnings({"unused", "WeakerAccess"})
abstract class Try<A> {
    private static final class Success<A> extends Try<A> {
        private final A value;

        private Success(final A value) {
            this.value = value;
        }

        @Override
        public A get() {
            return value;
        }

        @Override
        public A getOrElse(final A defaultValue) {
            return value;
        }

        @Override
        public <B> Try<B> map(final Function1<A, B> mapper) {
            return Try.success(mapper.apply(value));
        }

        @Override
        public <B> Try<B> flatMap(final Function1<A, Try<B>> mapper) {
            return mapper.apply(value);
        }

        @Override
        public Try<A> recover(final Function1<Throwable, A> fun) {
            return Try.success(value);
        }

        @Override
        public Try<A> recoverWith(final Function1<Throwable, Try<A>> fun) {
            return Try.success(value);
        }
    }

    private static final class Failure<A> extends Try<A> {
        private final Throwable error;

        private Failure(final Throwable error) {
            this.error = error;
        }

        @Override
        public A get() {
            throw new NoSuchElementException("Failure has no value!");
        }

        @Override
        public A getOrElse(final A defaultValue) {
            return defaultValue;
        }

        @Override
        public <B> Try<B> map(final Function1<A, B> mapper) {
            return Try.failure(error);
        }

        @Override
        public <B> Try<B> flatMap(final Function1<A, Try<B>> mapper) {
            return Try.failure(error);
        }

        @Override
        public Try<A> recover(final Function1<Throwable, A> fun) {
            return Try.success(fun.apply(error));
        }

        @Override
        public Try<A> recoverWith(final Function1<Throwable, Try<A>> fun) {
            return fun.apply(error);
        }
    }

    public abstract A get();

    public abstract A getOrElse(final A defaultValue);

    public abstract <B> Try<B> map(final Function1<A, B> mapper);

    public abstract <B> Try<B> flatMap(final Function1<A, Try<B>> mapper);

    public abstract Try<A> recover(final Function1<Throwable, A> fun);

    public abstract Try<A> recoverWith(final Function1<Throwable, Try<A>> fun);

    public static <A> Try<A> success(final A value) {
        return new Success<>(value);
    }

    public static <A> Try<A> failure(final Throwable error) {
        return new Failure<>(error);
    }
}

public class Practice {
    public static void main(final String[] args) {

    }
}
