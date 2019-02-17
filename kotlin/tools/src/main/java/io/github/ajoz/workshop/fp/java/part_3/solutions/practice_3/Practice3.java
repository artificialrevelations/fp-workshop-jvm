package io.github.ajoz.workshop.fp.java.part_3.solutions.practice_3;

import io.github.ajoz.workshop.fp.java.tools.Consumer1;
import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.util.NoSuchElementException;

@SuppressWarnings("unused")
abstract class Either<A, B> {
    public abstract Either<A, B> onLeft(final Consumer1<A> effect);
    public abstract Either<A, B> onRight(final Consumer1<B> effect);

    public abstract <C> Either<A, C> map(final Function1<B, C> function);
    public abstract <C> Either<C, B> mapLeft(final Function1<A, C> function);

    public abstract <C> Either<A, C> flatMap(final Function1<B, Either<A, C>> function);
    public abstract <C> Either<C, B> flatMapLeft(final Function1<A, Either<C, B>> function);

    public abstract A getLeft();

    public abstract B getRight();

    static class Left<A, B> extends Either<A, B> {
        private final A value;

        Left(final A value) {
            this.value = value;
        }

        @Override
        public Either<A, B> onLeft(final Consumer1<A> effect) {
            effect.accept(value);
            return this;
        }

        @Override
        public Either<A, B> onRight(final Consumer1<B> ignored) {
            return this;
        }

        @Override
        public <C> Either<A, C> map(final Function1<B, C> function) {
            return new Left<>(value);
        }

        @Override
        public <C> Either<C, B> mapLeft(final Function1<A, C> function) {
            return new Left<>(function.apply(value));
        }

        @Override
        public <C> Either<A, C> flatMap(final Function1<B, Either<A, C>> function) {
            return new Left<>(value);
        }

        @Override
        public <C> Either<C, B> flatMapLeft(final Function1<A, Either<C, B>> function) {
            return function.apply(value);
        }

        @Override
        public A getLeft() {
            return value;
        }

        @Override
        public B getRight() {
            throw new NoSuchElementException("There is no Right value in Left!");
        }
    }

    static class Right<A, B> extends Either<A, B> {
        private final B value;

        Right(final B value) {
            this.value = value;
        }

        @Override
        public Either<A, B> onLeft(final Consumer1<A> effect) {
            return this;
        }

        @Override
        public Either<A, B> onRight(final Consumer1<B> effect) {
            effect.accept(value);
            return this;
        }

        @Override
        public <C> Either<A, C> map(final Function1<B, C> function) {
            return new Right<>(function.apply(value));
        }

        @Override
        public <C> Either<C, B> mapLeft(final Function1<A, C> function) {
            return new Right<>(value);
        }

        @Override
        public <C> Either<A, C> flatMap(final Function1<B, Either<A, C>> function) {
            return function.apply(value);
        }

        @Override
        public <C> Either<C, B> flatMapLeft(final Function1<A, Either<C, B>> function) {
            return new Right<>(value);
        }

        @Override
        public A getLeft() {
            throw new NoSuchElementException("There is no Right value in Left!");
        }

        @Override
        public B getRight() {
            return value;
        }
    }

    private Either() {
    }
}

public class Practice3 {
    public static void main(final String[] args) {

    }
}
