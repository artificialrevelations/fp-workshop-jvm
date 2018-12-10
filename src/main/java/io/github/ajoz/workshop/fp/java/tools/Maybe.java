package io.github.ajoz.workshop.fp.java.tools;

import java.util.Iterator;
import java.util.Objects;

@SuppressWarnings({"unused", "WeakerAccess"})
public abstract class Maybe<A> implements Iterable<A> {

    public abstract <B> Maybe<B> map(final Function1<? super A, ? extends B> function);
    public abstract <B> Maybe<B> flatMap(final Function1<? super A, ? extends Maybe<? extends B>> function);

    public abstract A getOrElse(final A defaultValue);
    public A getOrNull() {
        return getOrElse(null);
    }
    public abstract A get();

    public abstract Maybe<A> orElse(final Maybe<A> defaultMaybe);
    public abstract Maybe<A> orElse(final Supplier<Maybe<A>> supplier);

    public abstract boolean isSome();
    public boolean isNone() {
        return !isSome();
    }

    public abstract Maybe<A> ifSome(final Consumer1<A> action);
    public abstract Maybe<A> ifNone(final Effect effect);

    public static <B> Maybe<B> ofNullable(final B value) {
        return (null != value) ? Maybe.some(value) : Maybe.none();
    }

    public static <B> Maybe<B> some(final B value) {
        return new Some<>(value);
    }

    public static <B> Maybe<B> none() {
        return new None<>();
    }

    @Override
    public Iterator<A> iterator() {
        return isSome()
                ? new ValueIterator<>(get())
                : new EmptyIterator<>();
    }

    @SuppressWarnings("unchecked")
    public static final class Some<A> extends Maybe<A> {
        private final A value;

        public Some(final A value) {
            this.value = value;
        }

        @Override

        public <B> Maybe<B> map(final Function1<? super A, ? extends B> function) {
            return Maybe.some(function.apply(value));
        }

        @Override

        public <B> Maybe<B> flatMap(final Function1<? super A, ? extends Maybe<? extends B>> function) {
            return (Maybe<B>) function.apply(value);
        }

        @Override
        public A getOrElse(final A ignored) {
            return value;
        }

        @Override
        public Maybe<A> orElse(final Maybe<A> ignored) {
            return Maybe.some(value);
        }


        @Override
        public Maybe<A> orElse(final Supplier<Maybe<A>> ignored) {
            return Maybe.some(value);
        }

        @Override
        public A get() {
            return value;
        }

        @Override
        public boolean isSome() {
            return true;
        }

        @Override
        public Maybe<A> ifSome(final Consumer1<A> action) {
            action.accept(value);
            return Maybe.some(value);
        }

        @Override
        public Maybe<A> ifNone(final Effect ignored) {
            return Maybe.some(value);
        }

        @Override
        public boolean equals(final Object other) {
            if (this == other)
                return true;
            if (other == null || getClass() != other.getClass())
                return false;

            final Some<?> some = (Some<?>) other;
            return Objects.equals(value, some.value);
        }

        @Override
        public int hashCode() {
            return value != null ? value.hashCode() : 0;
        }
    }

    public static final class None<A> extends Maybe<A> {
        @Override
        public <B> Maybe<B> map(final Function1<? super A, ? extends B> ignored) {
            return Maybe.none();
        }

        @Override

        public <B> Maybe<B> flatMap(final Function1<? super A, ? extends Maybe<? extends B>> ignored) {
            return Maybe.none();
        }

        @Override
        public A getOrElse(final A defaultValue) {
            return defaultValue;
        }

        @Override
        public Maybe<A> orElse(final Maybe<A> defaultMaybe) {
            return defaultMaybe;
        }

        @Override
        public Maybe<A> orElse(final Supplier<Maybe<A>> supplier) {
            return supplier.get();
        }

        @Override
        public A get() {
            throw new IllegalStateException("No value is stored in Nothing!");
        }

        @Override
        public boolean isSome() {
            return false;
        }

        @Override
        public Maybe<A> ifSome(final Consumer1<A> action) {
            return Maybe.none();
        }

        @Override
        public Maybe<A> ifNone(final Effect effect) {
            effect.perform();
            return Maybe.none();
        }
    }

    protected Maybe() {
    }
}
