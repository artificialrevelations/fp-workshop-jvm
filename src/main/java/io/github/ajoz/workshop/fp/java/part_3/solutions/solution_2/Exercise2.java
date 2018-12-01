package io.github.ajoz.workshop.fp.java.part_3.solutions.solution_2;

import io.github.ajoz.workshop.fp.java.tools.Function2;
import io.github.ajoz.workshop.fp.java.tools.Supplier;

abstract class SealedBoolean {
    private SealedBoolean() {}

    public abstract SealedBoolean and(final SealedBoolean other);

    public abstract SealedBoolean or(final SealedBoolean other);

    public abstract SealedBoolean ifTrue(final Runnable effect);

    public abstract SealedBoolean ifFalse(final Runnable effect);

    public abstract <A> A match(final Supplier<A> ifTrue,
                                final Supplier<A> ifFalse);

    public static final SealedBoolean TRUE = new True();
    public static final SealedBoolean FALSE = new False();

    private static class True extends SealedBoolean {
        @Override
        public SealedBoolean and(final SealedBoolean other) {
            return other;
        }

        @Override
        public SealedBoolean or(final SealedBoolean other) {
            return this;
        }

        @Override
        public SealedBoolean ifTrue(final Runnable effect) {
            effect.run();
            return this;
        }

        @Override
        public SealedBoolean ifFalse(final Runnable effect) {
            return this;
        }

        @Override
        public <A> A match(final Supplier<A> ifTrue,
                           final Supplier<A> ifFalse) {
            return ifTrue.get();
        }

        @Override
        public String toString() {
            return "SealedTrue";
        }
    }

    private static class False extends SealedBoolean {

        @Override
        public SealedBoolean and(final SealedBoolean other) {
            return this;
        }

        @Override
        public SealedBoolean or(final SealedBoolean other) {
            return other;
        }

        @Override
        public SealedBoolean ifTrue(final Runnable effect) {
            return this;
        }

        @Override
        public SealedBoolean ifFalse(final Runnable effect) {
            effect.run();
            return this;
        }

        @Override
        public <A> A match(final Supplier<A> ifTrue,
                           final Supplier<A> ifFalse) {
            return ifFalse.get();
        }

        @Override
        public String toString() {
            return "SealedFalse";
        }
    }
}

public class Exercise2 {
    public static void main(String[] args) {
        System.out.println(SealedBoolean.TRUE.and(SealedBoolean.TRUE));
        System.out.println(SealedBoolean.TRUE.and(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.FALSE.and(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.TRUE.or(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.FALSE.or(SealedBoolean.TRUE));
        System.out.println(SealedBoolean.FALSE.or(SealedBoolean.FALSE));

        SealedBoolean.TRUE
                .ifTrue(() -> System.out.println("SealedTrue is SealedTrue!"))
                .ifFalse(() -> System.out.println("SealedTrue is SealedFalse?"));

        SealedBoolean.FALSE
                .ifFalse(() -> System.out.println("SealedFalse is SealedFalse!"))
                .ifTrue(() -> System.out.println("SealedFalse is SealedTrue?"));

        final String trueMessage =
                SealedBoolean.TRUE
                        .match(
                                () -> "Matching SealedTrue to SealedTrue!",
                                () -> "Matching SealedTrue to SealedFalse?"
                        );

        System.out.println(trueMessage);

        final String falseMessage =
                SealedBoolean.FALSE
                        .match(
                                () -> "Matching SealedFalse to SealedTrue?",
                                () -> "Matching SealedFalse to SealedFalse!"
                        );

        System.out.println(falseMessage);
    }
}
