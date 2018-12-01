package io.github.ajoz.workshop.fp.java.part_3.solutions.solution_1;

import io.github.ajoz.workshop.fp.java.tools.Supplier;

/*
  -- Creating Boolean from scratch --


 */
enum EnumBoolean {
    TRUE() {
        @Override
        public EnumBoolean and(final EnumBoolean other) {
            return other;
        }

        @Override
        public EnumBoolean or(EnumBoolean other) {
            return this;
        }

        @Override
        public EnumBoolean ifTrue(Runnable effect) {
            effect.run();
            return this;
        }

        @Override
        public EnumBoolean ifFalse(Runnable effect) {
            return this;
        }

        @Override
        public <A> A match(final Supplier<A> ifTrue,
                           final Supplier<A> ifFalse) {
            return ifTrue.get();
        }
    },
    FALSE() {
        @Override
        public EnumBoolean and(final EnumBoolean other) {
            return this;
        }

        @Override
        public EnumBoolean or(EnumBoolean other) {
            return other;
        }

        @Override
        public EnumBoolean ifTrue(Runnable effect) {
            return this;
        }

        @Override
        public EnumBoolean ifFalse(Runnable effect) {
            effect.run();
            return this;
        }

        @Override
        public <A> A match(final Supplier<A> ifTrue,
                           final Supplier<A> ifFalse) {
            return ifFalse.get();
        }
    };

    public abstract EnumBoolean and(final EnumBoolean other);

    public abstract EnumBoolean or(final EnumBoolean other);

    public abstract EnumBoolean ifTrue(final Runnable effect);

    public abstract EnumBoolean ifFalse(final Runnable effect);

    public abstract <A> A match(final Supplier<A> ifTrue,
                                final Supplier<A> ifFalse);
}

public class Exercise1 {
    public static void main(String[] args) {
        System.out.println("" + EnumBoolean.TRUE.and(EnumBoolean.TRUE));
        System.out.println(EnumBoolean.TRUE.and(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.FALSE.and(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.TRUE.or(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.FALSE.or(EnumBoolean.TRUE));
        System.out.println(EnumBoolean.FALSE.or(EnumBoolean.FALSE));

        EnumBoolean.TRUE
                .ifTrue(() -> System.out.println("EnumTrue is EnumTrue!"))
                .ifFalse(() -> System.out.println("EnumTrue is EnumFalse?"));

        EnumBoolean.FALSE
                .ifTrue(() -> System.out.println("EnumFalse is EnumTrue?"))
                .ifFalse(() -> System.out.println("EnumFalse is EnumFalse!"));

        final String trueMessage =
                EnumBoolean.TRUE
                        .match(
                                () -> "Matching EnumTrue to EnumTrue!",
                                () -> "Matching EnumTrue to EnumFalse?"
                        );

        System.out.println(trueMessage);

        final String falseMessage =
                EnumBoolean.FALSE
                        .match(
                                () -> "Matching EnumFalse to EnumTrue?",
                                () -> "Matching EnumFalse to EnumFalse!"
                        );

        System.out.println(falseMessage);
    }
}
