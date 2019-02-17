package io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_1;

import io.github.ajoz.workshop.fp.java.tools.Effect;
import io.github.ajoz.workshop.fp.java.tools.Supplier;

enum EnumBoolean {
    TRUE() {
        @Override
        public EnumBoolean and(final EnumBoolean other) {
            // if this is TRUE then `and` relies on other
            return other;
        }

        @Override
        public EnumBoolean or(final EnumBoolean other) {
            // if this is TRUE then `or` does not depend on other
            return this;
        }

        @Override
        public EnumBoolean not() {
            return FALSE;
        }

        @Override
        public EnumBoolean ifTrue(final Effect effect) {
            effect.perform();
            return this;
        }

        @Override
        public EnumBoolean ifFalse(final Effect effect) {
            return this; //pass through
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
            // if this is FALSE then `and` does not depend on other
            return this;
        }

        @Override
        public EnumBoolean or(final EnumBoolean other) {
            // if this is FALSE then `or` relies on other
            return other;
        }

        @Override
        public EnumBoolean not() {
            return TRUE;
        }

        @Override
        public EnumBoolean ifTrue(final Effect effect) {
            return this;
        }

        @Override
        public EnumBoolean ifFalse(final Effect effect) {
            effect.perform();
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

    public abstract EnumBoolean not();

    public abstract EnumBoolean ifTrue(final Effect effect);

    public abstract EnumBoolean ifFalse(final Effect effect);

    public abstract <A> A match(final Supplier<A> ifTrue,
                                final Supplier<A> ifFalse);
}

@SuppressWarnings("ConstantConditions")
final class Exercise1 {
    public static void main(final String[] args) {
        // simple boolean algebra:
        // and:
        System.out.println(EnumBoolean.TRUE.and(EnumBoolean.TRUE));
        System.out.println(EnumBoolean.TRUE.and(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.FALSE.and(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.FALSE.and(EnumBoolean.TRUE));

        // or:
        System.out.println(EnumBoolean.TRUE.or(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.FALSE.or(EnumBoolean.TRUE));
        System.out.println(EnumBoolean.FALSE.or(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.TRUE.or(EnumBoolean.TRUE));

        // not:
        System.out.println(EnumBoolean.TRUE.not());
        System.out.println(EnumBoolean.FALSE.not());

        // conditions as methods:
        EnumBoolean.TRUE
                .ifTrue(() -> System.out.println("EnumTrue is EnumTrue!"))
                .ifFalse(() -> System.out.println("EnumTrue is EnumFalse?"));

        EnumBoolean.FALSE
                .ifFalse(() -> System.out.println("EnumFalse is EnumFalse!"))
                .ifTrue(() -> System.out.println("EnumFalse is EnumTrue?"));

        // conditions as expressions:
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

        // using the switch:
        final EnumBoolean bool = EnumBoolean.TRUE;
        switch (bool) {
            case FALSE:
                System.out.println("Switch FALSE");
                break;
            case TRUE:
                System.out.println("Switch TRUE");
                break;
        }
    }
}
