package io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_2;

import io.github.ajoz.workshop.fp.java.tools.Supplier;

@SuppressWarnings({"StaticInitializerReferencesSubClass", "WeakerAccess"})
abstract class SealedBoolean {
    private SealedBoolean() {
    }

    public abstract SealedBoolean and(final SealedBoolean other);

    public abstract SealedBoolean or(final SealedBoolean other);

    public abstract SealedBoolean not();

    public abstract SealedBoolean ifTrue(final Runnable effect);

    public abstract SealedBoolean ifFalse(final Runnable effect);

    public abstract <A> A match(final Supplier<A> ifTrue,
                                final Supplier<A> ifFalse);

    // This is here just to emulate the way how the enum is used
    // this can lead to a class loader deadlock so beware
    public static final SealedBoolean TRUE = new True();
    public static final SealedBoolean FALSE = new False();

    public static class True extends SealedBoolean {
        @Override
        public SealedBoolean and(final SealedBoolean other) {
            return other;
        }

        @Override
        public SealedBoolean or(final SealedBoolean other) {
            return this;
        }

        @Override
        public SealedBoolean not() {
            return FALSE;
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

    public static class False extends SealedBoolean {

        @Override
        public SealedBoolean and(final SealedBoolean other) {
            return this;
        }

        @Override
        public SealedBoolean or(final SealedBoolean other) {
            return other;
        }

        @Override
        public SealedBoolean not() {
            return TRUE;
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

class Exercise2 {
    public static void main(final String[] args) {
        // simple boolean algebra:
        // and:
        System.out.println(SealedBoolean.TRUE.and(SealedBoolean.TRUE));
        System.out.println(SealedBoolean.TRUE.and(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.FALSE.and(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.FALSE.and(SealedBoolean.TRUE));

        // or:
        System.out.println(SealedBoolean.TRUE.or(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.FALSE.or(SealedBoolean.TRUE));
        System.out.println(SealedBoolean.FALSE.or(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.TRUE.or(SealedBoolean.TRUE));

        // not:
        System.out.println(SealedBoolean.TRUE.not());
        System.out.println(SealedBoolean.FALSE.not());

        // conditions as methods:
        SealedBoolean.TRUE
                .ifTrue(() -> System.out.println("SealedTrue is SealedTrue!"))
                .ifFalse(() -> System.out.println("SealedTrue is SealedFalse?"));

        SealedBoolean.FALSE
                .ifFalse(() -> System.out.println("SealedFalse is SealedFalse!"))
                .ifTrue(() -> System.out.println("SealedFalse is SealedTrue?"));

        // conditions as expressions:
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

        // using the switch on Strings (only Java 7+) - super wonky:
        final SealedBoolean bool = SealedBoolean.FALSE;
        switch (bool.toString()) {
            // we cannot do
            // case SealedBoolean.FALSE.toString()
            // because switch statement demands constant conditions :-(
            case "SealedFalse":
                System.out.println("Switch FALSE");
                break;
            case "SealedTrue":
                System.out.println("Switch TRUE");
                break;
        }

        // Is using instanceOf really that bad?
        if (bool instanceof SealedBoolean.True) {
            System.out.println("InstanceOf TRUE");
        } else {
            System.out.println("InstanceOf FALSE");
        }
    }
}
