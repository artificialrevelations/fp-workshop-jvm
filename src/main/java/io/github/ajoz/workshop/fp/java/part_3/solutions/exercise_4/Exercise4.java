package io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_4;

@SuppressWarnings({"WeakerAccess", "SameParameterValue"})
public class Exercise4 {

    static Integer div1(final Integer a, final Integer b) {
        return a / b;
    }

    static class DivideByZero extends Exception {
    }

    static Integer div2(final Integer a, final Integer b) throws DivideByZero {
        if (b == 0)
            throw new DivideByZero();

        return a / b;
    }

    static Integer div3(final Integer a, Integer b) {
        return b != 0 ? a / b : null;
    }

    @SuppressWarnings("WeakerAccess")
    static class Result {
        public final Integer value;
        public final boolean exists;

        public Result(Integer value, boolean exists) {
            this.value = value;
            this.exists = exists;
        }
    }

    static Result div4(final Integer a, final Integer b) {
        return b != 0
                ? new Result(a / b, true)
                : new Result(null, false);
    }

    static Maybe<Integer> safeDiv(final Integer a, final Integer b) {
        return b != 0
                ? new Maybe.Just<>(a / b)
                : new Maybe.Nothing<>();
    }


    public static void main(final String[] args) {
        // Part 1:
        System.out.println(div1(42, 0));

        // Part 2:
        try {
            final Integer res2 = div2(42, 0);
            System.out.println(res2);
        } catch (DivideByZero divideByZero) {
            divideByZero.printStackTrace();
        }

        // Part 3:
        final Integer res3 = div3(42, 0);
        if (null != res3) {
            System.out.println("Div3 result: " + res3);
        } else {
            System.out.println("Error handling after div3 failed!");
        }

        // Part 4:
        final Result res4 = div4(42, 0);
        if (res4.exists) {
            System.out.println("Div4 result: " + res4);
        } else {
            System.out.println("Error handling after div4 failed!");
        }

        // Part 5:
        final Maybe<Integer> safeRes = safeDiv(24, 0);
        if (safeRes instanceof Maybe.Just) {
            System.out.println("SafeDiv result: " + ((Maybe.Just) safeRes).value);
        } else {
            System.out.println("Error handling after safeDiv failed!");
        }
    }
}

// Part of Part 5 because Java is like that :-(
@SuppressWarnings("unused")
abstract class Maybe<A> {
    public static class Just<A> extends Maybe<A> {
        public final A value;

        public Just(final A value) {
            this.value = value;
        }
    }

    public static class Nothing<A> extends Maybe<A> {
    }

    private Maybe() {
    }
}