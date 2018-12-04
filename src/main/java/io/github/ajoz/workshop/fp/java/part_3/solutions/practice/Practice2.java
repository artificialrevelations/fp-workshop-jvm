package io.github.ajoz.workshop.fp.java.part_3.solutions.practice;

import static io.github.ajoz.workshop.fp.java.part_3.solutions.practice.Expr.*;

abstract class Expr {
    private Expr() {
    }

    public abstract Integer eval();

    public static Expr value(final Integer value) {
        return new Val(value);
    }

    public static Expr add(final Expr left, final Expr right) {
        return new Add(left, right);
    }

    public static Expr multiply(final Expr left, final Expr right) {
        return new Mul(left, right);
    }

    public static Expr substract(final Expr left, final Expr right) {
        return new Sub(left, right);
    }

    public static Expr divide(final Expr left, final Expr right) {
        return new Div(left, right);
    }

    private static final class Val extends Expr {
        private final Integer value;

        private Val(final Integer value) {
            this.value = value;
        }

        @Override
        public Integer eval() {
            return value;
        }
    }

    private static final class Add extends Expr {
        private final Expr left;
        private final Expr right;

        private Add(final Expr left,
                    final Expr right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public Integer eval() {
            return left.eval() + right.eval();
        }
    }

    private static final class Sub extends Expr {
        private final Expr left;
        private final Expr right;

        private Sub(Expr left, Expr right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public Integer eval() {
            return left.eval() - right.eval();
        }
    }

    private static final class Mul extends Expr {
        private final Expr left;
        private final Expr right;

        private Mul(Expr left, Expr right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public Integer eval() {
            return left.eval() * right.eval();
        }
    }

    private static final class Div extends Expr {
        private final Expr left;
        private final Expr right;

        private Div(Expr left, Expr right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public Integer eval() {
            return left.eval() / right.eval();
        }
    }
}

public class Practice2 {
    public static void main(String[] args) {
        System.out.println(add(value(1), value(2)).eval());
        System.out.println(multiply(add(value(1), value(2)), substract(value(42), value(2))).eval());
    }
}
