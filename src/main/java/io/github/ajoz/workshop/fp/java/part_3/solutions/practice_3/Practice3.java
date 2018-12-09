package io.github.ajoz.workshop.fp.java.part_3.solutions.practice_3;

import io.github.ajoz.workshop.fp.java.part_3.solutions.practice_3.Expr2.Operator;
import io.github.ajoz.workshop.fp.java.part_3.solutions.practice_3.Expr2.Value;
import io.github.ajoz.workshop.fp.java.tools.Function2;

abstract class Expr2<A> {
    public static final class Value<A> extends Expr2<A> {
        private final A value;

        Value(final A value) {
            this.value = value;
        }

        @Override
        public A evaluate() {
            return value;
        }
    }

    public static final class Operator<A> extends Expr2<A> {
        private final Expr2<A> left;
        private final Expr2<A> right;
        private final Function2<A, A, A> operator;

        Operator(final Expr2<A> left,
                 final Expr2<A> right,
                 final Function2<A, A, A> operator) {
            this.left = left;
            this.right = right;
            this.operator = operator;
        }

        @Override
        public A evaluate() {
            return operator.apply(left.evaluate(), right.evaluate());
        }
    }

    public abstract A evaluate();

    private Expr2() {
    }
}

public class Practice3 {
    public static Expr2<Integer> value(final Integer value) {
        return new Value<>(value);
    }

    public static Expr2<Integer> add(final Expr2<Integer> left,
                                     final Expr2<Integer> right) {
        return new Operator<>(left, right, (a, b) -> a + b);
    }

    public static void main(final String[] args) {
        System.out.println(add(add(value(1), value(2)), value(10)).evaluate());
    }
}
