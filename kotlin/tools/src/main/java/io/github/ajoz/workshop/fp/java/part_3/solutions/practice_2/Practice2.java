package io.github.ajoz.workshop.fp.java.part_3.solutions.practice_2;

import static io.github.ajoz.workshop.fp.java.part_3.solutions.practice_2.BinaryTree.branch;
import static io.github.ajoz.workshop.fp.java.part_3.solutions.practice_2.BinaryTree.leaf;

abstract class BinaryTree<A> {
    public static class Leaf<A> extends BinaryTree<A> {
        private final A value;

        Leaf(final A value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return String.format("Leaf(%s)", value);
        }
    }

    public static class Branch<A> extends BinaryTree<A> {
        private final BinaryTree<A> left;
        private final BinaryTree<A> right;

        Branch(final BinaryTree<A> left,
               final BinaryTree<A> right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public String toString() {
            return String.format("Branch(%s, %s)", left, right);
        }
    }

    static <A> BinaryTree<A> leaf(final A value) {
        return new Leaf<>(value);
    }

    static <A> BinaryTree<A> branch(final BinaryTree<A> left,
                                    final BinaryTree<A> right) {
        return new Branch<>(left, right);
    }

    private BinaryTree() {
    }
}

public class Practice2 {
    public static void main(String[] args) {
        final BinaryTree<Integer> simpleTree = branch(leaf(1), leaf(2));
        System.out.println("simpleTree = " + simpleTree);

        final BinaryTree<Integer> complexTree =
                branch(
                        branch(
                                leaf(1),
                                leaf(2)
                        ),
                        branch(
                                leaf(3),
                                leaf(4)
                        )
                );
        System.out.println("complexTree = " + complexTree);
    }
}
