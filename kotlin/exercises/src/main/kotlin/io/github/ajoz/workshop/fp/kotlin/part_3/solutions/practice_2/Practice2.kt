@file:Suppress("PackageName", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_3.solutions.practice_2

import io.github.ajoz.workshop.fp.kotlin.part_3.solutions.practice_2.BinaryTree.Branch
import io.github.ajoz.workshop.fp.kotlin.part_3.solutions.practice_2.BinaryTree.Leaf

internal sealed class BinaryTree<A> {
    class Leaf<A>(private val value: A) : BinaryTree<A>() {
        override fun toString() = "Leaf($value)"
    }

    class Branch<A>(private val left: BinaryTree<A>,
                    private val right: BinaryTree<A>) : BinaryTree<A>() {
        override fun toString() = "Branch(\n\t$left, \n\t$right\n)"
    }
}

fun main(args: Array<String>) {
    val simpleTree: BinaryTree<Int> = Branch(Leaf(1), Leaf(2))
    println("simpleTree = $simpleTree")

    val complexTree: BinaryTree<Int> =
            Branch(
                    Branch(
                            Leaf(1),
                            Leaf(2)
                    ),
                    Branch(
                            Leaf(3),
                            Leaf(4)
                    )
            )
    println("complexTree = $complexTree")
}