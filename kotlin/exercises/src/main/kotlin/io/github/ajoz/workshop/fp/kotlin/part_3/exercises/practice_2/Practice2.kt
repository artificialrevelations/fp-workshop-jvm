@file:Suppress("PackageName", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_3.exercises.practice_2

/*
  -- Other curious recursive types --

  Recursive types are a very peculiar thing. We learned it when creating a single
  linked List. As you might imagine recursive types do not end on a List, we have
  a plethora of academic ;-) data structures that we can use for training.

  Please create a full binary tree:
  - each node has at most two children, which are referred to as the left child
    and the right child.
  - every node has either 0 or 2 children.

  Implement a `toString` method and use it to print the tree structure. Try to
  use your implementation with few example trees:
  - simple tree with a single branch with two leaves containing some numbers like
    1 and 2
  - complex tree:
    branch [
             branch [
                     left leaf 1,
                     right leaf 2
             ],
             branch [
                     left leaf 3,
                     right leaf 4
             ]
    ]
 */
internal sealed class BinaryTree<A>// Implement the tree here! :-)

fun main(args: Array<String>) {
    // Print tree structure here for the two trees
}