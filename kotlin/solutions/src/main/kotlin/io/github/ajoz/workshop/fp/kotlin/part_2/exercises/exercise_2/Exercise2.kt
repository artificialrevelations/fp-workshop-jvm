@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_2.exercises.exercise_2


/*
  -- Advanced functions --

  Generalizing the generalization of `sum` and `product` (pun intended).
 */

/*
  Part 1:

  This function `foo` that we created in the previous exercise
  can now be generalized to work on a List of type A and return
  a type B
 */
fun foo(list: List<Int>,
        initial: Int,
        operator: (Int, Int) -> Int): Int {
    var accumulator = initial
    for (element in list) {
        accumulator = operator(accumulator, element)
    }

    return accumulator
}

/*
  Part 2:

  Create a function `bar` that can work on a List<A> and return
  a result of type B
*/
/*
// hint:
// - what will be the type of initial value?
// - what will be the types of arguments for the operator?
//
// Question:
// - how do you think does the order of arguments in the operator
//   has any meaning?
fun <A, B> bar(list: List<A>, ...): B {

}
*/

/*
  Part 3:

  Implement functions `sum` and `product` with the use of
  the newly created function `bar`
*/

/*
fun sum(list: List<Int>): Int {
    return bar(list, ...);
}

fun product(list: List<Int>): Int {
    return bar(list, ...);
}
 */
