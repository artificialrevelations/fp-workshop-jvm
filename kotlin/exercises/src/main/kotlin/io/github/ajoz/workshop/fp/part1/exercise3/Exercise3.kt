@file:Suppress("UNUSED_PARAMETER", "FunctionName", "unused")

package io.github.ajoz.workshop.fp.part1.exercise3

/*
  -- Function Composition --

  Please test your solutions against available unit tests!

  Part 1:

  Create a function called composeIntFuns for composing two functions
  from Integer to Integer. The result of the composition should be the third
  function.
 */
fun composeIntFuns(first: (Int) -> Int, second: (Int) -> Int): (Int) -> Int =
        TODO("Exercise 3 composeIntFuns is missing!")


/*
  Part 2:

  Create a function called kompose for composing two functions. One from
  type A to type B, second from type B to type C. The result of the composition
  should be a function from type A to type C.

  Function is called kompose because it would be conflicting with the extension
  function of the same name!

  Hint:
  - Allow the types to guide you with the composition

  Questions:
  - Was there any other way to solve this exercise?
 */
fun <A, B, C> kompose(f: (A) -> B, g: (B) -> C): (A) -> C =
        TODO("Exercise 3 kompose is missing!")

/*
  Part 3:

  Create a function called composeAll_1 for composing multiple functions
  passed as an argument. Solve this exercise by using function application.

  Application means that you need to calculate each partial result and apply it
  to the next function on the list.

  Hints:
  - the result of the previous function should be the argument of the next

  Questions:
  - Does it matter in what order we will compose the functions?
  - How should the composeAll_1 behave for one argument?
  - How should the composeAll_1 behave for zero arguments?
 */
fun <A> composeAll_1(vararg functions: (A) -> A): (A) -> A =
        TODO("Exercise 3 composeAll_1 is missing!")

/*
  Part 4:

  Create a function called composeAll_2 for composing multiple functions
  passed as an argument. Solve this exercise by using the function
  compose(Function1, Function1).

  Hints:
  - Can you think of a function that composed with any other function does
    not change the result?
 */
fun <A> composeAll_2(vararg functions: (A) -> A): (A) -> A =
        TODO("Exercise 3 composeAll_2 is missing!")

/*
  Part 5:

  Create an extension function called `andThen` to the Kotlin function type. It
  should compose function on which it is called with the function that is passed
  to it as an argument. The resulting function should first apply the given value
  and then it should apply the result to the function that was passed as the
  `andThen` argument.

  Example:
  val fooify: (String) -> String = { "foo" + it }
  val length: (String) -> Int  = { it.length }

  // composition:
  val fooifiedLength = fooify andThen length
 */
infix fun <A, B, C> ((A) -> B).andThen(after: (B) -> C): (A) -> C =
        TODO("Exercise 3 KFunction1.andThen is missing!")

/*
  Part 6:

  Add a method called `compose` to the `Function1` interface. It should compose
  function of which it is called with the function that is passe to it as an
  argument. The resulting function should first apply the given value to
  the function that was passed as the `compose` argument and then apply the
  result.

  Example:
  val fooify: (String) -> String = { "foo" + it }
  val length: (String) -> Int  = { it.length }

  // composition:
  val fooifiedLength = length compose fooify
 */
infix fun <A, B, C> ((A) -> B).compose(before: (C) -> A): (C) -> B =
        TODO("Exercise 3 KFunction1.compose is missing!")

/*
  Part 7:

  You are working for an online shopping company Nozama, you need to create
  a code that will produce urls for particular products that you have available.

  The Product is a simple class containing id and a name:
 */
data class Product(val id: Id, val description: Description)

data class Id(val value: String)
data class Description(val name: String)

/*
  You already have a few functions available:

  - getProductId that takes a Product and returns its Id
  - getPurchaseUri that takes an Id and returns a String uri
  - getSecureUrl that takes an uri and returns a secure url that is using https
 */

val getProductId: (Product) -> Id = { it.id }

val getPurchaseUri: (Id) -> String = { "nozama.com/shop/purchase?=${it.value}" }

val getSecureUrl: (String) -> String = { "https://$it" }

/*
  Compose those three functions together to get a third function:
 */
val getSecureProductPurchaseUrl: (Product) -> String =
        { TODO("Exercise 3 getSecureProductPurchaseUrl is missing!") }

fun main(args: Array<String>) {
    // You can play here with the code:

    val products =
            listOf(
                    Product(
                            Id("JUGLDZ42JAVA"),
                            Description("Workshop: FP in Java")
                    ),
                    Product(
                            Id("JUGLDZ42KOTLIN"),
                            Description("Workshop: FP in Kotlin")
                    )
            )

    /*
      As you can clearly see, this approach gives us a very high reusability
      as we can mix and match our "lego" pieces to create new business logic.
     */
    for (product in products) {
        println(getSecureProductPurchaseUrl(product))
    }
}