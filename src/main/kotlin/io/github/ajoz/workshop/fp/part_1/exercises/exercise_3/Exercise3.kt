package io.github.ajoz.workshop.fp.part_1.exercises.exercise_3

/*
  -- Function Composition --

  Please test your solutions against available unit tests!

  part 1:

  Create a function/method called composeIntFuns for composing two functions
  from Integer to Integer. The result of the composition should be the third
  function.

  part 2:

  Create a function/method called compose for composing two functions. One from
  type A to type B, second from type B to type C. The result of the composition
  should be a function from type A to type C.

  part 3:

  Create a function/method called composeAll for composing multiple functions
  passed as an argument. Try to solve this part in several ways:
  - use the compose(Function1, Function1)
  - use function application

  Question: Does it matter in what order we will compose the functions?
  Question: How should the composeAll behave for one argument?
  Question: How should the composeAll behave for zero arguments?

  part 4:

  Enhance Function1 interface with two methods for composition.
  - andThen that first applies the function and then applies the one given as the argument
  - compose that first applies the function give as the argument and then applies the function
*/

fun kComposeIntFuns(first: (Int) -> Int, second: (Int) -> Int): (Int) -> Int {
    TODO("Exercise3 kComposeIntFuns is missing!")
}

fun <A, B, C> kCompose(f: (A) -> B, g: (B) -> C): (A) -> C {
    TODO("Exercise3 kCompose is missing!")
}

fun <A> kComposeAll(vararg functions: (A) -> A): (A) -> A {
    TODO("Exercise3 kComposeAll is missing!")
}

infix fun <A, B, C> ((A) -> B).andThen(after: (B) -> C): (A) -> C {
    TODO("Exercise3 KFunction1.andThen is missing!")
}

infix fun <A, B, C> ((A) -> B).compose(before: (C) -> A): (C) -> B {
    TODO("Exercise3 KFunction1.compose is missing!")
}