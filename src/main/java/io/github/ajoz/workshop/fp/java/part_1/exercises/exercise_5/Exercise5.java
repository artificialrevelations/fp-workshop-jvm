package io.github.ajoz.workshop.fp.java.part_1.exercises.exercise_5;

/*
  -- Partial Application --

  From one point of view two argument functions are problematic because we always
  need to supply it with both arguments. Like you can imagine the problem escalates
  quickly with functions of larger arity.

  We already explored few ways of changing functions with arities greater then
  one to a one argument function.

  For the task at hand the most useful would be the curried form of a two or more
  argument function. Function of the form:

  Function1<A, Function1<B, C>>

  Can be partially applied. It means that the first argument can be supplied
  without passing the other one. As a result of passing the argument A we would
  get the result of Function1<B, C>

  Example:

  For example let's say you have a certain class that is responsible for loading
  some data let's call it a Loader, this class accepts instances of Foo and returns
  instances of Bar.

  interface Loader {
      // some class
      public Bar loadInfo(Foo foo) {
          // some heavy business logic
      }
  }

  Now somewhere in the code you are processing a List<Foo> and you need a List<Bar>
  That code expects a Function1<Foo, Bar>:

  public void doSomeMightyProcessing(final Function1<Foo, Bar> function) {

  We would like to use our Loader class and pass a correct Function1<Foo, Bar>
  to the `doSomeMightyProcessing`.

  Let's create a two argument curried Function:

  final Function1<Loader, Function1<Foo, Bar>> loadBarFromFoo =
        loader -> foo -> loader.loadInfo(foo);

  Now we can partially apply the function with a correct Loader instance

  final Function1<Foo, Bar> barToFoo = loadBarFromFoo.apply(someParticularLoader);
  doSomeMightyProcessing(barToFoo);
 */
public class Exercise5 {

}
