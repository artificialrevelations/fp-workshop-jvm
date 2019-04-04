package org.artrev.workshop.fp.part4.practice1;

import org.artrev.workshop.fp.tools.CheckedSupplier;
import org.artrev.workshop.fp.tools.control.Maybe;
import org.artrev.workshop.fp.tools.control.Try;

import java.util.concurrent.atomic.AtomicReference;

/*
  -- Further Enhancing the Supplier --

  We defined a Supplier as:

  interface Supplier<A> {
      A get();
  }

  This is a very nice definition but might be problematic if we would like to
  work with any kind of method that Java has to offer. This means working with
  methods that might throw a checked exception.

  Please check these two methods:

  class Problem {
      public static String bar() {
          return "a very complicated computation happens here";
      }

      public static String foo() throws Exception {
          throw new SomethingHappenedToFoo();
      }
  }

  It is possible to this:

  Supplier<String> supplier1 = Problem::bar;

  but it is not possible to do:

  Supplier<String> supplier2 = Problem:foo;

  Compiler will not be super happy about it, it will give us information that
  there is an error: "Unhandled exception: java.lang.Exception".

  How should we solve this?

  1) Add the throws declaration to the Supplier<A>.get() method

  interface Supplier<A> {
      A get() throws Exception;
  }

  This is problematic, as adding the `throws` declaration will cause that each
  call to `get` will have to be surrounded with a try/catch block. Also we will
  loose the information about the type of the Exception that is happening within
  the supplier.

  2) We could define a new interface:

  interface CheckedSupplier<A> {
      A get() throws Exception;
  }

  Also problematic from the design perspective as we are introducing another
  type of Supplier. The way we declared the CheckedSupplier is also eating up
  the type of the Exception that is happening within the supplier?

  3) We could rework this to include the Exception type in the CheckedSupplier
  signature:

  interface CheckedSupplier<E extends Throwable, A> {
    A get() throws E;
  }

  This would mean that we will have to specify the exception type upfront. This
  new type CheckedSupplier looks familiar to something we worked with already.
  It looks somewhat like Try<A> but in case of Try<A> the exception was hidden
  and not well specified.

  We could use Either<E extends Throwable, A> to model the similar thing as this
  new CheckedSupplier.

  The problem is now that we cannot specify other Exceptions, only subclasses
  of the E should work.

  For the time being we will only implement the simplified CheckedSupplier<A>:

  interface CheckedSupplier<A> {
    A get() throws Exception;
  }
 */
@SuppressWarnings("unused")
interface Supplier<A> {
    A get();

    /*
      Part 1:

      Please implement static method `ofChecked` that can create a `Supplier<A>`
      out of the passed `CheckedSupplier`. This method should produce a Supplier
      that throw a RuntimeException in case the wrapped CheckSupplier throws
      an exception.
     */
    static <A> Supplier<A> ofChecked(final CheckedSupplier<A> supplier) {
        throw new UnsupportedOperationException("Practice 1 Supplier.ofChecked is missing!");
    }

    /*
      Part 2:

      Please implement default method `tryGet` that returns an instance of
      `Try<A>` instead of just `A`. It should return Try.Failure if using `get`
      would cause an exception, it should return the result wrapped inside the
      Try.Success otherwise.
     */
    default Try<A> tryGet() {
        throw new UnsupportedOperationException("Practice 1 Supplier.tryGet is missing!");
    }

    /*
      Part 3:

      Please implement default method `maybeGet` that returns an instance of
      `Maybe<A>` instead of `A`. It should return Maybe.None if using `get`
      would cause an exception, it should return the result wrapped inside the
      Maybe.Some otherwise.
     */
    default Maybe<A> maybeGet() {
        throw new UnsupportedOperationException("Practice 1 Supplier.tryGet is missing!");
    }

    /*
      Part 4:

      Please implement default method `getOrElse` that returns a supplied value
      in case the operation deferred by the Supplier throws an exception.
     */
    default A getOrElse(final A defaultValue) {
        throw new UnsupportedOperationException("Practice 1 Supplier.getOrElse is missing!");
    }

    default Supplier<A> memoized() {
        final AtomicReference<A> value = new AtomicReference<>();
        return () -> {
            synchronized (value) {
                if (value.get() == null) {
                    value.set(get());
                }
                return value.get();
            }
        };
    }
}

class SomethingHappenedToFoo extends Exception {
    SomethingHappenedToFoo() {
        super("Something very very very bad happened!");
    }
}

/*
  Part 5:

  What will happen if an exception is thrown in a memoized supplier?
 */
@SuppressWarnings({"WeakerAccess", "unused"})
public class Practice1 {
    public static String foo() throws Exception {
        throw new SomethingHappenedToFoo();
    }

    public static String foo2() throws SomethingHappenedToFoo {
        throw new SomethingHappenedToFoo();
    }

    private static final CheckedSupplier<String> checkedSupplier = Practice1::foo;
    private static final Supplier<String> supplier = Supplier.ofChecked(Practice1::foo2);

    public static void main(final String[] args) {
        try {
            checkedSupplier.get();
        } catch (Exception e) {
            System.out.println(String.format("Exception thrown: %s", e.getMessage()));
        }

        supplier.maybeGet()
                .ifSome(System.out::println)
                .ifNone(() -> System.out.println("none result :("));

        supplier.tryGet()
                .ifSuccess(System.out::println)
                .ifFailure(System.out::println);

        System.out.println(supplier.getOrElse("Value in case of error!"));
    }
}
