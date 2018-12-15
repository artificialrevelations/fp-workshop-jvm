package io.github.ajoz.workshop.fp.java.part_4.exercises.practice_1;

import io.github.ajoz.workshop.fp.java.tools.CheckedSupplier;
import io.github.ajoz.workshop.fp.java.tools.Maybe;
import io.github.ajoz.workshop.fp.java.tools.Try;

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
interface Supplier<A> {
    A get();

    /*
      Part 1:

      Please implement static method `ofChecked` that can create a `Supplier<A>`
      out of the passed 
     */
    static <A> Supplier<A> ofChecked(final CheckedSupplier<A> supplier) {
        return () -> {
            try {
                return supplier.get();
            } catch (final Exception e) {
                throw new RuntimeException(e);
            }
        };
    }

    default Try<A> tryGet() {
        try {
            return Try.success(get());
        } catch (final Exception e) {
            return Try.failure(e);
        }
    }

    default Maybe<A> maybeGet() {
        try {
            return Maybe.some(get());
        } catch (final Exception e) {
            return Maybe.none();
        }
    }
}

class SomethingHappenedToFoo extends Exception {
}

@SuppressWarnings({"WeakerAccess", "unused"})
public class Practice1 {
    public static String foo() throws Exception {
        throw new SomethingHappenedToFoo();
    }

    public static String foo2() throws SomethingHappenedToFoo {
        throw new SomethingHappenedToFoo();
    }

    private final CheckedSupplier<String> cs = Practice1::foo;
    private final Supplier<String> s = Supplier.ofChecked(Practice1::foo2);
}
