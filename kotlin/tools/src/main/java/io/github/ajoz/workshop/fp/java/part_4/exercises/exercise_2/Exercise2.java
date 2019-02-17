package io.github.ajoz.workshop.fp.java.part_4.exercises.exercise_2;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/*
  -- Memoization --

  In the previous exercise we modified the `AccidentMetadata` class to use the
  `Supplier<String>` and be fully immutable. Although we made it lazy we made
  it worse then the original, because the file is read on each call to `getData`
  method.

  Was it worth to change the code to a Supplier form?

  In the beginning of the workshop we tried to define what is FP. One of the
  core characteristics was composition. The power of FP as a paradigm lies in
  the possibility of building bigger "things" from smaller "things".

  Although it's possible to do it in standard "OO" or procedural languages, FP
  gives us tools to make it easier.

  We moved to the Supplier implementation to help us express a bigger problem
  with a smaller "thing". Another advantage of this is the standardized "form".
  It's easier to build if we know the shape of our bricks? Lego bricks? :-)

  Back to the problem at hand. We do not want to call `loadAccidents` multiple
  times.

  The FP answer to this problem is memoization. The term was created in 1968.
  Derived from a latin word "memorandum". It is an "optimization" technique used
  to speed up the execution by storing the results of time-consuming functions,
  a memoized function returns the cached result if the same input occurs.

  Memoization is a trade-off, we exchange execution time for the memory consumption.
  As with all "optimization" techniques it should be used with careful
  consideration.
 */

/*
  Part 1:

  Please create two additional methods for the Supplier:
  - default method called `memoized` that returns a supplier that will store the
    value, once it is generated it will always return the "cached" result in
    subsequent calls
  - static method called `memoize` that takes a supplier as an argument and
    returns memoized version of the given supplier.

  Hints:
  - do not worry about the using the supplier between several threads
  - we need to store the value somehow (maybe mutation?)

  Questions:
  - what kind of functions pure or impure can be memoized?
  - why?
 */
@SuppressWarnings("unused")
interface Supplier<A> {
    A get();

    default Supplier<A> memoized() {
        throw new UnsupportedOperationException("Exercise 2 Supplier.memoized is missing!");
    }

    static <A> Supplier<A> memoize(final Supplier<A> supplier) {
        throw new UnsupportedOperationException("Exercise 2 Supplier.memoize is missing!");
    }
}

/*
  Part 2:

  Please modify the `AccidentMetadata` class implementation to use the newly
  created memoization capabilities of the Supplier.

  Hints:
  - are there a lot of changes required?
 */
@SuppressWarnings("unused")
final class AccidentMetadata {
    private final Long id;
    private final File source;
    private final Supplier<String> data = this::loadAccident;

    AccidentMetadata(final Long id,
                     final File source) {
        this.id = id;
        this.source = source;
    }

    private String loadAccident() {
        System.out.println("AccidentMetadata.loadAccident");
        try {
            final Path path = source.toPath();
            final byte[] bytes = Files.readAllBytes(path);
            return new String(bytes);
        } catch (final IOException e) {
            throw new SourceFileCorrupted(
                    String.format("Error when reading: %s", source.getAbsolutePath())
            );
        }
    }

    public Long getId() {
        return id;
    }

    public String getData() {
        return data.get();
    }

    public static class SourceFileCorrupted extends RuntimeException {
        SourceFileCorrupted(final String message) {
            super(message);
        }
    }
}

public class Exercise2 {
    public static void main(final String[] args) {
        final File file = new File("src/main/resources/part_4/accident.info");
        final AccidentMetadata metadata = new AccidentMetadata(42L, file);

        // Let's see the console and check if the accident data is correctly
        // memoized
        String data = "error!";
        for (int i = 0; i < 100; i++) {
            data = metadata.getData();
        }

        System.out.println("accident data = " + data);
    }
}
