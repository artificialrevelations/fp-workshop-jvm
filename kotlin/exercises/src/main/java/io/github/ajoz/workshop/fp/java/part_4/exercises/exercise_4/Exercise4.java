package io.github.ajoz.workshop.fp.java.part_4.exercises.exercise_4;

import io.github.ajoz.workshop.fp.java.tools.Consumer1;
import io.github.ajoz.workshop.fp.java.tools.Effect;
import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

/*
  -- Even more laziness --

  Currently the `Supplier` although lazy and memoized is not super useful. We
  are still lacking tools to work with it. The ultimate goal is to create a
  type which can be easily composed.

  Let's start with our old friends: `map` and `flatMap` so far we created them
  for a `List<A>`, a `Maybe<A>`, a `Try<A>` and `Either<A, B>`. If we would
  squint our eyes a little single element `List<A>` doesn't differ much from
  `Maybe<A>`, so does a `Supplier<A>`.

  We need to remember that `Supplier<A>` is a `() -> A`. So if we would like to
  "map" it with a function `A -> B` we need to transform it to a `() -> B`.
 */
@SuppressWarnings("unused")
interface Supplier<A> {
    A get();

    /*
      Part 1:

      Please add `map` method to the supplier interface.
     */
    default <B> Supplier<B> map(final Function1<A, B> function) {
        throw new UnsupportedOperationException("Exercise 4 Supplier.map is missing!");
    }

    /*
      Part 2:

      Please add `flatMap` method to the supplier interface. It is simpler then
      you think.

      Hints:
      - what do we have to do to not have a Supplier<Supplier<B>>?
      - almost like `map` + this one flatten
     */
    default <B> Supplier<B> flatMap(final Function1<A, Supplier<B>> function) {
        throw new UnsupportedOperationException("Exercise 4 Supplier.flatMap is missing!");
    }

    /*
      Part 3:

      Please add `before` method to the supplier interface. It should take an
      effect as an argument and perform it before returning the value.
     */
    default Supplier<A> before(final Effect effect) {
        throw new UnsupportedOperationException("Exercise 4 Supplier.before is missing!");
    }

    /*
      Part 4:

      Please add `after` method to the supplier interface. It should take an
      effect as an argument and perform it before returning the value.
     */
    default Supplier<A> after(final Consumer1<A> effect) {
        throw new UnsupportedOperationException("Exercise 4 Supplier.after is missing!");
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

    static <A> Supplier<A> memoize(final Supplier<A> supplier) {
        return supplier.memoized();
    }
}

/*
  Part 5:

  After talking with other teams working on the Accident user stories, you and
  your team decide to rework the `AccidentMetadata` API so it's much safer for
  the user.

  Instead of returning a bare String as data, you painstakingly designed a better
  type called `Accident`. It will keep all the parsed information. Users of your
  glorious `AccidentMetadata` class won't ever know how the underlying information
  is structured.

  Some day maybe, just maybe you will move to a more human readable majestic
  format like XML instead of this plain and stupid CSV.

  Please modify the `AccidentMetadata` so it's possible to:
  - print to logs the ID of the accident before the loadAccident is performed
  - split the line from the file over the commas
  - transform the resulting string array into Accident object
  - print to logs the result
  - remember that the result should be memoized

 */
@SuppressWarnings("unused")
final class Accident {
    private final Double latitude;
    private final Double longitude;
    private final String message;

    Accident(final Double latitude,
             final Double longitude,
             final String message) {
        this.latitude = latitude;
        this.longitude = longitude;
        this.message = message;
    }

    @Override
    public boolean equals(final Object other) {
        if (this == other)
            return true;
        if (other == null || getClass() != other.getClass())
            return false;
        final Accident accident = (Accident) other;
        return Objects.equals(latitude, accident.latitude) &&
                Objects.equals(longitude, accident.longitude) &&
                Objects.equals(message, accident.message);
    }

    @Override
    public int hashCode() {
        return Objects.hash(latitude, longitude, message);
    }

    @Override
    public String toString() {
        return "Accident{" +
                "latitude=" + latitude +
                ", longitude=" + longitude +
                ", message='" + message + '\'' +
                '}';
    }
}

@SuppressWarnings("unused")
final class AccidentMetadata {
    private final Long id;
    private final File source;
    private final Supplier<String> data = Supplier.memoize(this::loadAccident);

    AccidentMetadata(final Long id,
                     final File source) {
        this.id = id;
        this.source = source;
    }

    private String loadAccident() {
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

public class Exercise4 {
    public static void main(final String[] args) {
        // Part 1:
        final Supplier<String> s1 = () -> "JUG Lodz";
        final Supplier<Integer> s1map = s1.map(String::length);
        System.out.println("s1map  = " + s1map.get());

        // Part 2:
        final Supplier<String> s2 = () -> "Checkout our meetup";
        final Supplier<Integer> s2flatMap = s2.flatMap(string -> string::length);
        System.out.println("s2flatMap  = " + s2flatMap.get());

        // Part 3:
        final Supplier<String> s3 = () -> "and facebook page";
        s3.before(() -> System.out.println("Happens before!"));
        System.out.println("s2flatMap  = " + s3.get());

        // Part 4:
        final Supplier<String> s4 = () -> "We are organizing Mobilization conference!";
        s4.after((string) -> System.out.println("Happens after: " + string));
        System.out.println("s2flatMap  = " + s4.get());

        /*
        Part 5:

        final File file = new File("src/main/resources/part_4/accident.info");
        final AccidentMetadata metadata = new AccidentMetadata(42L, file);

        // Let's see the console and check if the accident data is correctly
        // memoized
        Accident data = null;
        for (int i = 0; i < 100; i++) {
            data = metadata.getData();
        }

        System.out.println("accident data = " + data);
        */
    }
}
