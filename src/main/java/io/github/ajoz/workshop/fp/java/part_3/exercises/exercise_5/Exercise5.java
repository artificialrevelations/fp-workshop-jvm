package io.github.ajoz.workshop.fp.java.part_3.exercises.exercise_5;

import java.util.NoSuchElementException;

/*
  -- Working effectively with optionality --

  In the previous exercise we defined a type called Maybe. We used it to express
  partial-nes of a function. Let's examine first a bit enhanced version of the
  Maybe type:
  - `isPresent` method returns true if Maybe is Just and false otherwise
  - `get` method returns value stored in Just and throws exception in Nothing
  - two static methods for creating the types
 */
@SuppressWarnings("unused")
abstract class Maybe<A> {
    private static final class Nothing<A> extends Maybe<A> {
        @Override
        public A get() {
            throw new NoSuchElementException("Cannot get value from Nothing!");
        }

        @Override
        public boolean isPresent() {
            return false;
        }
    }

    private static final class Just<A> extends Maybe<A> {
        private final A value;

        private Just(final A value) {
            this.value = value;
        }

        @Override
        public A get() {
            return value;
        }

        @Override
        public boolean isPresent() {
            return true;
        }
    }

    public abstract A get();

    public abstract boolean isPresent();

    public static <A> Maybe<A> just(final A value) {
        return new Just<>(value);
    }

    public static <A> Nothing<A> nothing() {
        return new Nothing<>();
    }
}

/*
  You are working on a API for some device SDK. Currently you are working on
  function that will log information about the Architecture of the device's
  underlying hardware. Fortunately the SDK API allows to retrieve the
  information, there is just one catch. The SDK supports multiple vendors, it
  is possible that some information is not available :-(

  The SDK is built around several parts.
  - DeviceAPI which is the entry point for anything related with the SDK
  - DeviceInfo which holds the information about the underlying hardware
  - HardwareInfo which holds the information about the architecture

  Depending on the vendor anything can be null (everything except the DeviceAPI).

  Look at the classes to get familiar with the SDK design:
 */
@SuppressWarnings("unused")
class DeviceAPI {
    // this might return a null :-(
    DeviceInfo getDeviceInfo() {
        return new DeviceInfo();
    }
}

@SuppressWarnings("unused")
class DeviceInfo {
    // this might return a null :-(
    HardwareInfo getHardwareInfo() {
        return new HardwareInfo();
    }
}

@SuppressWarnings("unused")
class HardwareInfo {
    // this might return a null :-(
    Architecture getArchitecture() {
        return Architecture.VLIW;
    }
}

@SuppressWarnings("unused")
enum Architecture {
    VLIW,
    CISC,
    RISC,
    MISC,
    ZISC,
    EPIC
}

@SuppressWarnings({"WeakerAccess", "unused"})
public class Exercise5 {
    /*
      Part 1:

      Please create function `logging1` that takes a DeviceAPI as an argument
      and logs to console the architecture of the device. Please remember that
      everything except the passed DeviceAPI can be null.

      Questions:
      - is the code nice? do you like it?
      - does it look like an arrow?
     */
    static void logging1(final DeviceAPI api) {
        throw new UnsupportedOperationException("Execise 5 logging1 is missing!");
    }

    /*
      Part 2:

      Please modify the SDK classes:
      - to DeviceAPI add a `safeGetDeviceInfo` that returns a Maybe<DeviceInfo>
      - to DeviceInfo add a `safeGetHardwareInfo` that returns a Maybe<HardwareInfo>
      - to HardwareInfo add a `safeGetArchitecture` that returns a Maybe<Architecture>

      Please create a function `logging2` does the same thing as `logging1` but
      this time it uses "safe" versions of the methods.

      Hints:
      - use Maybe.just() method to create Maybe instances for the exercise

      Questions:
      - does it look better or worse then the null version?
     */
    static void logging2(final DeviceAPI api) {
        throw new UnsupportedOperationException("Execise 5 logging2 is missing!");
    }

    /*
      Part 3:

      Please look closely at the part 2 there is an operation that is happening
      several times here:

      1. We return a Maybe<A> from some method
      2. We check if the value is present
      3. We take out the value
      4. We transform the value into another thing

      If we squint our eyes a bit we could say that Maybe<A> looks like a List<A>
      that can be empty or have just one element. We already created a function
      that works similarly to what we described above for a function.

      It was called `map`. Let's think how the `map` worked:
      - for empty List it returned an empty List
      - for single element List it took the value, applied the function to it and
        returned a new single element List with the result of the function.

      We could implement the same in Maybe<A>, there is a problem though. The
      function `map` was designed to work with total functions:

      List<B> map(List<A> list, Function1<A, B> function)

      Maybe<B> map(Maybe<A> maybe, Function1<A, B> function)

      But we want to work with partial functions:

      Maybe<B> differentMap(Maybe<A>, Function1<A, Maybe<B>>)

      If we would use the `map` with a partial function we would get
      Maybe<Maybe<B>> as a result, would be nice to somehow "flatten" the
      Maybe's into a single one.

      Maybe<B> flatMap(Maybe<A>, Function1<A, Maybe<B>>)

      Please modify the Maybe type:
      - add `map` method, for Just it should use the passed function to calculate
        the result and wrap it inside another Just, for Nothing it should return
        Nothing
      - add `flatMap` method, for Just it should return the result of the passed
        function, for Nothing it should return Nothing
      - add `getOrElse` method that should take a value as an argument, for Just
        it should return the value stored in Just, for Nothing it should return
        value passed as an argument.

      Please create function `logging3` that is doing the same as previous
      two functions `logging1` and `logging2` but use the newly created `map`,
      `flatMap` and `getOrElse` methods.

      Can the message string be final?

      Hints:
      - you can chain the calls to `flatMap` and `getOrElse` to get a single
        expression
     */
    static void logging3(final DeviceAPI api) {
        throw new UnsupportedOperationException("Execise 5 logging3 is missing!");
    }

    public static void main(final String[] args) {
        // Part 1:
        logging1(new DeviceAPI());

        // Part 2:
        logging2(new DeviceAPI());

        // Part 3:
        logging3(new DeviceAPI());
    }
}
