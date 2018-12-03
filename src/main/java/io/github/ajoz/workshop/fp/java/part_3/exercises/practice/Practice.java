package io.github.ajoz.workshop.fp.java.part_3.exercises.practice;

import io.github.ajoz.workshop.fp.java.tools.CheckedSupplier;

import java.util.NoSuchElementException;
import java.util.Random;

/*
  -- Working effectively with failure --

  The sum type Maybe<A> we introduced in the exercises was very nice but it has
  a one major flaw. Although it allows indicating that there was an error, it
  eats the reason. There is no real way of communicating why the error occurred.

  Lets create a better type that will allow us to do that:

  data Try a = Success a | Failure Throwable

  A Try<A> is a sum type just like Maybe<A> but is capable of storing information
  about the exception (error situation) that caused the computation to fail.

  In this exercise create the type Try<A> with methods:
  - `map`
  - `flatMap`
  - `get`
  - `getOrElse`

  New methods:
  - `recoverWith` that takes a Function1<Throwable, Try<A>> and allows recovering
    from a failed computation. Think of it like "flatMap" but for Failure.
  - `recover` that takes a Function1<Throwable, A> and allows recovering from a
    failed computation. Think about it like a "map" but for Failure.
  - static `success` that takes a value and returns a Try.Success of this value
  - static `failure` that takes a Throwable and returns a Try.Failure of this
    error

    We will do some practice with the Device SDK example we worked on in Exercise 5.

    Below is the Try type that needs implementation:
 */
@SuppressWarnings({"unused", "WeakerAccess"})
abstract class Try<A> {

    public static <A> Try<A> success(final A value) {
        throw new UnsupportedOperationException("Practice Try.success is missing!");
    }

    public static <A> Try<A> failure(final Throwable error) {
        throw new UnsupportedOperationException("Practice Try.failure is missing!");
    }

    public static <A> Try<A> ofChecked(final CheckedSupplier<A> supplier) {
        try {
            return Try.success(supplier.get());
        } catch (final Throwable error) {
            return Try.failure(error);
        }
    }

    public static <A> Try<A> ofNullable(final A value) {
        if (null != value) {
            return Try.success(value);
        }
        return Try.failure(new IllegalArgumentException("Null value passed to ofNullable!"));
    }
}

/*
  In this version all the Device SDK classes return Try<A>. A simple logic was
  added to simulate a possibility of an exception happening while the code is
  run.

  Please check the implementation of the classes below and main method in the
  Practice class.
 */
class DeviceAPI {
    private final Random random = new Random();
    private final DeviceInfo deviceInfo = new DeviceInfo();

    Try<DeviceInfo> getDeviceInfo() {
        if (random.nextBoolean()) {
            return Try.success(deviceInfo);
        }
        return Try.failure(new NoSuchElementException("DeviceInfo does not exist!"));
    }
}

@SuppressWarnings("unused")
class DeviceInfo {
    private final Random random = new Random();
    private final HardwareInfo hardwareInfo = new HardwareInfo();

    Try<HardwareInfo> getHardwareInfo() {
        if (random.nextBoolean()) {
            return Try.success(hardwareInfo);
        }
        return Try.failure(new IllegalStateException("HardwareInfo unavailable!"));
    }
}

@SuppressWarnings("unused")
class HardwareInfo {
    private final Random random = new Random();
    private final Architecture architecture = Architecture.VLIW;

    Try<Architecture> getArchitecture() {
        if (random.nextBoolean()) {
            return Try.success(architecture);
        }
        return Try.failure(new RuntimeException("Architecture not specified!"));
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

/*
  Please create function `tryLogging`:
  - DeviceAPI can be null
  - Print the current architecture
  - If the architecture is unavailable (missing DeviceInfo, missing HardwareInfo,
    missing Architecture) print the error message instead

  Hints:
  - What should be the order of operations?
  - When should the error recovery happen?
 */
@SuppressWarnings({"WeakerAccess", "unused"})
public class Practice {
    public static void tryLogging(final DeviceAPI api) {
        throw new UnsupportedOperationException("Practice tryLogging is missing!");
    }

    public static void main(final String[] args) {
        // with null:
        tryLogging(null);

        // without null:
        final DeviceAPI api = new DeviceAPI();
        for (int i = 0; i < 100; i++) {
            tryLogging(api);
        }
    }
}
