package io.github.ajoz.workshop.fp.java.part_3.solutions.practice_1;

import io.github.ajoz.workshop.fp.java.tools.CheckedSupplier;
import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.util.NoSuchElementException;
import java.util.Random;

@SuppressWarnings({"unused", "WeakerAccess"})
abstract class Try<A> {
    private static final class Success<A> extends Try<A> {
        private final A value;

        private Success(final A value) {
            this.value = value;
        }

        @Override
        public A get() {
            return value;
        }

        @Override
        public A getOrElse(final A defaultValue) {
            return value;
        }

        @Override
        public <B> Try<B> map(final Function1<A, B> mapper) {
            return Try.success(mapper.apply(value));
        }

        @Override
        public <B> Try<B> flatMap(final Function1<A, Try<B>> mapper) {
            return mapper.apply(value);
        }

        @Override
        public Try<A> recover(final Function1<Throwable, A> fun) {
            return Try.success(value);
        }

        @Override
        public Try<A> recoverWith(final Function1<Throwable, Try<A>> fun) {
            return Try.success(value);
        }
    }

    private static final class Failure<A> extends Try<A> {
        private final Throwable error;

        private Failure(final Throwable error) {
            this.error = error;
        }

        @Override
        public A get() {
            throw new NoSuchElementException("Failure has no value!");
        }

        @Override
        public A getOrElse(final A defaultValue) {
            return defaultValue;
        }

        @Override
        public <B> Try<B> map(final Function1<A, B> mapper) {
            return Try.failure(error);
        }

        @Override
        public <B> Try<B> flatMap(final Function1<A, Try<B>> mapper) {
            return Try.failure(error);
        }

        @Override
        public Try<A> recover(final Function1<Throwable, A> fun) {
            return Try.success(fun.apply(error));
        }

        @Override
        public Try<A> recoverWith(final Function1<Throwable, Try<A>> fun) {
            return fun.apply(error);
        }
    }

    public abstract A get();

    public abstract A getOrElse(final A defaultValue);

    public abstract <B> Try<B> map(final Function1<A, B> mapper);

    public abstract <B> Try<B> flatMap(final Function1<A, Try<B>> mapper);

    public abstract Try<A> recover(final Function1<Throwable, A> fun);

    public abstract Try<A> recoverWith(final Function1<Throwable, Try<A>> fun);

    public static <A> Try<A> success(final A value) {
        return new Success<>(value);
    }

    public static <A> Try<A> failure(final Throwable error) {
        return new Failure<>(error);
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

@SuppressWarnings("WeakerAccess")
public class Practice1 {
    public static void tryLogging(final DeviceAPI api) {
        final String message =
                Try.ofNullable(api)
                        .flatMap(DeviceAPI::getDeviceInfo)
                        .flatMap(DeviceInfo::getHardwareInfo)
                        .flatMap(HardwareInfo::getArchitecture)
                        .map(Architecture::name)
                        .recover(Throwable::getMessage)
                        .get();
        System.out.println(message);
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
