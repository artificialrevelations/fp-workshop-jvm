package io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_5;

import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.util.NoSuchElementException;

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

        @Override
        public <B> Maybe<B> map(final Function1<A, B> mapper) {
            return Maybe.nothing();
        }

        @Override
        public <B> Maybe<B> flatMap(final Function1<A, Maybe<B>> mapper) {
            return Maybe.nothing();
        }

        @Override
        public A getOrElse(final A defaultValue) {
            return defaultValue;
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

        @Override
        public <B> Maybe<B> map(final Function1<A, B> mapper) {
            return Maybe.just(mapper.apply(value));
        }

        @Override
        public <B> Maybe<B> flatMap(Function1<A, Maybe<B>> mapper) {
            return mapper.apply(value);
        }

        @Override
        public A getOrElse(A defaultValue) {
            return value;
        }
    }

    public abstract A get();

    public abstract boolean isPresent();

    public abstract <B> Maybe<B> map(final Function1<A, B> mapper);

    public abstract <B> Maybe<B> flatMap(final Function1<A, Maybe<B>> mapper);

    public abstract A getOrElse(final A defaultValue);

    public static <A> Maybe<A> just(final A value) {
        return new Just<>(value);
    }

    public static <A> Nothing<A> nothing() {
        return new Nothing<>();
    }
}

class DeviceAPI {
    // this might return a null :-(
    DeviceInfo getDeviceInfo() {
        return new DeviceInfo();
    }

    // this will never be null
    Maybe<DeviceInfo> safeGetDeviceInfo() {
        return Maybe.just(new DeviceInfo());
    }
}

class DeviceInfo {
    // this might return a null :-(
    HardwareInfo getHardwareInfo() {
        return new HardwareInfo();
    }

    // this will never be null
    Maybe<HardwareInfo> safeGetHardwareInfo() {
        return Maybe.just(new HardwareInfo());
    }
}

class HardwareInfo {
    // this might return a null :-(
    Architecture getArchitecture() {
        return Architecture.VLIW;
    }

    // this will never be null
    Maybe<Architecture> safeGetArchitecture() {
        return Maybe.just(Architecture.VLIW);
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
public class Exercise5 {
    static void logging1(final DeviceAPI api) {
        String message = "Architecture info unavailable";
        final DeviceInfo deviceInfo = api.getDeviceInfo();
        if (deviceInfo != null) {
            final HardwareInfo hardwareInfo = deviceInfo.getHardwareInfo();
            if (hardwareInfo != null) {
                final Architecture architecture = hardwareInfo.getArchitecture();
                if (architecture != null) {
                    message = "#1 Architecture: " + architecture.name();
                }
            }
        }
        System.out.println(message);
    }

    static void logging2(final DeviceAPI api) {
        String message = "Architecture info unavailable";
        final Maybe<DeviceInfo> deviceInfo = api.safeGetDeviceInfo();
        if (deviceInfo.isPresent()) {
            final Maybe<HardwareInfo> hardwareInfo = deviceInfo.get().safeGetHardwareInfo();
            if (hardwareInfo.isPresent()) {
                final Maybe<Architecture> architecture = hardwareInfo.get().safeGetArchitecture();
                if (architecture.isPresent()) {
                    message = "#2 Architecture: " + architecture.get().name();
                }
            }
        }
        System.out.println(message);
    }

    static void logging3(final DeviceAPI api) {
        final String message = api
                .safeGetDeviceInfo()
                .flatMap(DeviceInfo::safeGetHardwareInfo)
                .flatMap(HardwareInfo::safeGetArchitecture)
                .map(architecture -> "#3 Architecture: " + architecture)
                .getOrElse("Architecture info unavailable");
        System.out.println(message);
    }

    public static void main(final String[] args) {
        logging1(new DeviceAPI());
        logging2(new DeviceAPI());
        logging3(new DeviceAPI());
    }
}
