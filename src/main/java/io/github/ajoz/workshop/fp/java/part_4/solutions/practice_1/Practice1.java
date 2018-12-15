package io.github.ajoz.workshop.fp.java.part_4.solutions.practice_1;

import io.github.ajoz.workshop.fp.java.tools.CheckedSupplier;
import io.github.ajoz.workshop.fp.java.tools.Maybe;
import io.github.ajoz.workshop.fp.java.tools.Try;

import java.util.concurrent.atomic.AtomicReference;

@SuppressWarnings("unused")
interface Supplier<A> {
    A get();

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

    default A getOrElse(final A defaultValue) {
        try {
            return get();
        } catch (final Exception exception) {
            return defaultValue;
        }
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

        final Supplier<String> memoized = supplier.memoized();
        for (int i = 0; i < 100; i++) {
            supplier.maybeGet()
                    .ifSome(System.out::println)
                    .ifNone(() -> System.out.println("none result :("));

            supplier.tryGet()
                    .ifSuccess(System.out::println)
                    .ifFailure(System.out::println);
        }
    }
}
