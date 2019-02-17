package io.github.ajoz.workshop.fp.java.part_4.solutions.exercise_2;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicReference;

/*
  - what kind of functions pure or impure can be memoized?
    Only pure, referential transparent functions!

  - why?
    Because any side effect that is done by an impure function will occur only
    once for the memoized version. For example no logging like in the `loadAccident`
    below.
 */
@SuppressWarnings("unused")
interface Supplier<A> {
    A get();

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
