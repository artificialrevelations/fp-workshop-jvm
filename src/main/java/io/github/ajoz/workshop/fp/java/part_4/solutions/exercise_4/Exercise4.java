package io.github.ajoz.workshop.fp.java.part_4.solutions.exercise_4;

import io.github.ajoz.workshop.fp.java.tools.Consumer1;
import io.github.ajoz.workshop.fp.java.tools.Effect;
import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

@SuppressWarnings("unused")
interface Supplier<A> {
    A get();

    default <B> Supplier<B> map(final Function1<A, B> function) {
        return () -> function.apply(this.get());
    }

    default <B> Supplier<B> flatMap(final Function1<A, Supplier<B>> function) {
        return () -> function.apply(this.get()).get();
    }

    default Supplier<A> before(final Effect effect) {
        final Supplier<A> self = this;
        return () -> {
            effect.perform();
            return self.get();
        };
    }

    default Supplier<A> after(final Consumer1<A> effect) {
        final Supplier<A> self = this;
        return () -> {
            final A result = self.get();
            effect.accept(result);
            return result;
        };
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

    static <A> Supplier<A> of(final Supplier<A> supplier) {
        return supplier;
    }
}

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
        Accident accident = (Accident) other;
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
    private final Supplier<Accident> data;

    AccidentMetadata(final Long id,
                     final File source) {
        this.id = id;
        this.source = source;
        this.data = Supplier
                .of(this::loadAccident)
                // before the loading print logs to console
                .before(() ->
                        System.out.println(String.format("Loading Accident id = %d", id))
                )
                // parse everything
                .map(AccidentMetadata::dataToRecord)
                // print the lines
                .after(AccidentMetadata::printRecord)
                // transform the record
                .map(AccidentMetadata::recordToAccident)
                // print the result
                .after(System.out::println)
                .memoized();
    }

    // We should strive to avoid bare lambdas and upgrade them to named
    // functions
    private static Accident recordToAccident(final String[] record) {
        final Double latitude = Double.valueOf(record[0]);
        final Double longitude = Double.valueOf(record[1]);
        final String message = record[2];
        return new Accident(latitude, longitude, message);
    }

    private static void printRecord(final String[] record) {
        System.out.println(Arrays.toString(record));
    }

    private static String[] dataToRecord(final String data) {
        return data.split(",");
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

    public Accident getData() {
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
        final File file = new File("src/main/resources/part_4/accident.info");
        final AccidentMetadata metadata = new AccidentMetadata(42L, file);

        // Let's see the console and check if the accident data is correctly
        // memoized
        Accident data = null;
        for (int i = 0; i < 100; i++) {
            data = metadata.getData();
        }

        System.out.println("accident data = " + data);
    }
}
