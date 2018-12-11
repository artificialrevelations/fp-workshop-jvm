package io.github.ajoz.workshop.fp.java.part_4.solutions.exercise_1;

import io.github.ajoz.workshop.fp.java.tools.Supplier;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/*
  The order of the fields and methods in this file was changed to better show
  the changes that were made.

  - Did we achieve the laziness?
    Yes! and the class is fully immutable, much less problematic to debug or work
    with.
  - Are there any problems with the implementation?
    Yes :-( The code "wrapped" by the supplier is invoked each time the `getData`
    is called.
 */
@SuppressWarnings("unused")
final class AccidentMetadata {
    // Change #1: We make the existing fields immutable
    private final Long id;
    private final File source;
    // Change #2: We remove the data field
    // Change #3: We add a final field holding a Supplier<String>
    private final Supplier<String> data;

    // Change #4: We change the return type of the `loadAccident` method from void
    //            to String. We make the method private.
    private String loadAccident() {
        try {
            final Path path = source.toPath();
            final byte[] bytes = Files.readAllBytes(path);
            return new String(bytes); //instead of mutation we return a value
        } catch (final IOException e) {
            throw new SourceFileCorrupted(
                    String.format("Error when reading: %s", source.getAbsolutePath())
            );
        }
    }

    // Change #5: We use the supplier and wrap `loadAccident` inside it.
    //            We could initiate this field outside of the constructor.
    AccidentMetadata(final Long id,
                     final File source) {
        this.id = id;
        this.source = source;
        this.data = this::loadAccident;
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

public class Exercise1 {
    public static void main(final String[] args) {
        final File file = new File("src/main/resources/part_4/accident.info");
        final AccidentMetadata metadata = new AccidentMetadata(42L, file);

        final String data = metadata.getData();
        System.out.println("accident data = " + data);
    }
}
