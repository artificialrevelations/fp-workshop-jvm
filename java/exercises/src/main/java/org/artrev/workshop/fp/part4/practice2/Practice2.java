package org.artrev.workshop.fp.part4.practice2;

import org.artrev.workshop.fp.tools.Supplier;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;

/*
  -- Illegal States of Accident Metadata --

  Your new API for AccidentMetadata is a success. Everyone in the "THERE" company
  is super happy with it until suddenly someone from QA reports that the app has
  problems on low-end devices with little to not storage space.

  This is a problem because Management has plans to storm the low-end devices
  market. You and your team are tasked with improving the AccidentMetadata API
  with capability of loading Accident information from a given URL.

  As you are very preoccupied with other things in the moment, someone from your
  team gets to do it. The problem is that he is not as proficient with design
  as you.

  After few days he comes back with the class below, although it is working it
  allows for illegal states to happen.

  The API can now be used to create a metadata that both has the file and url
  set to null. This is why a new exception was created called: MetadataCorrupted.
  The `loadAccident` method is getting complicated and long.

  You got a new requirement that the `getData` should be safe to use, so this
  means that no RuntimeExceptions should be thrown.

  Please rework the `AccidentMetadata`:
  - illegal states should not be representable
  - `getData` should return Try<A> instead of a bare A
  - loading the accident data should now throw checked exceptions

  Hints:
  - we do not need to store the File and URL as Maybe<File> or Maybe<URL> as this
    would cause the `loadAccident` to be as complex as it previously was
  - in the previous part we learned about modeling with types, can we use the
    knowledge here?
 */
@SuppressWarnings("WeakerAccess")
final class AccidentMetadata {
    private final Long id;
    private final File file;
    private final URL url;

    private final Supplier<Accident> data = Supplier
            .of(this::loadAccident)
            .map(AccidentMetadata::dataToRecord)
            .map(AccidentMetadata::recordToAccident)
            .memoized();

    protected String loadAccident() {
        if (null != file) {
            try {
                final Path path = file.toPath();
                final byte[] bytes = Files.readAllBytes(path);
                return new String(bytes);
            } catch (final IOException e) {
                throw new FileCorrupted(
                        String.format("Error when reading: %s", file.getAbsolutePath())
                );
            }
        }

        if (null != url) {
            try {
                final URLConnection connection = url.openConnection();

                try (
                        final InputStream is = connection.getInputStream();
                        final InputStreamReader isr = new InputStreamReader(is, StandardCharsets.UTF_8);
                        final BufferedReader br = new BufferedReader(isr)
                ) {
                    final StringBuilder builder = new StringBuilder();
                    String line;
                    while ((line = br.readLine()) != null) {
                        builder.append(line);
                    }
                    return builder.toString();
                }
            } catch (final IOException e) {
                throw new URLCorrupted("Error while reading URL: " + url.getPath());
            }
        }

        throw new MetadataCorrupted("Both File and URL are null!");
    }

    public Long getId() {
        return id;
    }

    public Accident getData() {
        return data.get();
    }

    public AccidentMetadata(final Long id,
                            final File file) {
        this.id = id;
        this.file = file;
        this.url = null;
    }

    public AccidentMetadata(final Long id,
                            final URL url) {
        this.id = id;
        this.file = null;
        this.url = url;
    }

    public static class FileCorrupted extends RuntimeException {
        FileCorrupted(final String message) {
            super(message);
        }
    }

    public static class URLCorrupted extends RuntimeException {
        URLCorrupted(final String message) {
            super(message);
        }
    }

    public static class MetadataCorrupted extends RuntimeException {
        MetadataCorrupted(final String message) {
            super(message);
        }
    }

    private static Accident recordToAccident(final String[] record) {
        final Double latitude = Double.valueOf(record[0]);
        final Double longitude = Double.valueOf(record[1]);
        final String message = record[2];
        return new Accident(latitude, longitude, message);
    }

    private static String[] dataToRecord(final String data) {
        return data.split(",");
    }
}

public class Practice2 {
    public static void main(final String[] args) throws MalformedURLException {
        // Using File Based
        final File file = new File("src/main/resources/part4/accident.info");
        final AccidentMetadata fileSource = new AccidentMetadata(42L, file);
        System.out.println("File based accident data = " + fileSource.getData());

        // Using URL Based
        final URL url = new URL("https://raw.githubusercontent.com/ajoz/fp-workshop-jvm/master/src/main/resources/part4/accident.info");
        final AccidentMetadata urlSource = new AccidentMetadata(24L, url);
        System.out.println("URL based accident data = " + urlSource.getData());
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