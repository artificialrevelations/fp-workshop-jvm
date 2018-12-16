package io.github.ajoz.workshop.fp.java.part_4.solutions.practice_2;

import io.github.ajoz.workshop.fp.java.tools.Supplier;
import io.github.ajoz.workshop.fp.java.tools.Try;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;

/*
  AccidentMetadata can either be created out of an id and a file, or an id and
  url. We cannot have a situation where both File and Url is set, thus we can
  model AccidentMetadata as a simple sum type:

  AccidentMetadata = FileSource | UrlSource

  As both FileSource and UrlSource differ in the way that the data is loaded,
  we can make `loadAccident` abstract and allow each case to implement its own
  version.

  As we were asked by our clients to change the return type of `getData` to be
  more secure we can use the `Supplier.tryGet` instead of a simple `get`.

  We can go as far as changing the exceptions thrown from `loadAccident` from
  an unchecked to checked ones. The supplier can be constructed then with the
  `Supplier.ofChecked` method.
 */
@SuppressWarnings("WeakerAccess")
abstract class AccidentMetadata {
    private final Long id;
    private final Supplier<Accident> data = Supplier
            .ofChecked(this::loadAccident)
            .map(AccidentMetadata::dataToRecord)
            .map(AccidentMetadata::recordToAccident)
            .memoized();

    protected abstract String loadAccident() throws Exception;

    public Long getId() {
        return id;
    }

    public Try<Accident> getData() {
        return data.tryGet();
    }

    private AccidentMetadata(final Long id) {
        this.id = id;
    }

    public static final class FileSource extends AccidentMetadata {
        // We can move the exception to the newly created FileSource class, this
        // way we can now qualify it with the context it is used in so:
        // AccidentMetadata.FileSource.FileCorrupted
        public class FileCorrupted extends Exception {
            FileCorrupted(final String message) {
                super(message);
            }
        }

        private final File source;

        public FileSource(final Long id,
                          final File source) {
            super(id);
            this.source = source;
        }

        @Override
        protected String loadAccident() throws FileCorrupted {
            try {
                final Path path = source.toPath();
                final byte[] bytes = Files.readAllBytes(path);
                return new String(bytes);
            } catch (final IOException e) {
                throw new FileCorrupted(
                        String.format("Error when reading: %s", source.getAbsolutePath())
                );
            }
        }
    }

    public static final class UrlSource extends AccidentMetadata {
        public class URLCorrupted extends Exception {
            URLCorrupted(final String message) {
                super(message);
            }
        }

        private final URL source;

        public UrlSource(final Long id,
                         final URL source) {
            super(id);
            this.source = source;
        }

        @Override
        protected String loadAccident() throws URLCorrupted {
            try {
                final URLConnection connection = source.openConnection();

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
                throw new URLCorrupted("Error while reading URL: " + source.getPath());
            }
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
        final File file = new File("src/main/resources/part_4/accident.info");
        final AccidentMetadata fileSource = new AccidentMetadata.FileSource(42L, file);
        System.out.println("File based accident data = " + fileSource.getData());

        // Using URL Based
        final URL url = new URL("https://raw.githubusercontent.com/ajoz/fp-workshop-jvm/master/src/main/resources/part_4/accident.info");
        final AccidentMetadata urlSource = new AccidentMetadata.UrlSource(42L, url);
        System.out.println("URL based accident data = " + urlSource.getData());
    }
}

// Moved to the back because we already know this class
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