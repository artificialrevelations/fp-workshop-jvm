package org.artrev.workshop.fp.part5.exercise_2;

import org.artrev.workshop.fp.tools.Function1;
import org.artrev.workshop.fp.tools.control.Maybe;
import org.artrev.workshop.fp.tools.control.Try;

import java.util.Collections;
import java.util.List;

@SuppressWarnings("unused")
final class Maybes {
    static <A, B> Function1<A, Maybe<B>> liftP(final Function1<A, B> function) {
        return a -> {
            try {
                return Maybe.some(function.apply(a));
            } catch (final Exception e) {
                return Maybe.none();
            }
        };
    }
}

@SuppressWarnings("unused")
final class Trys {
    static <A, B> Function1<A, Try<B>> liftP(final Function1<A, B> function) {
        return a -> Try.ofNullable(a).map(function);
    }
}

@SuppressWarnings("unused")
public class Exercise2 {
    public static Integer div(final Integer a, final Integer b) {
        return a / b;
    }

    public static <A> A head(final List<A> list) {
        return list.get(0);
    }

    public static void main(final String[] args) {
        final Function1<List<String>, Maybe<String>> maybeHead =
                Maybes.liftP(Exercise2::head);

        System.out.println("maybeHead.apply(emptyList) = " + maybeHead.apply(Collections.emptyList()));

        final Function1<List<String>, Try<String>> tryHead =
                Trys.liftP(Exercise2::head);

        System.out.println("tryHead.apply(emptyList) = " + tryHead.apply(Collections.emptyList()));
    }
}
