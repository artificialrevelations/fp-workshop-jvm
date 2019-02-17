package io.github.ajoz.workshop.fp.java.tools.flow;

import io.github.ajoz.workshop.fp.java.tools.Function2;
import io.github.ajoz.workshop.fp.java.tools.control.Try;

import java.util.LinkedList;
import java.util.List;

@SuppressWarnings("ALL")
public final class Flows {
    public static <A> List<A> toList(final Flow<A> flow) {
        final List<A> list = new LinkedList<>();
        for (final A item : flow) {
            list.add(item);
        }
        return list;
    }

    static <A, B> B foldLeft(final Function2<B, A, B> function,
                             final B initial,
                             final Flow<A> flow) {
        final Try<A> next = flow.next();
        if (next.isFailure())
            return initial;

        return foldLeft(function, function.apply(initial, next.get()), flow);
    }
}
