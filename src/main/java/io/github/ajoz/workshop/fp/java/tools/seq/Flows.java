package io.github.ajoz.workshop.fp.java.tools.seq;

import java.util.LinkedList;
import java.util.List;

public final class Flows {

    public static <A> List<A> toList(final Flow<A> flow) {
        final List<A> list = new LinkedList<>();
        for (final A item : flow) {
            list.add(item);
        }
        return list;
    }

}
