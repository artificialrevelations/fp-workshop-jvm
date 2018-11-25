package io.github.ajoz.workshop.fp.java.tools;

import java.util.ArrayList;
import java.util.List;

public final class Lists {
    public static <A> List<A> reverse(final List<A> list) {
        final List<A> revered = new ArrayList<>(list.size());
        for (int i = list.size() - 1; i >= 0; i--) {
            revered.add(list.get(i));
        }
        return revered;
    }
}
