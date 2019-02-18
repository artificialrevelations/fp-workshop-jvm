package io.github.ajoz.workshop.fp.part2.exercise4;

import io.github.ajoz.workshop.fp.tools.Pair;
import io.github.ajoz.workshop.fp.tools.collections.Lists;

import java.util.Collections;
import java.util.List;

class Exercise4 {
    static Integer maximum(final List<Integer> list) {
        if (list.isEmpty())
            throw new IllegalArgumentException("list is empty");

        return Lists.foldLeft(list, Integer.MIN_VALUE, Math::max);
    }

    static Integer minimum(final List<Integer> list) {
        if (list.isEmpty())
            throw new IllegalArgumentException("list is empty");

        return Lists.foldLeft(list, Integer.MAX_VALUE, Math::min);
    }

    static <A> Integer count(final List<A> list) {
        return Lists.foldLeft(list, 0, (count, item) -> count + 1);
    }

    static <A> A last(final List<A> list) {
        return Lists.foldLeft(list, Lists.head(list), (x, y) -> y);
    }

    static <A> List<A> reverse(final List<A> list) {
        return Lists.foldLeft(list, Collections.emptyList(),
                (reversedList, item) -> Lists.prepend(item, reversedList)
        );
    }

    static Integer average(final List<Integer> list) {
        return list.isEmpty() ? 0 : Lists.sum(list) / list.size();
    }

    static <A> Boolean contains(final List<A> list,
                                final A searched) {
        return Lists.foldLeft(list, false,
                (contains, item) -> contains || item.equals(searched)
        );
    }

    static <A> String join(final List<A> list,
                           final String separator) {
        if (list.isEmpty())
            return "";
        else
            return Lists.foldLeft(
                    Lists.tail(list),
                    Lists.head(list).toString(),
                    (joined, item) -> joined + separator + item
            );
    }

    static <A> A penultimate(final List<A> list) {
        if (list.isEmpty())
            throw new IllegalArgumentException("list is empty");

        if (list.size() == 1)
            throw new IllegalArgumentException("list has only one element");

        return Lists.foldLeft(
                list,
                new Pair<>(
                        Lists.head(list),
                        Lists.head(Lists.tail(list))
                ),
                (tuple, a) -> new Pair<>(tuple.getSecond(), a)
        ).getFirst();
    }
}
