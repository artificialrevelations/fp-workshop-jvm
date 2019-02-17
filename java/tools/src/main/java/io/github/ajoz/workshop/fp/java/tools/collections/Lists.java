package io.github.ajoz.workshop.fp.java.tools.collections;

import io.github.ajoz.workshop.fp.java.tools.Function2;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

import static java.util.Collections.emptyList;

public final class Lists {
    // returns a list with elements in the reverse order
    public static <A> List<A> reverse(final List<A> list) {
        final List<A> revered = new ArrayList<>(list.size());
        for (int i = list.size() - 1; i >= 0; i--) {
            revered.add(list.get(i));
        }
        return revered;
    }

    // returns a head of a list (first element)
    // if there is no element then throws a NoSuchElementException
    public static <A> A head(final List<A> list) {
        if (list.isEmpty())
            throw new NoSuchElementException("head of empty list");
        return list.get(0);
    }

    // returns a tail of a list (all elements except the head)
    // if there is no element then throws a NoSuchElementException
    // if there is only one element then an empty list is returned
    public static <A> List<A> tail(final List<A> list) {
        if (list.isEmpty())
            throw new UnsupportedOperationException("tail of empty list");
        else if (list.size() == 1)
            return emptyList();
        else
            return list.subList(1, list.size());
    }

    // Prepends the given item at the beginning of the list.
    public static <A> List<A> prepend(final A item, final List<A> list) {
        final List<A> newList = new LinkedList<>(); //to construct a new list
        newList.add(item);
        newList.addAll(list);
        return newList;
    }

    // Appends the given item at the end of the given list.
    public static <A> List<A> append(final List<A> list, final A item) {
        final List<A> newList = new LinkedList<>(list);
        newList.add(item);
        return newList;
    }

    // Folds the list from the left
    public static <A, B> B foldLeft(final List<A> list,
                                    final B initial,
                                    final Function2<B, A, B> operator) {
        B accumulator = initial;
        for (final A element : list) {
            accumulator = operator.apply(accumulator, element);
        }

        return accumulator;
    }

    // Fold the list from the right
    public static <A, B> B foldRight(final List<A> list,
                                     final B initial,
                                     final Function2<A, B, B> operator) {
        B accumulator = initial;
        for (int i = list.size(); i > 0; i--) {
            accumulator = operator.apply(list.get(i - 1), accumulator);
        }
        return accumulator;
    }

    public static Integer sum(final List<Integer> list) {
        return foldLeft(list, 0, (a, b) -> a + b);
    }

    public static Integer product(final List<Integer> list) {
        return foldLeft(list, 1, (a, b) -> a * b);
    }
}
