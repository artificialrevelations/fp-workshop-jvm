package io.github.ajoz.workshop.fp.java.part_1.exercises.exercise_6;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Function2;
import io.github.ajoz.workshop.fp.java.tools.Supplier;
import kotlin.Pair;

import java.util.Objects;

/*
  -- Putting the knowledge to use --

  Functional programming is about building complexity from small parts. Let's
  try to use the knowledge we gained so far to solve this simple example.

  Our apps domain is responsible for handling Customers and Orders. We
  already have a function implemented for retrieving Orders for a given Customer
  from a given Database. This function has a lot of domain logic and it is not
  feasible to refactor it now. We will have to use it while preparing our logic.

  One of the other teams we are working with prepared a code that expects a
  function from a Customer to hash (Long), but they would like us to provide
  the database for it.

  Our task is to write such function. We have all the necessary tools.

  Please pick and use methods/functions from previous exercises to finish this
  task.
 */

@FunctionalInterface
interface Consumer1<A> {
    void accept(A a);
}

// this object represents data related to the company customers
final class Customer {
    final String name;
    // other fields like: surname, address, etc.

    Customer(final String name) {
        this.name = name;
    }
}

// this object represents data related to the order that customers make
final class Order {
    final Title title;
    final Timestamp date;
    // other fields like: amount, currency, tax, etc.

    Order(final Title title,
          final Timestamp date) {
        this.title = title;
        this.date = date;
    }
}

// this object is here to emphasise not using Strings for everything
final class Title {
    final String title;

    Title(final String title) {
        this.title = title;
    }
}

// this object is for the similar reason as the Title :>
final class Timestamp {
    final Long unixTimestamp;

    Timestamp(final Long seconds) {
        this.unixTimestamp = seconds;
    }
}

// this object is for the similar reason as the Timestamp :>
final class Hash {
    final Long value;

    @Override
    public boolean equals(final Object other) {
        if (this == other)
            return true;

        if (other == null || getClass() != other.getClass())
            return false;

        final Hash hash = (Hash) other;
        return Objects.equals(value, hash.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    Hash(final Long value) {
        this.value = value;
    }
}

final class Database {
    // for now let's assume that the DB will always return an Order for a
    // given Customer
    Order findOrder(final Customer customer) {
        return new Order(
                new Title(String.format("FP Workshop - %s", customer.name)),
                new Timestamp(42L)
        );
    }
}

@SuppressWarnings("unused")
class Exercise6 {
    // Please do not change this function!
    private static final Function2<Customer, Database, Order> getOrderForCustomer =
            (customer, database) -> database.findOrder(customer);

    // Please do not change this function!
    private static final Supplier<Database> getProductionDatabase =
            Database::new;

    // Please do not change this function!
    private static final Function1<Order, Title> getOrderTitle =
            order -> order.title;

    // Please do not change this function!
    private static final Function1<Title, Hash> getTitleHash =
            value -> new Hash((long) value.title.length());

    public static Function1<Customer, Hash> getCustomerToHash() {
        return customer -> {

            /* Draft for the logical steps:
            Order order = getOrderForCustomer.apply(customer, getProductionDatabase.get());

            Title title = getOrderTitle.apply(order);

            Hash hash = getTitleHash.apply(title);

            return hash;
            */

            return getOrderForCustomer
                    .applySecond(getProductionDatabase)
                    .andThen(getOrderTitle)
                    .andThen(getTitleHash)
                    .apply(customer);
        };
    }

    static <A, B> Consumer1<A> composeConsumer(final Function1<A, B> function,
                                               final Consumer1<B> consumer) {
        return (A argA) -> consumer.accept(function.apply(argA));
    }

    static <A, B> Supplier<B> composeSupplier(final Function1<A, B> function,
                                              final Supplier<A> supplier) {
        return () -> function.apply(supplier.get());
    }

    static <A, B, C> Function1<B, C> applyFirst(final Function1<A, Function1<B, C>> function,
                                                final Supplier<A> supplier) {
        return (B argB) -> function.apply(supplier.get()).apply(argB);
    }

    static <A, B, C> Function1<A, C> applySecond(final Function1<A, Function1<B, C>> function,
                                                 final Supplier<B> supplier) {
        throw new UnsupportedOperationException("Exercise5 applyCurriedFirst is missing!");
    }

    static <A, B, C> Function1<Pair<A, B>, C> tuple(final Function2<A, B, C> function2) {
        return (Pair<A, B> abPair) -> function2.apply(abPair.getFirst(), abPair.getSecond());
    }

    static <A, B, C> Function1<A, Function1<B, C>> curry(final Function2<A, B, C> function2) {
        return (A argA) -> (B argb) -> function2.apply(argA, argb);
    }

    static <A, B, C> Function2<A, B, C> untuple(final Function1<Pair<A, B>, C> function1) {
        return (a, b) -> function1.apply(new Pair<>(a, b));
    }

    static <A, B, C> Function2<A, B, C> uncurry(final Function1<A, Function1<B, C>> function1) {
        return (A argA, B argB) -> function1.apply(argA).apply(argB);
    }

    static <A, B, C> Function2<B, A, C> flip(final Function2<A, B, C> function2) {
        return (B argB, A argA) -> function2.apply(argA, argB);
    }

    static <A, B, C> Function1<Pair<B, A>, C> flipTupled(final Function1<Pair<A, B>, C> function1) {
        return (Pair<B, A> baPair) -> function1.apply(new Pair<>(baPair.getSecond(), baPair.getFirst()));
    }

    static <A, B, C> Function1<B, Function1<A, C>> flipCurried(final Function1<A, Function1<B, C>> function1) {
        return (B argB) -> (A arga) -> function1.apply(arga).apply(argB);
    }
}
