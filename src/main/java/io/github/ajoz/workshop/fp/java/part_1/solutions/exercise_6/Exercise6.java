package io.github.ajoz.workshop.fp.java.part_1.solutions.exercise_6;


import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Function2;
import io.github.ajoz.workshop.fp.java.tools.Supplier;

final class Customer {
    final String name;
    // other fields like: surname, address, etc.

    Customer(final String name) {
        this.name = name;
    }
}

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

final class Title {
    final String title;

    Title(final String title) {
        this.title = title;
    }
}

final class Timestamp {
    final Long unixTimestamp;

    Timestamp(final Long seconds) {
        this.unixTimestamp = seconds;
    }
}

final class Hash {
    final Long value;

    Hash(final Long value) {
        this.value = value;
    }
}

final class Database {
    // for now let's assume that the DB will always return an Order for a
    // given Customer
    Order findOrder(final Customer customer) {
        return new Order(
                new Title("JUG Łódź -- visit our FB, twitter and meetup!"),
                new Timestamp(42L)
        );
    }
}

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
        // flip the arguments of the original
        // change it to curried form
        // partially apply the first argument
        // change the Order to a title
        // change the title to a secret hash

        return applyFirst(getOrderForCustomer.flip().curry(), getProductionDatabase)
                .andThen(getOrderTitle)
                .andThen(getTitleHash);
    }

    @SuppressWarnings("SameParameterValue")
    private static <A, B, C> Function1<B, C> applyFirst(final Function1<A, Function1<B, C>> function,
                                                        final Supplier<A> supplier) {
        return (B b) -> function.apply(supplier.get()).apply(b);
    }
}
