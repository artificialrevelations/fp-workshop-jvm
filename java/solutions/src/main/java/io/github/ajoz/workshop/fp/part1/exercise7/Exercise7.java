package io.github.ajoz.workshop.fp.part1.exercise7;

import io.github.ajoz.workshop.fp.tools.Function1;
import io.github.ajoz.workshop.fp.tools.Function2;
import io.github.ajoz.workshop.fp.tools.Supplier;

import java.util.Objects;

class Exercise7 {
    private static final Function2<Customer, Database, Order> getOrderForCustomer =
            (customer, database) -> database.findOrder(customer);

    private static final Supplier<Database> getProductionDatabase =
            Database::new;

    private static final Function1<Order, Metadata> getOrderMetadata =
            order -> order.metadata;

    private static final Function1<Metadata, Hash> getTitleHash =
            value -> new Hash((long) value.info.length());

    static Function1<Customer, Hash> getCustomerOrderHash() {
        /*
        // this version is using flipping, currying, application of the first argument and composition
        return CurriedFunctions
                .applyFirst(getOrderForCustomer.flip().curry(), getProductionDatabase)
                .andThen(getOrderMetadata)
                .andThen(getTitleHash);
         */

        /*
        // this version is using currying, application of first value and composition
        return CurriedFunctions
                .applySecond(getOrderForCustomer.curry(), getProductionDatabase)
                .andThen(getOrderMetadata)
                .andThen(getTitleHash);
         */

        /*
        // this version is skipping currying and just works with two arg function
        return getOrderForCustomer
                .flip()
                .applyFirst(getProductionDatabase)
                .andThen(getOrderMetadata)
                .andThen(getTitleHash);
        */

        // this is probably the easiest to grasp and read in Java version
        // for anyone that is not used to functional programming
        return getOrderForCustomer                  // (Customer, Database) -> Order
                .applySecond(getProductionDatabase) // Customer -> Order
                .andThen(getOrderMetadata)          // Customer -> Metadata
                .andThen(getTitleHash);             // Customer -> Hash
    }
}


final class Customer {
    final String name;
    // other fields like: surname, address, etc.

    Customer(final String name) {
        this.name = name;
    }
}

final class Order {
    final Metadata metadata;
    final Timestamp date;
    // other fields like: amount, currency, tax, etc.

    Order(final Metadata metadata,
          final Timestamp date) {
        this.metadata = metadata;
        this.date = date;
    }
}

final class Metadata {
    final String info;

    Metadata(final String info) {
        this.info = info;
    }
}

@SuppressWarnings("WeakerAccess")
final class Timestamp {
    final Long unixTimestamp;

    Timestamp(final Long seconds) {
        this.unixTimestamp = seconds;
    }
}

@SuppressWarnings("ALL")
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
                new Metadata(String.format("FP Workshop - %s", customer.name)),
                new Timestamp(42L)
        );
    }
}

