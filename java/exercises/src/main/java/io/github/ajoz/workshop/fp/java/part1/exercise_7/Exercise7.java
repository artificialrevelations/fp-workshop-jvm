package io.github.ajoz.workshop.fp.java.part1.exercise_7;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Function2;
import io.github.ajoz.workshop.fp.java.tools.Supplier;

import java.util.Objects;

/*
  -- Putting the knowledge to use --

  Functional programming is about building complexity from small parts. Let's
  use the knowledge we gained so far to solve the task below.

  You are working for a large company called EyeKeyA that is delivering ultra
  high quality furniture to the customers all over the world. Just as all other
  respected furniture companies, it has an IT department that is maintaining
  and improving company's software.

  You are on the team that maintains the Cutomers and Orders related modules.
  It's a very tough time as several teams including yours are working on a new
  exciting feature. You are tasked with creating a function that takes a customer
  information as an argument and returns a hash of the order that customer made.

  The module you and your team are working on already has a lot of useful
  functions:
  - to get the Order for a particular Customer
  - to get the Database from which Orders can be retrieved
  - to get the Metadata of the Order
  - to get the Hash from the given Metadata
 */
@SuppressWarnings({"unused", "WeakerAccess"})
class Exercise7 {
    // Please do not change this function!
    private static final Function2<Customer, Database, Order> getOrderForCustomer =
            (customer, database) -> database.findOrder(customer);

    // Please do not change this function!
    private static final Supplier<Database> getProductionDatabase =
            Database::new;

    // Please do not change this function!
    private static final Function1<Order, Metadata> getOrderMetadata =
            order -> order.metadata;

    // Please do not change this function!
    private static final Function1<Metadata, Hash> getHashFromMetadata =
            metadata -> new Hash((long) metadata.info.length());

    /*
       Please solve this exercise using any of the tools we've built so far.
     */
    public static Function1<Customer, Hash> getCustomerOrderHash() {
        throw new UnsupportedOperationException("Exercise 7 getCustomerOrderHash is missing!");
    }
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
@SuppressWarnings("WeakerAccess")
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

// this object is here to emphasise not using Strings for everything
final class Metadata {
    final String info;

    Metadata(final String info) {
        this.info = info;
    }
}

// this object is for the similar reason as the Metadata :>
@SuppressWarnings("WeakerAccess")
final class Timestamp {
    final Long unixTimestamp;

    Timestamp(final Long seconds) {
        this.unixTimestamp = seconds;
    }
}

// this object is for the similar reason as the Timestamp :>
@SuppressWarnings("WeakerAccess")
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
