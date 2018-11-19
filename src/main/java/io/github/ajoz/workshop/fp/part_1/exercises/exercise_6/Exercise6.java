package io.github.ajoz.workshop.fp.part_1.exercises.exercise_6;

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


final class Customer {
    final String name;

    Customer(final String name) {
        this.name = name;
    }
}

final class Order {
    final String title;
    final Long date;

    Order(String title, Long date) {
        this.title = title;
        this.date = date;
    }
}

final class Database {
    // for now let's assume that the DB will always return an Order for a
    // given Customer
    Order findOrder(final Customer customer) {
        return new Order("JUG Łódź -- visit our FB, twitter and meetup!", 42L);
    }
}

@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);
}

@FunctionalInterface
interface Function2<A, B, C> {
    C apply(A a, B b);
}

@FunctionalInterface
interface Supplier<A> {
    A get();
}

public class Exercise6 {
    // Please do not change this function!
    private static final Function2<Customer, Database, Order> getOrderForCustomer =
            (customer, database) -> database.findOrder(customer);

    // Please do not change this function!
    private static final Supplier<Database> getProductionDatabase = Database::new;

    // Please do not change this function!
    private static final Function1<Order, String> getOrderTitle = order -> order.title;

    // Please do not change this function!
    private static final Function1<String, Long> getHash = value -> 42L;

    public static Function1<Customer, Long> getCustomerToHash() {
        throw new UnsupportedOperationException("Exercise6 getCustomerToHash is missing!");
    }
}
