package io.github.ajoz.workshop.fp.java.part_1.solutions.exercise_6;

final class Customer {
    private final String name;

    Customer(final String name) {
        this.name = name;
    }
}

final class Order {
    final String title;
    private final Long date;

    Order(String title, Long date) {
        this.title = title;
        this.date = date;
    }
}

final class Database {
    io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6.Order findOrder(final io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6.Customer customer) {
        return new io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6.Order("JUG Łódź -- visit our FB, twitter and meetup!", 42L);
    }
}

@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);

    default <C> Function1<A, C> andThen(final Function1<B, C> after) {
        return a -> after.apply(this.apply(a));
    }
}

@FunctionalInterface
interface Function2<A, B, C> {
    C apply(A a, B b);
}

@FunctionalInterface
interface Supplier<A> {
    A get();
}

class Exercise6 {
    private static final Function2<io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6.Customer, io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6.Database, io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6.Order> getOrderForCustomer =
            (customer, database) -> database.findOrder(customer);

    private static final Supplier<io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6.Database> getProductionDatabase = io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6.Database::new;

    private static final Function1<io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6.Order, String> getOrderTitle = order -> order.title;

    private static final Function1<String, Long> getHash = value -> 42L;

    public static Function1<io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6.Customer, Long> getCustomerToHash() {
        // flip the arguments of the original
        // change it to curried form
        // partially apply the first argument
        // change the Order to a title
        // change the title to a secret hash
        return applyFirst(curry(flip(getOrderForCustomer)), getProductionDatabase)
                .andThen(getOrderTitle)
                .andThen(getHash);
    }

    private static <A, B, C> Function1<A, Function1<B, C>> curry(final Function2<A, B, C> function2) {
        return (A a) -> (B b) -> function2.apply(a, b);
    }

    private static <A, B, C> Function2<B, A, C> flip(final Function2<A, B, C> function2) {
        return (B b, A a) -> function2.apply(a, b);
    }

    private static <A, B, C> Function1<B, C> applyFirst(final Function1<A, Function1<B, C>> function,
                                                        final Supplier<A> supplier) {
        return (B b) -> function.apply(supplier.get()).apply(b);
    }
}
