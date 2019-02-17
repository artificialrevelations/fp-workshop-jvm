package io.github.ajoz.workshop.fp.java.part5.solutions.exercise_3;

import io.github.ajoz.workshop.fp.java.tools.control.Try;

final class Id {
}

final class Product {
}

@SuppressWarnings({"WeakerAccess", "unused", "RedundantThrows"})
final class API {
    static Try<Product> getProductFromService(final Id id) {
        return Try.success(new Product()); // super heavy business logic!
    }

    static Try<Product> getProductFromDatabase(final Id id) {
        return Try.success(new Product()); // even more business logic then before!
    }

    static Try<Product> getProductFromCache(final Id id) {
        return Try.success(new Product()); // just a lot of business logic!
    }

    static Try<Product> getProduct(final Id id) {
        return getProductFromCache(id)
                .orElse(() -> getProductFromDatabase(id))
                .orElse(() -> getProductFromService(id));
    }

    static class ProductIsMissing extends Exception {
    }

    static class ProductNotInCache extends Exception {
    }

    static class ProductNotInDatabase extends Exception {
    }

    static class ProductNotInService extends Exception {
    }
}

public class Exercise3 {
    public static void main(final String[] args) {
        API.getProduct(new Id())
                .ifFailure(System.out::println);
    }
}
