package io.github.ajoz.workshop.fp.java.part1.solutions.exercise_3;

import java.util.Arrays;
import java.util.List;

class Exercise3 {
    // Part 1
    static Function1<Integer, Integer> composeIntFuns(final Function1<Integer, Integer> first,
                                                      final Function1<Integer, Integer> second) {
        return (Integer value) -> second.apply(first.apply(value));
    }

    // Part 2
    static <A, B, C> Function1<A, C> compose(final Function1<A, B> f,
                                             final Function1<B, C> g) {
        return (A a) -> g.apply(f.apply(a));
    }

    // Part 3
    @SafeVarargs
    static <A> Function1<A, A> composeAll_1(final Function1<A, A>... functions) {
        return (A a) -> {
            A result = a;
            for (final Function1<A, A> function : functions) {
                result = function.apply(result);
            }
            return result;
        };
    }

    // Part 4
    @SafeVarargs
    static <A> Function1<A, A> composeAll_2(final Function1<A, A>... functions) {
        Function1<A, A> result = x -> x;
        for (Function1<A, A> function : functions) {
            result = compose(result, function);
        }
        return result;
    }
}

@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);

    // Part 5
    default <C> Function1<A, C> andThen(final Function1<B, C> after) {
        return a -> after.apply(this.apply(a));
    }

    // Part 6
    default <C> Function1<C, B> compose(final Function1<C, A> before) {
        return c -> this.apply(before.apply(c));
    }
}


class Example {
    static class Product {
        public Id id;
        Description description;
        // other fields

        Product(final Id id,
                final Description description) {
            this.id = id;
            this.description = description;
        }
    }

    static class Id {
        public String value;

        Id(final String value) {
            this.value = value;
        }
    }

    static class Description {
        public String name;

        Description(final String name) {
            this.name = name;
        }
    }

    static final Function1<Product, Id> getProductId =
            product -> product.id;

    static final Function1<Id, String> getPurchaseUri =
            productId -> "nozama.com/shop/purchase?=" + productId.value;

    static final Function1<String, String> getSecureUrl =
            uri -> "https://" + uri;

    /*
      Compose those three functions together to get a third function:
     */
    static final Function1<Product, String> getSecureProductPurchaseUrl =
            getProductId.andThen(getPurchaseUri).andThen(getSecureUrl);

    public static void main(final String[] args) {
        // You can play here with the code:

        final List<Product> products =
                Arrays.asList(
                        new Product(
                                new Id("JUGLDZ42JAVA"),
                                new Description("Workshop: FP in Java")
                        ),
                        new Product(
                                new Id("JUGLDZ42KOTLIN"),
                                new Description("Workshop: FP in Kotlin")
                        )
                );

        for (final Product product : products) {
            System.out.println(getSecureProductPurchaseUrl.apply(product));
        }
    }
}