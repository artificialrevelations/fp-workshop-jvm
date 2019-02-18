package io.github.ajoz.workshop.fp.java.part1.exercise3;

import static io.github.ajoz.workshop.fp.java.part1.exercise3.Example.getSecureProductPurchaseUrl;
import static io.github.ajoz.workshop.fp.java.part1.exercise3.Exercise3.compose;
import static io.github.ajoz.workshop.fp.java.part1.exercise3.Exercise3.composeAll_1;
import static io.github.ajoz.workshop.fp.java.part1.exercise3.Exercise3.composeAll_2;
import static io.github.ajoz.workshop.fp.java.part1.exercise3.Exercise3.composeIntFuns;
import static org.junit.Assert.assertEquals;

import io.github.ajoz.workshop.fp.java.part1.exercise3.Example.Description;
import io.github.ajoz.workshop.fp.java.part1.exercise3.Example.Id;
import io.github.ajoz.workshop.fp.java.part1.exercise3.Example.Product;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

@RunWith(Enclosed.class)
public class Exercise3Test {
    public static class ComposeIntFuns {
        @Test
        public void should_compose_two_simple_functions() {
            // given:
            final Function1<Integer, Integer> f = x -> x + 1;
            final Function1<Integer, Integer> g = x -> x + 2;

            // when:
            final Function1<Integer, Integer> h = composeIntFuns(f, g);

            // then:
            assertEquals(new Integer(3), h.apply(0));
            assertEquals(new Integer(4), h.apply(1));
        }

        @Test
        public void should_compose_two_identity_functions() {
            // given:
            final Function1<Integer, Integer> f = x -> x;
            final Function1<Integer, Integer> g = x -> x;

            // when:
            final Function1<Integer, Integer> h = composeIntFuns(f, g);

            // then:
            assertEquals(new Integer(0), h.apply(0));
            assertEquals(new Integer(1), h.apply(1));
        }

        @Test
        public void should_compose_two_isomorphic_functions() {
            // given:
            final Function1<Integer, Integer> f = x -> x + 1;
            final Function1<Integer, Integer> g = x -> x - 1;

            // when:
            final Function1<Integer, Integer> h = composeIntFuns(f, g);

            // then:
            assertEquals(new Integer(0), h.apply(0));
            assertEquals(new Integer(1), h.apply(1));
        }
    }

    public static class Compose {
        @Test
        public void should_compose_two_functions() {
            // given:
            final Function1<String, Integer> f = Integer::valueOf;
            final Function1<Integer, Boolean> g = integer -> integer == 42;

            // when:
            final Function1<String, Boolean> h = compose(f, g);

            // then:
            assertEquals(Boolean.TRUE, h.apply("42"));
            assertEquals(Boolean.FALSE, h.apply("24"));
        }
    }

    public static class ComposeAll {
        @Test
        public void multiple_functions_first_solution() {
            // given:
            final Function1<Integer, Integer> f = x -> x + 1;
            final Function1<Integer, Integer> g = x -> x + 2;
            final Function1<Integer, Integer> h = x -> x + 3;
            final Function1<Integer, Integer> i = x -> x + 4;

            // when:
            final Function1<Integer, Integer> j = composeAll_1(
                    f, g, h, i
            );

            // then:
            assertEquals(new Integer(10), j.apply(0));
            assertEquals(new Integer(0), j.apply(-10));
        }

        @Test
        public void multiple_functions_second_solution() {
            // given:
            final Function1<Integer, Integer> f = x -> x + 1;
            final Function1<Integer, Integer> g = x -> x + 2;
            final Function1<Integer, Integer> h = x -> x + 3;
            final Function1<Integer, Integer> i = x -> x + 4;

            // when:
            final Function1<Integer, Integer> j = composeAll_2(
                    f, g, h, i
            );

            // then:
            assertEquals(new Integer(10), j.apply(0));
            assertEquals(new Integer(0), j.apply(-10));
        }
    }

    public static class Function1Interface {
        @Test
        public void should_use_andThen() {
            // given:
            final Function1<String, Integer> f = Integer::valueOf;
            final Function1<Integer, Boolean> g = integer -> integer == 42;

            // when:
            final Function1<String, Boolean> h = f.andThen(g);

            // then:
            assertEquals(Boolean.TRUE, h.apply("42"));
            assertEquals(Boolean.FALSE, h.apply("24"));
        }

        @Test
        public void should_use_compose() {
            // given:
            final Function1<Integer, Boolean> g = integer -> integer == 42;
            final Function1<String, Integer> f = Integer::valueOf;

            // when:
            final Function1<String, Boolean> h = g.compose(f);

            // then:
            assertEquals(Boolean.TRUE, h.apply("42"));
            assertEquals(Boolean.FALSE, h.apply("24"));
        }
    }


    public static class ProductPurchaseUrl {
        @Test
        public void should_return_a_valid_secure_url() {
            // given:
            final String productId = "WORKSHOPTESTS42";
            final Product product =
                    new Product(
                            new Id(productId),
                            new Description("The more tests the better?")
                    );
            final String expected = "https://nozama.com/shop/purchase?=" + productId;

            // when:
            final String actual = getSecureProductPurchaseUrl.apply(product);

            // then:
            assertEquals(expected, actual);
        }
    }
}
