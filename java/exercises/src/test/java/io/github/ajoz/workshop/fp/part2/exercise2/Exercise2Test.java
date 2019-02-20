package io.github.ajoz.workshop.fp.part2.exercise2;

import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

/*import org.junit.Test;
import java.util.Collections;
import java.util.List;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;*/

@RunWith(Enclosed.class)
public class Exercise2Test {
    // uncomment to test the implementation of `sum` and `product` in terms of bar

    /*public static class SumTest {

        @Test
        public void shouldReturnPositiveValueForSinglePositiveItemListSum() {
            // given:
            final List<Integer> list = Collections.singletonList(128);
            final Integer expected = 128;

            // when:
            final Integer actual = sum(list);

            // then:
            assertThat(actual, is(expected));
        }

        @Test
        public void shouldReturnNegativeValueForSingleNegativeItemListSum() {
            // given:
            final List<Integer> list = Collections.singletonList(-256);
            final Integer expected = -256;

            // when:
            final Integer actual = sum(list);

            // then:
            assertThat(actual, is(expected));
        }

        @Test
        public void shouldReturnZeroForEmptyListSum() {
            // given:
            final List<Integer> emptyList = emptyList();
            final Integer expected = 0;
            // when:
            final Integer actual = sum(emptyList);
            // then:
            assertThat(actual, is(expected));
        }

        @Test
        public void shouldReturnNegativeValueSum() {
            // given:
            final List<Integer> negativeList = asList(-1, -1, -1, -1);
            final Integer expected = -4;
            // when:
            final Integer actual = sum(negativeList);
            // then:
            assertThat(actual, is(expected));
        }

        @Test
        public void shouldReturnPositiveValueSum() {
            // given:
            final List<Integer> positiveList = asList(1, 1, 1, 1);
            final Integer expected = 4;
            // when:
            final Integer actual = sum(positiveList);
            // then:
            assertThat(actual, is(expected));
        }

        @Test
        public void shouldReturnZeroForMixedValuesSum() {
            // given:
            final List<Integer> mixedList = asList(-1, -2, 1, 2);
            final Integer expected = 0;
            // when:
            final Integer actual = sum(mixedList);
            // then:
            assertThat(actual, is(expected));
        }

        @Test
        public void shouldReturnPositiveValueForIncrementalValuesSum() {
            // given:
            final List<Integer> incrementalList = asList(1, 2, 3);
            final Integer expected = 6;
            // when:
            final Integer actual = sum(incrementalList);
            // then:
            assertThat(actual, is(expected));
        }

        @Test
        public void shouldReturnZeroForListWithAllZerosSum() {
            // given:
            final List<Integer> list = asList(0, 0, 0, 0, 0, 0);
            final Integer expected = 0;
            // when:
            final Integer actual = sum(list);
            // then:
            assertThat(actual, is(expected));
        }
    }

    public static class ProductTest {

        @Test
        public void shouldReturnOneForEmptyListProduct() {
            // given:
            final List<Integer> emptyList = emptyList();
            final Integer expected = 1;
            // when:
            final Integer actual = product(emptyList);
            // then:
            assertThat(actual, is(expected));
        }

        @Test
        public void shouldReturnPositiveForPositiveListProduct() {
            // given:
            final List<Integer> positiveList = asList(1, 2, 3);
            final Integer expected = 6;
            // when:
            final Integer actual = product(positiveList);
            // then:
            assertThat(actual, is(expected));
        }

        @Test
        public void shouldReturnPositiveForEvenNumberOfNegativeValuesProduct() {
            // given:
            final List<Integer> mixedList = asList(-1, -2, 1, 2);
            final Integer expected = 4;
            // when:
            final Integer actual = product(mixedList);
            // then:
            assertThat(actual, is(expected));
        }

        @Test
        public void shouldReturnNegativeForOddNumberOfNegativeValuesProduct() {
            // given:
            final List<Integer> mixedList = asList(-1, 2, 1, 2);
            final Integer expected = -4;
            // when:
            final Integer actual = product(mixedList);
            // then:
            assertThat(actual, is(expected));
        }
    }*/
}
