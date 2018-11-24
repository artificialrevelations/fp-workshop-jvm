@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_2.solutions.exercise_1

import java.util.Arrays.asList
import java.util.Collections.emptyList
import org.hamcrest.CoreMatchers.`is`
import org.junit.Assert.assertThat
import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

@RunWith(Enclosed::class)
class Exercise1Test {
    class SumTest {

        @Test
        fun shouldReturnPositiveValueForSinglePositiveItemListSum() {
            // given:
            val list = listOf(128)
            val expected = 128

            // when:
            val actual = sum(list)

            // then:
            assertThat(actual, `is`(expected))
        }

        @Test
        fun shouldReturnNegativeValueForSingleNegativeItemListSum() {
            // given:
            val list = listOf(-256)
            val expected = -256

            // when:
            val actual = sum(list)

            // then:
            assertThat(actual, `is`(expected))
        }

        @Test
        fun shouldReturnZeroForEmptyListSum() {
            // given:
            val emptyList = emptyList<Int>()
            val expected = 0
            // when:
            val actual = sum(emptyList)
            // then:
            assertThat(actual, `is`(expected))
        }

        @Test
        fun shouldReturnNegativeValueSum() {
            // given:
            val negativeList = asList(-1, -1, -1, -1)
            val expected = -4
            // when:
            val actual = sum(negativeList)
            // then:
            assertThat(actual, `is`(expected))
        }

        @Test
        fun shouldReturnPositiveValueSum() {
            // given:
            val positiveList = asList(1, 1, 1, 1)
            val expected = 4
            // when:
            val actual = sum(positiveList)
            // then:
            assertThat(actual, `is`(expected))
        }

        @Test
        fun shouldReturnZeroForMixedValuesSum() {
            // given:
            val mixedList = asList(-1, -2, 1, 2)
            val expected = 0
            // when:
            val actual = sum(mixedList)
            // then:
            assertThat(actual, `is`(expected))
        }

        @Test
        fun shouldReturnPositiveValueForIncrementalValuesSum() {
            // given:
            val incrementalList = asList(1, 2, 3)
            val expected = 6
            // when:
            val actual = sum(incrementalList)
            // then:
            assertThat(actual, `is`(expected))
        }

        @Test
        fun shouldReturnZeroForListWithAllZerosSum() {
            // given:
            val list = asList(0, 0, 0, 0, 0, 0)
            val expected = 0
            // when:
            val actual = sum(list)
            // then:
            assertThat(actual, `is`(expected))
        }
    }

    class ProductTest {

        @Test
        fun shouldReturnOneForEmptyListProduct() {
            // given:
            val emptyList = emptyList<Int>()
            val expected = 1
            // when:
            val actual = product(emptyList)
            // then:
            assertThat(actual, `is`(expected))
        }

        @Test
        fun shouldReturnPositiveForPositiveListProduct() {
            // given:
            val positiveList = asList(1, 2, 3)
            val expected = 6
            // when:
            val actual = product(positiveList)
            // then:
            assertThat(actual, `is`(expected))
        }

        @Test
        fun shouldReturnPositiveForEvenNumberOfNegativeValuesProduct() {
            // given:
            val mixedList = asList(-1, -2, 1, 2)
            val expected = 4
            // when:
            val actual = product(mixedList)
            // then:
            assertThat(actual, `is`(expected))
        }

        @Test
        fun shouldReturnNegativeForOddNumberOfNegativeValuesProduct() {
            // given:
            val mixedList = asList(-1, 2, 1, 2)
            val expected = -4
            // when:
            val actual = product(mixedList)
            // then:
            assertThat(actual, `is`(expected))
        }
    }
}