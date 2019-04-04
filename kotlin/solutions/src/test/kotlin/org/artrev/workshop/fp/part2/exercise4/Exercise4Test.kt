@file:Suppress("PackageName")

package org.artrev.workshop.fp.part2.exercise4

import org.junit.Test

import java.util.Arrays.asList
import java.util.Collections.emptyList
import org.hamcrest.CoreMatchers.`is`
import org.junit.Assert.*

class Exercise4Test {
    @Test
    fun shouldNotContainAValueOnAnEmptyList() {
        // given:
        val emptyList = emptyList<Int>()
        val expected = false
        // when:
        val actual = contains(emptyList, 1)
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldNotContainAValue() {
        // given:
        val list = asList(2, 3, 4)
        val expected = false
        // when:
        val actual = contains(list, 1)
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldContainAValue() {
        // given:
        val stringList = asList("test", "of", "a", "function")
        val expected = true
        // when:
        val actual = contains(stringList, "test")
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldCalculateAverage() {
        // given:
        val list = asList(1, 2, 3)
        val expected = 2
        // when:
        val actual = average(list)
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldFindLastElement() {
        // given:
        val list = asList(0, 1, 2, 3, 4, 5, 6)
        val expected = 6
        // when:
        val actual = last(list)
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldJoinAList() {
        // given:
        val list = asList(true, false, false, true)
        val expected = "true:false:false:true"
        // when:
        val actual = join(list, ":")
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldJoinAnEmptyList() {
        val list = emptyList<Boolean>()
        val expected = ""
        // when:
        val actual = join(list, "::")
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldJoinOneElementList() {
        // given:
        val list = listOf(1)
        val expected = "1"
        // when:
        val actual = join(list, "_")
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldCountListElements() {
        // given:
        val list = asList(1, 2, 5, 6)
        val expected = 4
        // when:
        val actual = count(list)
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldCountEmptyList() {
        // given:
        val list = emptyList<String>()
        val expected = 0
        // when:
        val actual = count(list)
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldReverseAnEmptyList() {
        // given:
        val list = emptyList<String>()
        // when:
        val actual = reverse(list)
        // then:
        assertTrue(actual.isEmpty())
    }

    @Test
    fun shouldReverseAOneElementList() {
        // given:
        val list = listOf("Test")
        val expected = listOf("Test")
        // when:
        val actual = reverse(list)
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldReverseAList() {
        // given:
        val list = asList(1, 2, 3, 4)
        val expected = asList(4, 3, 2, 1)
        // when:
        val actual = reverse(list)
        // then:
        assertThat(actual, `is`(expected))
    }

    @Test
    fun shouldFindMaximum() {
        // given:
        val list = asList(1, 2, 3, 4)
        val expected = 4

        // when:
        val actual = maximum(list)

        // then:
        assertEquals(expected, actual)
    }

    @Test
    fun shouldFindMinimum() {
        // given:
        val list = asList(1, 2, 3, 4)
        val expected = 1

        // when:
        val actual = minimum(list)

        // then:
        assertEquals(expected, actual)
    }

    @Test
    fun shouldFindPenultimateElement() {
        // given:
        val list = asList(1, 2, 3, 4)
        val expected = 3

        // when:
        val actual = penultimate(list)

        // then:
        assertEquals(expected, actual)
    }
}