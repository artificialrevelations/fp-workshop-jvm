package io.github.ajoz.workshop.fp.kotlin.part_1.exercises.exercise_7

import org.junit.Test

import org.junit.Assert.assertEquals

class Exercise7Test {
    @Test
    fun should_generate_hash_for_customer() {
        // given:
        val customers = listOf(
                Customer("JUG Łódź!"),
                Customer("http://www.juglodz.pl/"),
                Customer("https://www.facebook.com/groups/juglodz/"),
                Customer("https://groups.google.com/d/forum/lodz-jug"),
                Customer("https://www.meetup.com/Java-User-Group-Lodz/")
        )

        // when:
        val tested = Exercise7.getCustomerOrderHash()

        // then:
        for (customer in customers) {
            val expected = Hash(
                    String.format("FP Workshop - %s", customer.name).length.toLong()
            )
            val actual = tested(customer)
            assertEquals(expected, actual)
        }
    }
}
