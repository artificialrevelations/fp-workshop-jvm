package io.github.ajoz.workshop.fp.java.part_1.solutions.practice;

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class PredicateTest {

    @Test
    public void trueAndFalseEqualsFalse() {
        // given:
        final Predicate<String> true1 = string -> true;
        final Predicate<String> false1 = string -> false;
        // when:
        final Predicate<String> result = true1.and(false1);
        // then:
        assertFalse(result.test("testTrueAndFalse"));
    }

    @Test
    public void falseAndTrueEqualsFalse() {
        // given:
        final Predicate<String> false1 = string -> false;
        final Predicate<String> true1 = string -> true;
        // when:
        final Predicate<String> result = false1.and(true1);
        // then:
        assertFalse(result.test("testFalseAndTrue"));
    }

    @Test
    public void falseAndFalseEqualsFalse() {
        // given:
        final Predicate<String> false1 = string -> false;
        final Predicate<String> false2 = string -> false;
        // when:
        final Predicate<String> result = false1.and(false2);
        // then:
        assertFalse(result.test("testFalseAndFalse"));
    }

    @Test
    public void trueOrTrueEqualsTrue() {
        // given:
        final Predicate<String> true1 = string -> true;
        final Predicate<String> true2 = string -> true;
        // when:
        final Predicate<String> result = true1.or(true2);
        // then:
        assertTrue(result.test("testTrueOrTrue"));
    }

    @Test
    public void trueOrFalseEqualsTrue() {
        // given:
        final Predicate<String> true1 = string -> true;
        final Predicate<String> false1 = string -> false;
        // when:
        final Predicate<String> result = true1.or(false1);
        // then:
        assertTrue(result.test("testTrueOrFalse"));
    }

    @Test
    public void falseOrTrueEqualsTrue() {
        // given:
        final Predicate<String> false1 = string -> false;
        final Predicate<String> true1 = string -> true;
        // when:
        final Predicate<String> result = false1.or(true1);
        // then:
        assertTrue(result.test("testFalseOrTrue"));
    }

    @Test
    public void falseOrFalseEqualsFalse() {
        // given:
        final Predicate<String> false1 = string -> false;
        final Predicate<String> false2 = string -> false;
        // when:
        final Predicate<String> result = false1.or(false2);
        // then:
        assertFalse(result.test("testFalseOrFalse"));
    }
}
