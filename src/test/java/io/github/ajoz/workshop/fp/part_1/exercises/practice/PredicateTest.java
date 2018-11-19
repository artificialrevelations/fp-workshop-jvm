package io.github.ajoz.workshop.fp.part_1.exercises.practice;


import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class PredicateTest {

    @Test
    public void trueFooFalseEqualsFalse() {
        // given:
        final Predicate<String> true1 = string -> true;
        final Predicate<String> false1 = string -> false;
        // when:
        final Predicate<String> result = true1.foo(false1);
        // then:
        assertFalse(result.test("testTrueFooFalse"));
    }

    @Test
    public void falseFooTrueEqualsFalse() {
        // given:
        final Predicate<String> false1 = string -> false;
        final Predicate<String> true1 = string -> true;
        // when:
        final Predicate<String> result = false1.foo(true1);
        // then:
        assertFalse(result.test("testFalseFooTrue"));
    }

    @Test
    public void falseFooFalseEqualsFalse() {
        // given:
        final Predicate<String> false1 = string -> false;
        final Predicate<String> false2 = string -> false;
        // when:
        final Predicate<String> result = false1.foo(false2);
        // then:
        assertFalse(result.test("testFalseFooFalse"));
    }

    @Test
    public void trueBarTrueEqualsTrue() {
        // given:
        final Predicate<String> true1 = string -> true;
        final Predicate<String> true2 = string -> true;
        // when:
        final Predicate<String> result = true1.bar(true2);
        // then:
        assertTrue(result.test("testTrueBarTrue"));
    }

    @Test
    public void trueBarFalseEqualsTrue() {
        // given:
        final Predicate<String> true1 = string -> true;
        final Predicate<String> false1 = string -> false;
        // when:
        final Predicate<String> result = true1.bar(false1);
        // then:
        assertTrue(result.test("testTrueBarFalse"));
    }

    @Test
    public void falseBarTrueEqualsTrue() {
        // given:
        final Predicate<String> false1 = string -> false;
        final Predicate<String> true1 = string -> true;
        // when:
        final Predicate<String> result = false1.bar(true1);
        // then:
        assertTrue(result.test("testFalseBarTrue"));
    }

    @Test
    public void falseBarFalseEqualsFalse() {
        // given:
        final Predicate<String> false1 = string -> false;
        final Predicate<String> false2 = string -> false;
        // when:
        final Predicate<String> result = false1.bar(false2);
        // then:
        assertFalse(result.test("testFalseBarFalse"));
    }
}

