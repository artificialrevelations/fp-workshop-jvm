package io.github.ajoz.workshop.fp.java.part1.exercise1;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class Exercise1Test {
    @Test
    public void test_f1() {
        assertEquals(42, (int) Exercise1.f1.apply(41));
    }

    @Test
    public void test_f2() {
        assertEquals(4, (int) Exercise1.f2.apply(0));
    }

    @Test
    public void test_f3() {
        assertEquals(0, (int) Exercise1.f3.apply(10));
    }

    @Test
    public void test_f4() {
        assertEquals(6, (int) Exercise1.f4.apply(1));
    }
}
