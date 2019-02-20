package io.github.ajoz.workshop.fp.part3.exercise1;

import org.junit.Assert;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertEquals;

@RunWith(Enclosed.class)
public class EnumBooleanTest {
    public static class True {
        @Test
        public void and_True_equals_True() {
            Assert.assertEquals(EnumBoolean.TRUE, EnumBoolean.TRUE.and(EnumBoolean.TRUE));
        }

        @Test
        public void and_False_equals_False() {
            Assert.assertEquals(EnumBoolean.FALSE, EnumBoolean.TRUE.and(EnumBoolean.FALSE));
        }

        @Test
        public void or_True_equals_True() {
            Assert.assertEquals(EnumBoolean.TRUE, EnumBoolean.TRUE.or(EnumBoolean.TRUE));
        }

        @Test
        public void or_False_equals_True() {
            Assert.assertEquals(EnumBoolean.TRUE, EnumBoolean.TRUE.or(EnumBoolean.FALSE));
        }

        @Test
        public void negated_equals_False() {
            Assert.assertEquals(EnumBoolean.FALSE, EnumBoolean.TRUE.not());
        }
    }

    public static class False {
        @Test
        public void and_True_equals_False() {
            Assert.assertEquals(EnumBoolean.FALSE, EnumBoolean.FALSE.and(EnumBoolean.TRUE));
        }

        @Test
        public void and_False_equals_False() {
            Assert.assertEquals(EnumBoolean.FALSE, EnumBoolean.FALSE.and(EnumBoolean.FALSE));
        }

        @Test
        public void or_True_equals_True() {
            Assert.assertEquals(EnumBoolean.TRUE, EnumBoolean.FALSE.or(EnumBoolean.TRUE));
        }

        @Test
        public void or_False_equals_True() {
            Assert.assertEquals(EnumBoolean.FALSE, EnumBoolean.FALSE.or(EnumBoolean.FALSE));
        }

        @Test
        public void negated_equals_True() {
            Assert.assertEquals(EnumBoolean.TRUE, EnumBoolean.FALSE.not());
        }
    }
}
