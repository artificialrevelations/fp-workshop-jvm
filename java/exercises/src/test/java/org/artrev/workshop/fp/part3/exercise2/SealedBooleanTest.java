package org.artrev.workshop.fp.part3.exercise2;

import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertEquals;

@RunWith(Enclosed.class)
public class SealedBooleanTest {
    public static class True {
        @Test
        public void and_True_equals_True() {
            assertEquals(SealedBoolean.TRUE, SealedBoolean.TRUE.and(SealedBoolean.TRUE));
        }

        @Test
        public void and_False_equals_False() {
            assertEquals(SealedBoolean.FALSE, SealedBoolean.TRUE.and(SealedBoolean.FALSE));
        }

        @Test
        public void or_True_equals_True() {
            assertEquals(SealedBoolean.TRUE, SealedBoolean.TRUE.or(SealedBoolean.TRUE));
        }

        @Test
        public void or_False_equals_True() {
            assertEquals(SealedBoolean.TRUE, SealedBoolean.TRUE.or(SealedBoolean.FALSE));
        }

        @Test
        public void negated_equals_False() {
            assertEquals(SealedBoolean.FALSE, SealedBoolean.TRUE.not());
        }
    }

    public static class False {
        @Test
        public void and_True_equals_False() {
            assertEquals(SealedBoolean.FALSE, SealedBoolean.FALSE.and(SealedBoolean.TRUE));
        }

        @Test
        public void and_False_equals_False() {
            assertEquals(SealedBoolean.FALSE, SealedBoolean.FALSE.and(SealedBoolean.FALSE));
        }

        @Test
        public void or_True_equals_True() {
            assertEquals(SealedBoolean.TRUE, SealedBoolean.FALSE.or(SealedBoolean.TRUE));
        }

        @Test
        public void or_False_equals_True() {
            assertEquals(SealedBoolean.FALSE, SealedBoolean.FALSE.or(SealedBoolean.FALSE));
        }

        @Test
        public void negated_equals_True() {
            assertEquals(SealedBoolean.TRUE, SealedBoolean.FALSE.not());
        }
    }
}
