package io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_2;

import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_2.SealedBoolean.FALSE;
import static io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_2.SealedBoolean.TRUE;
import static org.junit.Assert.assertEquals;

@RunWith(Enclosed.class)
public class SealedBooleanTest {
    public static class True {
        @Test
        public void and_True_equals_True() {
            assertEquals(TRUE, TRUE.and(TRUE));
        }

        @Test
        public void and_False_equals_False() {
            assertEquals(FALSE, TRUE.and(FALSE));
        }

        @Test
        public void or_True_equals_True() {
            assertEquals(TRUE, TRUE.or(TRUE));
        }

        @Test
        public void or_False_equals_True() {
            assertEquals(TRUE, TRUE.or(FALSE));
        }

        @Test
        public void negated_equals_False() {
            assertEquals(FALSE, TRUE.not());
        }
    }

    public static class False {
        @Test
        public void and_True_equals_False() {
            assertEquals(FALSE, FALSE.and(TRUE));
        }

        @Test
        public void and_False_equals_False() {
            assertEquals(FALSE, FALSE.and(FALSE));
        }

        @Test
        public void or_True_equals_True() {
            assertEquals(TRUE, FALSE.or(TRUE));
        }

        @Test
        public void or_False_equals_True() {
            assertEquals(FALSE, FALSE.or(FALSE));
        }

        @Test
        public void negated_equals_True() {
            assertEquals(TRUE, FALSE.not());
        }
    }
}
