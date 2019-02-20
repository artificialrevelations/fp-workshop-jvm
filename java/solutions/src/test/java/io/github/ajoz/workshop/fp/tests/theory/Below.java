package io.github.ajoz.workshop.fp.tests.theory;

import org.junit.experimental.theories.ParametersSuppliedBy;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
@ParametersSuppliedBy(BelowSupplier.class)
public @interface Below {
    int value();

    boolean inclusive() default false;

    int limit() default 1000;
}
