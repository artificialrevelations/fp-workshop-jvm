package io.github.ajoz.workshop.fp.java.tests.theory;

import org.junit.experimental.theories.ParameterSignature;
import org.junit.experimental.theories.ParameterSupplier;
import org.junit.experimental.theories.PotentialAssignment;

import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

public final class BelowSupplier extends ParameterSupplier {
    @Override
    public List<PotentialAssignment> getValueSources(final ParameterSignature sig) throws Throwable {
        final Below below = sig.getAnnotation(Below.class);
        return new Random().ints(
                below.limit(),
                Integer.MIN_VALUE,
                below.inclusive()
                        ? below.value() + 1
                        : below.value()
        )
                .boxed()
                .map(i -> PotentialAssignment.forValue("ints", i))
                .collect(Collectors.toList());
    }
}
