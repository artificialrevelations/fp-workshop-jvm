package io.github.ajoz.workshop.fp.java.tests.theory;

import org.junit.experimental.theories.ParameterSignature;
import org.junit.experimental.theories.ParameterSupplier;
import org.junit.experimental.theories.PotentialAssignment;

import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

public final class AboveSupplier extends ParameterSupplier {
    @Override
    public List<PotentialAssignment> getValueSources(final ParameterSignature sig) throws Throwable {
        final Above above = sig.getAnnotation(Above.class);
        return new Random().ints(
                above.limit(),
                above.inclusive()
                        ? above.value() - 1
                        : above.value(),
                Integer.MAX_VALUE
        )
                .boxed()
                .map(i -> PotentialAssignment.forValue("ints", i))
                .collect(Collectors.toList());
    }
}
