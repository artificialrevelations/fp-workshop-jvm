package io.github.ajoz.workshop.fp.tests.theory;

import org.junit.experimental.theories.ParameterSignature;
import org.junit.experimental.theories.ParameterSupplier;
import org.junit.experimental.theories.PotentialAssignment;

import java.util.ArrayList;
import java.util.List;

public final class BetweenSupplier extends ParameterSupplier {
    @Override
    public List<PotentialAssignment> getValueSources(final ParameterSignature sig) throws Throwable {
        final List<PotentialAssignment> list = new ArrayList<>();
        final Between annotation = sig.getAnnotation(Between.class);

        for (int i = annotation.first(); i <= annotation.last(); i++)
            list.add(PotentialAssignment.forValue("ints", i));
        return list;
    }
}
