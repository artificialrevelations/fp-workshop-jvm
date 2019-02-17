package io.github.ajoz.workshop.fp.kotlin.tests.theory

import org.junit.experimental.theories.ParameterSignature
import org.junit.experimental.theories.ParameterSupplier
import org.junit.experimental.theories.PotentialAssignment
import java.util.Random
import java.util.stream.Collectors

class BelowSupplier : ParameterSupplier() {
    @Throws(Throwable::class)
    override fun getValueSources(sig: ParameterSignature): List<PotentialAssignment> {
        val below: Below = sig.getAnnotation(Below::class.java)
        return Random().ints(
                below.limit.toLong(),
                Integer.MIN_VALUE,
                if (below.inclusive)
                    below.value + 1
                else
                    below.value
        )
                .boxed()
                .map { i -> PotentialAssignment.forValue("ints", i) }
                .collect(Collectors.toList())
    }
}
