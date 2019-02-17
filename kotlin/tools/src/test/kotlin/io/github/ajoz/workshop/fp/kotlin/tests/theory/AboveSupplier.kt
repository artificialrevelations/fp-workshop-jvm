package io.github.ajoz.workshop.fp.kotlin.tests.theory

import org.junit.experimental.theories.ParameterSignature
import org.junit.experimental.theories.ParameterSupplier
import org.junit.experimental.theories.PotentialAssignment
import java.util.Random
import java.util.stream.Collectors

class AboveSupplier : ParameterSupplier() {
    @Throws(Throwable::class)
    override fun getValueSources(sig: ParameterSignature): List<PotentialAssignment> {
        val above: Above = sig.getAnnotation(Above::class.java)
        return Random().ints(
                above.limit.toLong(),
                if (above.inclusive)
                    above.value - 1
                else
                    above.value,
                Integer.MAX_VALUE
        )
                .boxed()
                .map { i -> PotentialAssignment.forValue("ints", i) }
                .collect(Collectors.toList())
    }
}
