package io.github.ajoz.workshop.fp.kotlin.tests.theory

import org.junit.experimental.theories.ParameterSignature
import org.junit.experimental.theories.ParameterSupplier
import org.junit.experimental.theories.PotentialAssignment

class BetweenSupplier : ParameterSupplier() {
    @Throws(Throwable::class)
    override fun getValueSources(sig: ParameterSignature): List<PotentialAssignment> {
        val annotation: Between = sig.getAnnotation(Between::class.java)

        return (annotation.first..annotation.last)
                .map { PotentialAssignment.forValue("ints", it) }
    }
}
