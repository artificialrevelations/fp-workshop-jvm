package io.github.ajoz.workshop.fp.tests.theory

import org.junit.experimental.theories.ParametersSuppliedBy

@Retention(AnnotationRetention.RUNTIME)
@ParametersSuppliedBy(AboveSupplier::class)
annotation class Above(
        val value: Int,
        val inclusive: Boolean = false,
        val limit: Int = 1000
)
