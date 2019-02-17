package io.github.ajoz.workshop.fp.kotlin.tests.theory

import org.junit.experimental.theories.ParametersSuppliedBy

@Retention(AnnotationRetention.RUNTIME)
@ParametersSuppliedBy(BetweenSupplier::class)
annotation class Between(
        val first: Int,
        val last: Int
)
