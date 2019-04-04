@file:Suppress("unused")

package org.artrev.workshop.fp.tools.flow

import io.github.ajoz.workshop.fp.tools.control.Try

/**
 *
 */
fun <A> Flow.Companion.generate(initial: A, function: (A) -> A): Flow<A> {
    class GeneratorFlow(seed: A, val generator: (A) -> A) : Flow<A> {
        var generated: Try<A> = Try.Success(seed)

        override fun next(): Try<A> {
            generated = generated.map(generator)
            return generated
        }
    }

    return GeneratorFlow(initial, function)
}
