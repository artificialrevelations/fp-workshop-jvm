@file:Suppress("unused")

package io.github.ajoz.workshop.fp.tools.flow

import io.github.ajoz.workshop.fp.tools.control.Try
import io.github.ajoz.workshop.fp.tools.not

/**
 * Returns a [Flow] that only passes downstream elements that satisfy the given
 * condition.
 *
 * If you want to select only elements that do not satisfy the condition please
 * use [Flow.reject] instead.
 *
 * @param A the type of the returned flow.
 * @param[condition] a predicate that needs to be satisfied by the element of
 * the Flow to be passed downstream.
 * @return instance of a Flow.
 */
fun <A> Flow<A>.select(condition: (A) -> Boolean): Flow<A> {
    class FilterFlow(val upstream: Flow<A>,
                     val predicate: (A) -> Boolean) : Flow<A> {
        override fun next(): Try<A> {
            do {
                // take the next item from upstream
                val next = upstream.next()
                // if there is no more items upstream, then just return a failure
                if (next.isFailure)
                    return next

                // if there is an item upstream, then check if satisfies the
                // given predicate and if it does then return it
                val filtered = next.filter(predicate)
                if (filtered.isSuccess)
                    return filtered
                // if the item upstream does not satisfy the predicate then
                // try again
            } while (true)
        }
    }

    return FilterFlow(this, condition)
}

/**
 * Returns a [Flow] that only passes downstream elements that satisfy the given
 * condition.
 *
 * This method is deprecated, please use [Flow.select] instead.
 *
 * If you want to select only elements that do not satisfy the condition please
 * use [Flow.reject] instead.
 *
 * @param A the type of the returned flow.
 * @param[condition] a predicate that needs to be satisfied by the element of
 * the Flow to be passed downstream.
 * @return instance of a Flow.
 */
@Deprecated(
        message = "The name \"filter\" is too ambiguous! Use \"select\" or \"reject\" instead!",
        replaceWith = ReplaceWith("select(condition)"),
        level = DeprecationLevel.WARNING
)
fun <A> Flow<A>.filter(condition: (A) -> Boolean): Flow<A> =
        select(condition)

/**
 * Returns a [Flow] that only passes downstream elements that do NOT satisfy the
 * given condition.
 *
 * If you want to select only elements that do satisfy the condition please
 * use [Flow.select] instead.
 *
 * @param A the type of the returned flow.
 * @param[condition] a predicate that needs to be satisfied by the element of
 * the Flow to be passed downstream.
 * @return instance of a Flow.
 */
fun <A> Flow<A>.reject(condition: (A) -> Boolean): Flow<A> =
        select(condition.not())


