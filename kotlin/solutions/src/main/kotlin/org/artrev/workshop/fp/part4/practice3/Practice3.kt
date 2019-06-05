import org.artrev.workshop.fp.tools.control.Try
import org.artrev.workshop.fp.tools.flow.Flow
import org.artrev.workshop.fp.tools.flow.of
import org.artrev.workshop.fp.tools.flow.select
import java.util.HashSet
import java.util.NoSuchElementException

internal fun <A, B> Flow<A>.flatMap(mapper: (A) -> Flow<B>): Flow<B> =
        FlatMapFlow(this, mapper)

internal class FlatMapFlow<A, B>(val upstream: Flow<A>,
                                 val mapper: (A) -> Flow<B>) : Flow<B> {
    private var next: Flow<B>? = null

    override fun next(): Try<B> {
        do {
            // if we are already have next Flow then take an element of it
            // and pass it as next to downstream
            val nextDownstreamElement = Try
                    .ofNullable(next)
                    .flatMap { it.next() }

            // if this is a Success we can pass it
            if (nextDownstreamElement.isSuccess) {
                return nextDownstreamElement
            }

            // if this is a Failure this means that either we:
            // - started flatMapping (next is null)
            // - next finished and returned a Failure because there are no more elements in it
            val nextUpstreamElement = upstream.next()

            // if there are no more elements in the Flow then just return a Failure
            if (nextUpstreamElement.isFailure) {
                return Try.Failure(NoSuchElementException("No more elements to flatMap in upstream Flow!"))
            }

            // if there is an element in the upstream Flow then flatMap it and set as nextFlow
            // we need to then put elements from this retrieved Flow into downstream

            next = nextUpstreamElement.map(mapper).get()
        } while (true)
    }
}

internal fun <A> Flow<A>.filter(predicate: (A) -> Boolean): Flow<A> =
        FilterFlow(this, predicate)

internal class FilterFlow<A>(val upstream: Flow<A>,
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

internal fun <A> Flow<A>.peek(action: (A) -> Unit): Flow<A> =
        PeekFlow(this, action)

internal class PeekFlow<A>(val upstream: Flow<A>,
                           private val action: (A) -> Unit) : Flow<A> {

    override fun next(): Try<A> =
    // Try already has ifSuccess method so we do not need to manually
    // check for Try.Failure and Try.Success
            upstream.next().ifSuccess(action)
}

internal fun <A> Flow<A>.distinct(): Flow<A> =
        DistinctFlow(this)

class DistinctFlow<A>(val upstream: Flow<A>) : Flow<A> {
    private val distinctElements: MutableSet<A> = HashSet()

    override fun next(): Try<A> {
        do {
            val next = upstream.next()
            if (next.isFailure) {
                // no more elements in the upstream, we can clear the memory
                distinctElements.clear()
                return Try.Failure(NoSuchElementException("No more elements in distinct Flow!"))
            }

            // if there is an element upstream then check if we can put it
            // in hash set, this is a memory hog, it could use some witty
            // optimization but this is basically the same how tools is
            // doing this for Sequence class
            val a = next.get()
            if (distinctElements.add(a)) {
                return Try.Success(a)
            }
        } while (true)
    }
}

fun main(args: Array<String>) {
    val strings =
            Flow.of(1, 2, 3)
                    .peek { element -> println("#before filter -> $element") }
                    .select { element -> element >= 2 }
                    .peek { element -> println("#before flatMap -> $element") }
                    .flatMap { element -> Flow.of(">$element<", ">$element<") }
                    .peek { element -> println("#before distinct -> $element") }
                    .distinct()
                    .toList()
    println(strings)
}