package org.artrev.workshop.fp.tools

import org.artrev.workshop.fp.tools.control.Maybe
import org.artrev.workshop.fp.tools.control.Try
import java.util.concurrent.atomic.AtomicReference

fun <A> (() -> A).memoized(): () -> A {
    val memo: AtomicReference<A> = AtomicReference()
    return {
        synchronized(memo) {
            if (memo.get() == null) memo.set(invoke())
            memo.get()
        }
    }
}

fun <A, B> (() -> A).andThen(function: (A) -> B): () -> B =
        map(function)

fun <A, B> (() -> A).map(function: (A) -> B): () -> B = {
    function(this())
}

fun <A, B> (() -> A).flatMap(function: (A) -> () -> B): () -> B = {
    function(this())()
}

fun <A> (() -> A).before(effect: () -> Unit): () -> A = {
    effect()
    this()
}

fun <A> (() -> A).after(effect: (A) -> Unit): () -> A = {
    val result = this()
    effect(result)
    result
}

fun <A> (() -> A).tryGet(): Try<A> =
        Try.of(this)

fun <A> (() -> A).maybeGet(): Maybe<A> = try {
    Maybe.Some(invoke())
} catch (exception: Exception) {
    Maybe.None()
}