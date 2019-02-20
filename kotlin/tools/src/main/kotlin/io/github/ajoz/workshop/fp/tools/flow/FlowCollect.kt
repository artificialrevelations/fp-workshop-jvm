@file:Suppress("unused")

package io.github.ajoz.workshop.fp.tools.flow

import java.util.*
import kotlin.collections.HashSet

/**
 *
 */
fun <A> Flow<A>.toList(): List<A> {
    val list = LinkedList<A>()
    for (item in this) {
        list.add(item)
    }
    return list
}

/**
 *
 */
fun <A> Flow<A>.toSet(): Set<A> {
    val set = HashSet<A>()
    for (item in this) {
        set.add(item)
    }
    return set
}