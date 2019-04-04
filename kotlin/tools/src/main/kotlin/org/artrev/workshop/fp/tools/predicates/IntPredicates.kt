package org.artrev.workshop.fp.tools.predicates

fun isLargerThen(value: Int): (Int) -> Boolean = { it > value }

fun isLowerThen(value: Int): (Int) -> Boolean = { it < value }