package io.github.ajoz.workshop.fp.kotlin.tools

inline fun <T> T.also(block: () -> Unit): T {
    block()
    return this
}