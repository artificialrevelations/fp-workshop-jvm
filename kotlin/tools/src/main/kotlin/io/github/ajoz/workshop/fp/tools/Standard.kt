package io.github.ajoz.workshop.fp.tools

inline fun <T> T.also(block: () -> Unit): T {
    block()
    return this
}