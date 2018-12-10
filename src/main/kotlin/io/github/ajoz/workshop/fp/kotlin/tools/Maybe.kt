package io.github.ajoz.workshop.fp.kotlin.tools

sealed class Maybe<A> : Iterable<A> {
    abstract val isSome: Boolean
    val isNone: Boolean
        get() = !isSome

    abstract fun <B> map(function: (A) -> B): Maybe<B>
    abstract fun <B> flatMap(function: (A) -> Maybe<B>): Maybe<B>

    abstract fun getOrElse(default: A): A
    abstract fun get(): A

    abstract fun orElse(default: Maybe<A>): Maybe<A>
    abstract fun orElse(default: () -> Maybe<A>): Maybe<A>

    abstract fun ifSome(effect: (A) -> Unit): Maybe<A>
    abstract fun ifNone(effect: () -> Unit): Maybe<A>

    override fun iterator() =
            if (isSome)
                ValueIterator(get())
            else
                EmptyIterator

    data class Some<A>(private val value: A) : Maybe<A>() {
        override val isSome = true

        override fun <B> map(function: (A) -> B) =
                Some(function(value))

        override fun <B> flatMap(function: (A) -> Maybe<B>) =
                function(value)

        override fun getOrElse(default: A) =
                value

        override fun orElse(default: Maybe<A>) =
                Some(value)

        override fun orElse(default: () -> Maybe<A>) =
                Some(value)

        override fun get() =
                value

        override fun ifSome(effect: (A) -> Unit) =
                Some(value.also(effect))

        override fun ifNone(effect: () -> Unit) =
                Some(value)
    }

    class None<A> : Maybe<A>() {
        override val isSome: Boolean
            get() = false

        override fun <B> map(function: (A) -> B) =
                None<B>()

        override fun <B> flatMap(function: (A) -> Maybe<B>) =
                None<B>()

        override fun getOrElse(default: A) =
                default

        override fun orElse(default: Maybe<A>) =
                default

        override fun orElse(default: () -> Maybe<A>) =
                default()

        override fun get(): A {
            throw IllegalStateException("No value is stored in Nothing!")
        }

        override fun ifSome(effect: (A) -> Unit) =
                None<A>()

        override fun ifNone(effect: () -> Unit) =
                also(effect)
    }

    @Suppress("unused")
    companion object {
        fun <B> ofNullable(value: B?): Maybe<B> =
                if (null != value)
                    Some(value)
                else
                    None()
    }
}
