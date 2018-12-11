package io.github.ajoz.workshop.fp.kotlin.tools

sealed class Try<A> : Iterable<A> {
    abstract val isSuccess: Boolean
    val isFailure: Boolean
        get() = !isSuccess

    abstract fun <B> map(function: (A) -> B): Try<B>
    abstract fun <B> flatMap(function: (A) -> Try<B>): Try<B>

    abstract fun recover(function: (Throwable) -> A): Try<A>
    abstract fun recoverWith(function: (Throwable) -> Try<A>): Try<A>

    abstract fun orElse(default: Try<A>): Try<A>
    abstract fun orElse(default: () -> Try<A>): Try<A>

    abstract fun getOrElse(default: A): A
    abstract fun get(): A
    abstract fun getCause(): Throwable

    abstract fun ifSuccess(effect: (A) -> Unit): Try<A>
    abstract fun ifFailure(effect: (Throwable) -> Unit): Try<A>

    override fun iterator() =
            if (isSuccess)
                ValueIterator(get())
            else
                EmptyIterator

    data class Success<A>(val value: A) : Try<A>() {
        override val isSuccess = true

        override fun <B> map(function: (A) -> B) =
                try {
                    Success(function(value))
                } catch (throwable: Throwable) {
                    Failure<B>(throwable)
                }

        override fun <B> flatMap(function: (A) -> Try<B>) =
                try {
                    function(value)
                } catch (throwable: Throwable) {
                    Failure<B>(throwable)
                }


        override fun recover(function: (Throwable) -> A) = this
        override fun recoverWith(function: (Throwable) -> Try<A>) = this

        override fun orElse(default: Try<A>) = this
        override fun orElse(default: () -> Try<A>) = this

        override fun getOrElse(default: A) = value
        override fun get() = value
        override fun getCause(): Throwable =
                throw UnsupportedOperationException("Success is not a failure!")

        override fun ifSuccess(effect: (A) -> Unit) = Success(value.also(effect))
        override fun ifFailure(effect: (Throwable) -> Unit) = this
    }

    data class Failure<A>(val error: Throwable) : Try<A>() {
        override val isSuccess = false

        override fun <B> map(function: (A) -> B) = Failure<B>(error)
        override fun <B> flatMap(function: (A) -> Try<B>) = Failure<B>(error)

        override fun recover(function: (Throwable) -> A) =
                try {
                    Success(function(error))
                } catch (t: Throwable) {
                    this
                }

        override fun recoverWith(function: (Throwable) -> Try<A>) =
                try {
                    function(error)
                } catch (error: Throwable) {
                    Failure<A>(error)
                }

        override fun orElse(default: Try<A>) = default
        override fun orElse(default: () -> Try<A>) =
                try {
                    default()
                } catch (t: Throwable) {
                    this
                }

        override fun getOrElse(default: A) = default
        override fun get() =
                throw RuntimeException(error)

        override fun getCause() = error

        override fun ifSuccess(effect: (A) -> Unit) = this
        override fun ifFailure(effect: (Throwable) -> Unit) =
                Failure<A>(error.also(effect))
    }

    @Suppress("unused")
    companion object {
        fun <A> of(supplier: () -> A) =
                try {
                    Success(supplier())
                } catch (t: Throwable) {
                    Failure<A>(t)
                }

        fun <A> ofNullable(value: A?) =
                if (null != value) {
                    Success(value)
                } else
                    Failure<A>(NullPointerException("Null value passed to ofNullable"))
    }
}

