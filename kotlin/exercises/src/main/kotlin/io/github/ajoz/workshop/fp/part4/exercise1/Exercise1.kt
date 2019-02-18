@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part4.exercise1

import java.io.File
import java.io.IOException

/*
  -- Laziness --

  "I've heard that hard work never killed anyone,
  but I say why take the chance?" - Ronald Regan

  Laziness is a big part of functional programming, some functional languages
  like Haskell were built around it, some like Scala have tools that allow
  for a certain amount of laziness.

  What about Kotlin?

  Kotlin is a programming language with eager evaluation. This kind of evaluation
  is called "strict" or "greedy". Eagerness means that an expression is evaluated
  immediately as it is bound to a variable.

  Not all is lost because Kotlin has "short-circuit evaluation". It's used with
  boolean expressions:

  // imagine it has a proper constructor and everything ...
  class Foo {
      boolean hasBar() {}
  }

  foo: Foo = ... // let's imagine this expression could return null

  if (foo != null && foo.hasBar()) {

  }

  Thanks to the "short-circuit evaluation" we can do the condition shown above.
  If the `foo` variable is `null`, then the `hasBar()` method won't be called.

  Useful? What if we could do more things like this?

  Imagine you are working for a navigation company called "THERE" and you are
  currently working on your flagship Android application called "You Go!". The
  "There You Go!" app is increasing in popularity comparing to the competitor
  "Moogle Gaps" :-)

  You and your team are currently implementing a revolutionary feature, which is
  displaying information about accidents on map. This never heard before feature
  involves storing accident information in the file system.

  You know pretty well that in your city Mapsterdam, there are a lot of accidents
  on a daily basis, storing all this information in the memory would be a large
  overkill. You and your team decide to store only accident metadata:
  - an accident id
  - path to the file where the data is stored

  It is the first iteration of the work on the new API, below is the code you
  came up with.

  Before the AccidentMetadata class can be used it needs to be created:

  val am: AccidentMetadata = AccidentMetadata(42L, file)

  and initiated:

  am.loadAccident();

  Of course this `loadAccident()` needs to be called only when we want to retrieve
  the accident data.

  Questions:
  - do you like this implementation?
  - does this implementation have any problems?
  - is this a correct Object-Oriented implementation?
 */
internal class AccidentMetadata(var id: Long, var source: File) {
    // this could be done also by lazy delegate
    var data: String? = null
        private set

    fun loadAccident() {
        data = try {
            source.readLines().reduce { a, b -> a + b }
        } catch (e: IOException) {
            throw SourceFileCorrupted("Error when reading: ${source.absolutePath}")
        }
    }

    // an exception that can only happen in the context of the AccidentMetadata
    class SourceFileCorrupted internal constructor(message: String) : RuntimeException(message)
}

/*
  The implementation is not very Object Oriented. Why not? One rule of OO design
  is when an object is created then it should be immediately ready to be used.

  Here it is not the case, we have a half baked object that can return `null` if
  the `data` property is used before `loadAccident` is used.

  There is also a problem of accidental call to `loadAccident` (pun intended),
  before it is really needed or after it was already called.

  What can we do to avoid it?
  - what kind of rewrite do we have to make?
  - is it possible to make the `loadAccidents` method private?
  - are there any issues with the code?
 */

/*
  Part 1:

  Please modify the implementation of the class `AccidentMetadata`:
  - make all the fields private
  - make the `loadAccident` method private
  - rework the `loadAccident` method so its possible to use it with a supplier
    `() -> A`, do not worry about the exception thrown from the `loadAccident`.
    We do not have to wrap it in Maybe or Try. Just focus on wrapping accident
    loading into the Supplier
  - use the newly created supplier `() -> A`

  Hints:
  - do we need `data` field at all?
  - what should `data()` property do?

  Questions:
  - did we achieve the laziness?
  - are there any problems with the implementation?
 */

fun main(args: Array<String>) {
    // configuration:
    val file = File("src/main/resources/part4/accident.info")

    // class creation:
    val metadata = AccidentMetadata(42L, file)

    // class instantiation:
    metadata.loadAccident()

    // property usage
    metadata.data?.let(::println)
}
