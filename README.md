# Functional Programming workshop with examples in Java/Kotlin

This is a general plan of Functional Programming Workshop (beginner/intermediate level).
All parts of the workshop are planned to take around 3 hours (2 - 2.5h with 2x 15 min break)

## What is expected:
- intermediate knowledge of Java language
- basic knowledge of Kotlin lang (but not necessary)
- basic understanding of Java generics
- no previous knowledge about lambdas and method references is required

## You won't learn:
- RxJava/RxKotlin/Rx* (because rx is not functional, nor is it functional reactive programming - FRP)
- Java 8 API (Stream, NIO)
- Kotlin API (Sequence, Iterable extensions)
- Vavr library (although we will discuss and create several tools that the library has to offer)
- Arrow library (the same as in case of Vavr)
- Haskell or any ML variant (although we will look at some examples to compare with Java/Kotlin implementations)

## What is needed:
- at least Open JDK for Java 8 (JDK)
- latest version of Kotlin (1.3)
- Android Studio or Intelij Idea
- latest version of Kotlin plugin
- gradle installed
- GIT (repository with the workshop examples is stored on github)

## (Part 1) Functions and composition
- What is Functional Programming?
- Comparing Functional Programming with Procedural Programming
- Comparing Functional Programming with Object Oriented Programming
- What is Imperative style?
- What is Declarative style?
- What is a function? Or maybe what is it not?
- What is referential transparency? and why is it helpful?
- What is a Pure and Impure function? and why should we care?
- A brief look at side effects, what are intentional and nonintentional side effects?
- What is a Total and Partial function?
- What is currying and partial application?
- quick look at Kotlin (only in regards to the material)
- quick look at Haskell (just to understand the possibilities and comparing this with Java)

## (Part 2) Higher Order Functions
- Everything is a Set? Can whole math be built from the concept of a Set in an empty universe?
- Everything is a function? Can whole math be built from the concept of a Function in an empty universe?
- Function as a value? Why we have problems with adjusting to this idea?
- A quick glance at Lambda calculus
- What are higher order functions? How can we use them effectively?
- what benefits we get from abstracting simple looping?
- what issues does Java create when working with higher order functions?
- is Kotlin solving anything in this department?
- why higher order functions are importantant? (quick look into Haskell)
- does order of function arguments have a meaning? (point free notation), why OO is messing this up?

## (Part 3) Algebraic Data Types and expressing optionality
- What are Sum Types? How can they be expressed in Java/Kotlin?
- What are Product Types? Why are they a bit troublesome to work with?
- Combining Sum Types and Product Types into a single Type
- Are Enums a kind of a Sum Type?
- What is pattern matching?
- What is structural pattern matching?
- Can we work with ADTs without pattern matching?
- Are Algebraic Data Types contradicting OO (encapsulation)?
- Are ADTs the same as Anemic Domain Model? (is it an OO anti-pattern?)
- Can sum type/product type syntax be better and less verbose?
- How can ADTs be used to express optionality (for partial functions)?
- Creating a `Maybe<A>` type
- Other useful ADTs: `Try`, `Either`
- Beyond `Maybe`, `Try` and `Either`. basic modeling of the domain with our custom algebras

## (Part 4) Recursion, Lazyness and Streams
- Recursion? Is there something like a "stack safe" recursion?
- Recursion vs corecursion
- Tail Call Optimization or Tail Call Elimination (or why Haskell, F#, Scala do not blow the stack)
- Trampolines as a way of simulating stack safe recursion
- Memoization as a way of trading execution time for memory
- Immutability? Can we write useful immutable code? 
- A case of fully immutable State machine?
- Lazy initialization vs lazy processing
- Types of processing (PUSH vs PULL based)
- Creating a lazy "stream-like" data structure, is it doable in Java?
- A quick dip into the design of Java 8 Stream and Kotlin's Sequence

## (Part 5) Functional architecture or putting everything to use
- how do OO design patterns translate to Functional Programming?
- Stringly-types vs Strongly-typed (one letter but great difference)
- making illegal state unrepresentable!
- designing for errors, keeping them as a part of your domain
- how to mix pure and impure code?
- hexagonal architecture (ports and adapters) and why its natural in FP (at least in Haskell)?
- two types of pushing responsibility with types: backward (caller handles inputs) and foreward (caller handles outputs)
  

