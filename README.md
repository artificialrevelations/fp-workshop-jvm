# Beginner/Intermediate Functional Programming Workshop with examples in Java/Kotlin

This repository contains materials (code examples, exercises and solutions, wiki with commentary and explanations) for the Functional Programming Workshop. The main idea behind the exercises is to teach how certain Functional Programming idioms can be implemented on the JVM (in Java/Kotlin), this means implementing things like functions, composition, control structures etc.

Instead of learning some particular API the participants will write the API from scratch as they go, learning the concepts, names, patterns. This is why the workshop does not focus on the existing API (like Function from Java 8 stdlib) or popular libraries (like [Vavr](http://www.vavr.io/) or [Arrow](https://arrow-kt.io/)).

The terms, names of functions/methods and types are chosen to be maximaly familiar regardless of the library or programming language used. The idea is that after this workshop it should be easy to jump right into a project with FP libraries like Vavr and (to some extent) Arrow.

Each part of the workshop is planned to take around 3 hours, this means all 5 parts should take 15 hours, plus the time needed for practice exercises.

## What is expected:
- intermediate knowledge of Java language
- intermediate knowledge of Kotlin lang
- basic understanding of the concept of [Generics](https://docs.oracle.com/javase/tutorial/java/generics/)
- no previous knowledge about lambdas and method references is required (but could speed things up)

## You won't learn:
- RxJava/RxKotlin/Rx* (because rx is not functional, nor is it functional reactive programming - FRP)
- Java 8 API (Stream, NIO - but we will build Stream-like types)
- Kotlin API (Sequence, Iterable extensions)
- Vavr library (although we will discuss and create several tools that the library has to offer)
- Arrow library (the same as in case of Vavr)
- Haskell or any ML variant (although we will look at some examples to compare with Java/Kotlin implementations)

## What is needed:
- at least Open JDK for Java 8 (JDK)
- latest version of Kotlin (1.3)
- any IDE or editor capable of importing a Gradle project
- latest version of Kotlin plugin if using Intelij IDEA
- Gradle
- GIT (repository with the workshop examples is stored on github)

## Workshop plan:

For a detailed workshop plan and a description of all parts please check out the [wiki](https://github.com/ajoz/fp-workshop-jvm/wiki).
