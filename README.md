# Beginner/Intermediate Functional Programming Workshop with examples in Java/Kotlin

This repository contains materials (code examples, exercises and solutions, wiki with commentary and explanations) for the Functional Programming Workshop. The main idea behind the exercises is to teach how certain Functional Programming idioms can be implemented on the JVM (in Java/Kotlin), this means implementing things like functions, composition, control structures etc.

Instead of learning some particular API the participants will write the API from scratch as they go, learning the concepts, names, patterns. This is why the workshop does not focus on the existing API (like Function from Java 8 stdlib) or popular libraries (like Vavr or Arrow).

The terms, names of functions/methods and types are chosen to be maximaly familiar regardless of the library or programming language used. After workshop even if using other libraries you should be familiar with the concepts.

All parts of the workshop are planned to take around 3 hours (2 - 2.5h with 2x 15 min break) not counting the practice exercises.

## What is expected:
- intermediate knowledge of Java language
- basic knowledge of Kotlin lang (but not necessary)
- basic understanding of Java generics
- no previous knowledge about lambdas and method references is required

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
- Android Studio or Intelij Idea
- latest version of Kotlin plugin
- gradle installed
- GIT (repository with the workshop examples is stored on github)

## Workshop plan:

For a detailed workshop plan and a description of all parts please check out the [wiki](https://github.com/ajoz/fp-workshop-jvm/wiki).
