@file:Suppress("PackageName", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_3

/*
  -- Beyond Sum Types --

  In the previous exercises we created a Sum type. There is another kind of type
  out there called a Product type.

  Product type is a compound type that has a structure. The structure of the type
  is determined by the order of the operands (other types) in the product. Instance
  of a product type contains all possible instances of its primitive data types.

  A simple example of a product type is a Pair<A, B>. A Pair denotes a combination
  of all possible instances of A and B.

  Pair = A * B

  In Kotlin one might say that every class is a possible product type if you squint
  your eyes a little.

  class User {
      String name;
      String surname;
      Integer age;
  }

  is just

  User = String * String * Integer

  Just by looking at the used notation (ML-like) you can understand why its called
  a product type.

  Types extending both Sum and Product types are called Algebraic Data Types.

  Now let's create an example.

  Imagine you are working for a package delivery company on their management
  system. In your API you have a class called PackageDeliveryStatus, this
  class describes the current status of a particular package:
  - its id
  - its tracking code (this is only valid for packages that are already dispatched)
  - its stage of processing
  - its cause of rejection (this is only valid for packages that are rejected,
    using an enum for it is causing us to create a silly RejectionCause.NO_CAUSE
    if we do not want to use a null)

  It is not that bad? Yes because we are used to this kind of classes. There are
  some unfortunate problems lurking in such code:
  - incorrect states are possible e.g. rejected package with a tracking code
  - we are not relaying on type system to prevent bugs
 */
data class PackageDeliveryStatus(val id: Long?,
                                 val trackingCode: String?,
                                 val stage: Stage?,
                                 val rejectionCause: RejectionCause = RejectionCause.NO_CAUSE) {
    enum class Stage {
        PREPARING,
        PREPARED,
        REJECTED,
        DISPATCHED,
        WAITING_FOR_RECIPIENT_TO_LEAVE_HOME_SO_HE_WILL_MISS_IT
    }

    enum class RejectionCause {
        NO_CAUSE,
        RECIPIENT_UNKNOWN,
        PACKAGE_DAMAGED
    }
}

/*
  We can try to solve this with ADTs (Product and Sum Types). Let's write this
  type again (this time in F#):

  type RejectionCause = RECIPIENT_UNKNOWN | PACKAGE_DAMAGED

  type PackageDeliveryStatus =
        | Preparing of Long
        | Prepared of Long
        | Rejected of Long * RejectionCause
        | Dispatched of Long * String

  We create a sealed hierarchy of PackageDeliveryStatus with several subclasses:
  - Preparing that has only an ID of type Long
  - Prepared that has only and ID of type Long
  - Rejected that has an ID of type Long and RejectionCause. Please notice that
    we can now remove this silly RejectionCause.NO_CAUSE because the RejectionCause
    will never appear in any other case
  - Dispatched that has an ID of type Long and a TrackingStatus of type String.

  Let's rewrite it in Kotlin
 */
sealed class BetterPackageDeliveryStatus {
    data class Preparing(val id: Long) : BetterPackageDeliveryStatus()
    data class Prepared(val id: Long) : BetterPackageDeliveryStatus()
    data class Rejected(val id: Long, val cause: RejectionCause) : BetterPackageDeliveryStatus()
    data class Dispatched(val id: Long, val trackingCode: String) : BetterPackageDeliveryStatus()

    enum class RejectionCause {
        RECIPIENT_UNKNOWN,
        PACKAGE_DAMAGED
    }
}

/*
  But how to work with such types?

  First thought is to use `instanceOf`. It is problematic for a few reasons:
  - people coming from a C++ background will be thinking about dynamic_cast and
    how it takes precious cycles to do it and then check if the returned pointer
    is not null.
    Is instanceOf really that costly nowadays?
    Anyone checked?
    Any JMH results that we can relay on?
  - `instanceOf` will be probably used with the `if/else` statement. This is
    super verbose as Java forces us to do a cast afterwards, but also is prone
    to error. One can easily forget a case, compiler won't remind us we are not
    handling some case.
  - it is a defeat of a "normal" object oriented programming in Java. The
    canonical way of handling it is to use subtype polymorphism.

  Second thought is to create a family of `is***` methods. For our little
  package delivery example it would be `isPreparing()`, `isPrepared()`,
  `isDispatched()`, `isRejected()`.

  The problem with this is that it doesn't bring us any gain comparing to the
  similar usage of `instanceOf` operator. In the end we will have to still cast
  the thing to a desired type.

  The answer lies in polymorphism, subtype polymorphism to be more precise.
  Remember how we implemented the `ifTrue`, `ifFalse` and `match` methods over
  our Boolean type?

  We can add "domain" specific polymorphic methods for our ADTs.
 */

/*
  Part 1:

  You are working for a revolutionary startup that is preparing a restaurant
  aggregating website called eat.it. As the micro-services are the current latest
  and greatest hip technology you decided to communicate the with Events.

  Your solution architect created a list of possible events that you will have
  to work with.

  1) The food order was created created.
     - the event should contain an id
     - the event should contain information about the food ordered
     - the event should contain information about the address where it should be
       sent
  2) The food ordered was updated.
     - the event should contain an id
     - the event should contain information about the updated food
  3) The address was updated.
     - the event should contain an id
     - the event should contain information about the updated address
  4) The food order was canceled.
     - the event should contain an id

  Please create an ADT (use the knowledge about product and sum types to do it)
  that allows modeling a food order event for the eat.it portal.

  This is just a modeling exercise please do not focus on the id, what, where
  types. You can use Long for Id and String for What (ordered food) and Where
  (address). Do not focus on mutability or visibility of fields!

  Hints:
  - this problem can be solved in two different ways
  - what is the thing that is common for all events?
 */
class FoodOrderEvent// implement the type!

@Suppress("SpellCheckingInspection")
fun main(args: Array<String>) {
    // Discussion

    // If we want to create a delivery status for a package that is
    // dispatched to the designated address we need to do:
    val pds1 = PackageDeliveryStatus(
            // some id:
            1L,
            // this is obvious:
            "a super fancy tracking code for web ui!",
            PackageDeliveryStatus.Stage.DISPATCHED,
            // Depending on what we decide we need to either set this to null
            // or create a special enum case called NO_CAUSE, we are on a null
            // purging crusade so we go with NO_CAUSE instead
            PackageDeliveryStatus.RejectionCause.NO_CAUSE
    )

    println(pds1)

    // What about the a rejected package delivery?
    val pds2 = PackageDeliveryStatus(
            42L,
            null,
            PackageDeliveryStatus.Stage.REJECTED,
            PackageDeliveryStatus.RejectionCause.RECIPIENT_UNKNOWN
    )

    println(pds2)

    // This way we have only access to the fields that have sense in a
    // particular case
    val bpds1 = BetterPackageDeliveryStatus.Dispatched(
            1L,
            "another code for the super fancy tracking ui!"
    )

    println(bpds1)

    val bpds2 = BetterPackageDeliveryStatus.Rejected(
            42L,
            BetterPackageDeliveryStatus.RejectionCause.RECIPIENT_UNKNOWN
    )

    println(bpds2)
}
