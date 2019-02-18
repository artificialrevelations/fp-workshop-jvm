package io.github.ajoz.workshop.fp.java.part5.exercise4;

/*
  -- Modeling the Domain --

  You are now working on the user API in Calidada, CaliExpress can have registered
  users. You are currently working on the registration API, a previous team that
  did not manage to finish this User Story left it for you.

  You see a class:
 */
final class User {
    String firstName;
    String middleName;
    String lastName;
    String maidenName;

    String email;
    Boolean isVerified;

    // some constructors and getters
}

/*
  - Do you like this class?
  - What are the problems with this class? Can you name a few?

  Let's list the constraints:
  - Can we put whatever String into firstName, middleName, lastName and maidenName?
    Like: new lines, tabs, whitespaces?
  - Can we put first part of some book in the name? Do we have constraints on size?
  - Does every Use has a middleName or a maidenName?
  - Can we put whatever String into email?
  - Which values are optional?
  - Which values are linked?

  What will happen if a new business requirement comes and you will be asked to
  add phone number?
  - you will rename `isVerified` to `isEmailVerified`
  - you will add another `String` field for phone
  - you will add a field `isPhoneVerified`

  Soon this will probably get out of hand and you will be juggling null values.

  How should we rework it?
  - let's create proper types
  - let's group the things that have things in common
  - let's wrap the isEmailVerified into a type
  - let's add phone contact info
  --- how can we make it optional?
  --- how to make User to have email or phone?
  --- are there other ways of modeling this? (having primary or secondary contact info)?

  Part 1:

  Please fix the type by adding other necessary types!
 */
public class Exercise4 {
    public static void main(final String[] args) {

    }
}
