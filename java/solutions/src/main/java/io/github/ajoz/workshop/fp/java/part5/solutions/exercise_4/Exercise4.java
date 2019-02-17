package io.github.ajoz.workshop.fp.java.part5.solutions.exercise_4;

import io.github.ajoz.workshop.fp.java.tools.control.Maybe;

@SuppressWarnings("unused")
final class User {
    PersonalInfo personalInfo;

    ContactInfo contactInfo;
    // vs
    ContactInfo2 primaryContactInfo;
    Maybe<ContactInfo2> secondaryContactInfo;
}

@SuppressWarnings("unused")
class Name {
    String value;
}

@SuppressWarnings("unused")
class Email {
    String email;
}

@SuppressWarnings("unused")
class PersonalInfo {
    Name firstName;
    Name lastName;
    Maybe<Name> middleName;
    Maybe<Name> maidenName;
}

@SuppressWarnings("unused")
abstract class EmailContactInfo {
    static class VerifiedEmail extends EmailContactInfo {
        Email email;
    }

    static class UnverifiedEmail extends EmailContactInfo {
        Email email;
    }
}

@SuppressWarnings("unused")
class PhoneNumber {
    String phone;
}

@SuppressWarnings("unused")
abstract class PhoneNumberContactInfo {
    static class VerifiedPhoneNumer extends PhoneNumberContactInfo {
        PhoneNumber phoneNumber;
    }

    static class UnverifiedPhoneNumber extends PhoneNumberContactInfo {
        PhoneNumber phoneNumber;
    }
}

@SuppressWarnings("unused")
abstract class ContactInfo {
    static class EmailOnly extends ContactInfo {
        EmailContactInfo emailContactInfo;
    }

    static class PhoneNumberOnly extends ContactInfo {
        PhoneNumberContactInfo phoneNumberContactInfo;
    }

    static class EmailAndPhoneNumber extends ContactInfo {
        EmailContactInfo emailContactInfo;
        PhoneNumberContactInfo phoneNumberContactInfo;
    }
}

abstract class ContactInfo2 {
    static class Email extends ContactInfo2 {
        EmailContactInfo emailContactInfo;
    }

    static class Phone extends ContactInfo2 {
        PhoneNumberContactInfo phoneNumberContactInfo;
    }
}

public class Exercise4 {
    public static void main(final String[] args) {

    }
}
