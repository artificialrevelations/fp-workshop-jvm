package io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_3;

/*
  We can model this in two ways:

  1) Id is repeated for every type of an order event:

  type FoodOrderEvent =
                  | OrderCreated of Id * What * Where
                  | OrderWhatUpdated of Id * What
                  | OrderWhereUpdated of Id * Where
                  | OrderCanceled of Id
 */
@SuppressWarnings("unused")
class FoodOrderEvent1 {
    static class OrderCreated extends FoodOrderEvent1 {
        Long id;
        String what;
        String where;
    }

    static class OrderWhatUpdated extends FoodOrderEvent1 {
        Long id;
        String what;
    }

    static class OrderWhereUpdated extends FoodOrderEvent1 {
        Long id;
        String where;
    }

    static class OrderCanceled extends FoodOrderEvent1 {
        Long id;
    }

    private FoodOrderEvent1() {
    }
}

/*
  or

  2) Id is a part of an event (we can guarantee that it exists always)
  type FoodOrderEventType =
                  | OrderCreated of What * Where
                  | OrderWhatUpdated of What
                  | OrderWhereUpdated of Where
                  | OrderCanceled

  type FoodOrderEvent = Id * FoodOrderEventType
 */
@SuppressWarnings("unused")
class FoodOrderEventType {
    static class OrderCreated extends FoodOrderEventType {
        String what;
        String where;
    }

    static class OrderWhatUpdated extends FoodOrderEventType {
        String what;
    }

    static class OrderWhereUpdated extends FoodOrderEventType {
        String where;
    }

    static class OrderCanceled extends FoodOrderEventType {
    }

    private FoodOrderEventType() {
    }
}

@SuppressWarnings("unused")
class FoodOrderEvent2 {
    Long id;
    FoodOrderEventType type;
}

public class Exercise3 {
    public static void main(final String[] args) {
        // First version:
        final FoodOrderEvent1.OrderCreated foe1 = new FoodOrderEvent1.OrderCreated();
        foe1.id = 42L;
        foe1.what = "A very very very delicious food!";
        foe1.where = "Our office right around noon";

        // Second version:
        final FoodOrderEvent2 foe2 = new FoodOrderEvent2();
        foe2.id = 42L;
        final FoodOrderEventType.OrderCreated oc = new FoodOrderEventType.OrderCreated();
        oc.what = "A super delicious and definitely not spicy food!";
        oc.where = "Our office but please be on time this time :>";
        foe2.type = oc;
    }
}
