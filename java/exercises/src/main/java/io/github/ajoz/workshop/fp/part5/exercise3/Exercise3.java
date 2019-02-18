package io.github.ajoz.workshop.fp.part5.exercise3;

/*
  -- Working with Alternatives --

  You are working for a company called CaliDada and you have a shopping website
  called CaliExpress, it's 2018 so you have a nice mobile app for all the major
  platforms.

  The API you are using is modeled to help with low latency, low storage space,
  low memory. Your API can get all the necessary information out of Cache,
  Database or a rock solid REST Service.

  You are currently in the middle of reworking your API and refactoring the code
  and you thought you could make it more safe and cleaner.
 */

// Unique Id used for retrieving things in you API
final class Id {
}

// The domain object describing the Product
final class Product {
    // some specific things related to CaliDada products
}

/*
  Your API facade class that exposes all the `getProduct*` methods to the other
  parts of the system. Below are the definitions of your currently used functions
  - getProductFromCache
  - getProductFromDatabase
  - getProductFromService

  and there is a
  - getProduct

  that combines them all into a single thing.
 */
@SuppressWarnings({"WeakerAccess", "unused", "RedundantThrows"})
final class API {
    static Product getProductFromService(final Id id) throws ProductNotInService {
        return new Product(); // super heavy business logic!
    }

    static Product getProductFromDatabase(final Id id) throws ProductNotInDatabase {
        return new Product(); // even more business logic then before!
    }

    static Product getProductFromCache(final Id id) throws ProductNotInCache {
        return new Product(); // just a lot of business logic!
    }

    /*
      Questions:
      - Do you like the code below?
      - Does this code look familiar to some other code we seen before?
      - Can we make it more linear?
     */
    @Deprecated
    static Product getProduct(final Id id) throws ProductIsMissing {
        try {
            // first we try to get this from cache
            return getProductFromCache(id);
        } catch (final ProductNotInCache productNotInCache) {
            // then if the cache fails we try to get the product from DB
            try {
                return getProductFromDatabase(id);
            } catch (ProductNotInDatabase productNotInDatabase) {
                // if all else fails then try to get this from REST
                try {
                    return getProductFromService(id);
                } catch (ProductNotInService productNotInService) {
                    // even our glorious REST service failed we need to give up
                    throw new ProductIsMissing();
                }
            }
        }
    }

    static class ProductIsMissing extends Exception {
    }

    static class ProductNotInCache extends Exception {
    }

    static class ProductNotInDatabase extends Exception {
    }

    static class ProductNotInService extends Exception {
    }
}

/*
  Part 1:

  Rework the existing API to use be type safe and use Try instead of throwing an
  exception. Please change the:
  - getProductFromCache
  - getProductFromDatabase
  - getProductFromService
  - getProduct

  We already created a method for working with Failure case in Try it was called
  `recover` and `recoverWith`:

  Try<A> recover(Function1<Throwable, A> function)
  Try<A> recoverWith(Function1<Throwable, Try<A>> function)

  They resemble `map` and `flatMap` but for Failure case.

  Sometimes though we are not interested in the error stored in the Failure case
  we would like to just handle the situation. This is why two additional methods
  were added:

  Try<A> orElse(Try<A> defaultTry);
  Try<A> orElse(Supplier<Try<A>> supplier);

  Questions:
  - Which do you think should be used?
  - How should they work?

  Hints:
  - Try has a static method called lift and liftP
  - Try has a orElse method that doesn't care about the error

  Questions:
  - Could we somehow compose it in Java?
 */
public class Exercise3 {
    public static void main(final String[] args) {
        try {
            final Product product = API.getProduct(new Id());
            System.out.println("product = " + product);
        } catch (final API.ProductIsMissing productIsMissing) {
            productIsMissing.printStackTrace();
        }
    }
}
