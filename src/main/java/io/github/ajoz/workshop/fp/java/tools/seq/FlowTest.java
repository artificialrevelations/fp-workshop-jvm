package io.github.ajoz.workshop.fp.java.tools.seq;

import java.util.List;

public class FlowTest {
    public static void main(String[] args) {
        /*
         It's possible to chain each Flow one by one just with proper passing
         of the reference, although it does not look as sexy as what we are
         used to with Stream API (Java) or Sequence API (Kotlin) or Rx it still
         works and is lazy. If the last call to forEach is commented out, this
         code won't process the given array of Strings.
         */
        final Flow<String> strings = new ArrayFlow<>("This", "is", "a", "basic", "test");
        final Flow<String> peeks = new OnEachFlow<>(strings, System.out::println);
        final Flow<Integer> mapped = new MapFlow<>(peeks, String::length);
        final Flow<Integer> filtered = new FilterFlow<>(mapped, length -> length > 2);
        final Flow<Integer> taken = new TakeFlow<>(filtered, 2);
//        Flows.forEach(System.out::println, taken);


//        final Flow<Integer> iterator =
        Flow.from("This", "is", "a", "very", "basic", "test", "of", "Flow")
                .onEach(System.out::println)
                .map(String::length)
                .filter(len -> len > 2)
                .take(1);
        // .forEach(System.out::println); or passing the reference to Flow directly
        // Flows.forEach(System.out::println, iterator);

        /*
         This is a potentially infinite operation that might result with out
         of memory error.
         */
        final Flow<Integer> iterator =
                Flow.from(0, i -> i + 1)
                        .map(x -> x * 2)
                        .take(10);

        // although Flow<T> extends Iterable<T> .for live template does not work :(
        // for (final Integer integer : iterator) {
        //    System.out.println("item: " + integer);
        // }

        // Flow is very simple and it's not possible to traverse it multiple times
        // calling iterator.toList().size() will have different results
        System.out.println("#1 Elements in the lazy seq: " + iterator.toList().size());
        System.out.println("#2 Elements in the lazy seq: " + iterator.toList().size());

        // Zip test
        Flow<String> a = Flow.from("1", "2");
        Flow<String> b = Flow.from("a", "b");

        Flow<String> zipped = Flow.zip(a, b, (l, r) -> l + r);
        System.out.println("Elements in the lazy seq: " + zipped.toList());

        // cycle test
        List<String> cycled = Flow.cycle("foo", "bar").take(10).toList();
        System.out.println("cycled = " + cycled);
    }
}
