package practice;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

public class UsageJava8 {

	public static void main(String[] args) {
		// IntStream
	    final int[] valArray1 = IntStream.range(0, 10).toArray();
	    System.out.println(IntStream.of(valArray1).sum());
	    System.out.println(IntStream.of(1, 2, 3, 4)
        	.filter(e -> e > 2)
        	// Streamの状態を変えないforEach
        	// Debugとかで使う
        	.peek(e -> System.out.println("Filtered value: " + e))
        	.map(e -> e * e)
        	.peek(e -> System.out.println("Mapped value: " + e))
        	.sum());
	    
	    final int[] valArray2 = IntStream.rangeClosed(0, 10).toArray();
	    final long[] valArray3 = LongStream.range(0, 10).toArray();
	    final long[] valArray4 = LongStream.rangeClosed(0, 10).toArray();
	    IntStream.rangeClosed(1, 9).forEach( i -> {
	    	IntStream.rangeClosed(1,9).forEach( j -> {
	    		System.out.printf("%2d ", i*j);
	    	});
            System.out.println();
	    });
	    IntStream is = IntStream.range(1,10);
	    is.forEach( i -> {
	    	is.forEach( j -> {
	    		System.out.printf("%2d ", i*j);
	    	});
	    	System.out.println();
	    });
	}
	
	public static void streamUsage() {
		Stream<String> ss = Stream.of("123", "456");
		IntStream is = ss.mapToInt(s -> Integer.parseInt(s));
		is.forEach(System.out::println);
	}

}
