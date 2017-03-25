package practice;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class Usage {

	public static void main(String[] args) {

		List<String> list = new ArrayList<>(Arrays.asList("AA", "BB", "CC"));
		// Print [AA, BB, CC]
		System.out.println(list);
		for (String x : list) {
			// Print
			// AA
			// BB
			// CC
			System.out.println(x);
		}

		List<String> list1 = new ArrayList<>();
		for (String item : new String[]{"AA", "BB", "CC"}) {
		    list1.add(item);
		}

		List<String> list2 = new ArrayList<String>(){
			private static final long serialVersionUID = 1L;
			{
		        add("AA");
		        add("BB");
		        add("CC");
		    }
		};

		List<String> list3 = new ArrayList<>();
		Collections.addAll(list2, new String[]{"AA", "BB", "CC"});

		List<String> list4 = new ArrayList<>();
		Collections.addAll(list4, "AA", "BB", "CC");
	}

}
