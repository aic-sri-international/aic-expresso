import java.util.Random;


public class WrappingPerformanceTest {

	private static class Wrap {
		Object wrapped;
		public Wrap(Object object) {
			this.wrapped = object;
		}
	}

	private static boolean doWrap = true;
	
	public static void main(String[] args) {
		long start = System.currentTimeMillis();
		for (int i = 0; i != 100000000; i++) {
			Object object = i + i;
			object = ((Integer) object).intValue() + new Random().nextInt();
			Object object2;
			
			if (doWrap) {
				Wrap w = new Wrap(object);
				object2 = w.wrapped;
			}
			else {
				object2 = object;
			}
			
			if (object2 == null) {
				System.out.println("Null!");	
			}
		}
		long end = System.currentTimeMillis();
		System.out.println("Took: " + (end - start) + " ms.");
	}
}
