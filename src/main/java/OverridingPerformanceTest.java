
public class OverridingPerformanceTest {

	interface Person {
		String live();
	}
	
	static class Parent implements Person {
		public String live() {
			return "do what parents do";
		}
	}

	static class Child extends Parent {
		
		String name;
		boolean useParentAsRoleModel = true;
		
		public Child(String name, boolean useParentAsRoleModel) {
			super();
			this.name = name;
			this.useParentAsRoleModel = useParentAsRoleModel;
		}

		public String live() {
			if (useParentAsRoleModel) {
				return super.live();
			}
			else {
				Parent anotherRoleModel = new Parent();
				return anotherRoleModel.live();
			}
		}
	}
	
	private static final long REPETITIONS = 10000000000L;

	public static void main(String[] args) {
		Child goodKid  = new Child("Good kid",  true);
		Child rebelKid = new Child("Rebel kid", false);

		for (Child kid : new Child[] {goodKid, rebelKid, goodKid}) {
			long start = System.currentTimeMillis();
			for (long i = 0; i != REPETITIONS; i++) {
				kid.live();
			}
			long end = System.currentTimeMillis();
			System.out.println(kid.name + " took: " + (end - start));
		}
	}
}
