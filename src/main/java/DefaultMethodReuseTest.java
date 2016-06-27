interface HumanoidForm {
    default void reproduce() {
       System.out.println("From HumanoidForm interface");
    }
}

abstract class ArtificialHumanoidForm implements HumanoidForm {
}

class Android extends ArtificialHumanoidForm {
	@Override
	public void reproduce() {
		// The following produces: "no enclosing instance of the type HumanoidForm is available in scope"
		// However, adding "implements HumanoidForm" to this class fixes it,
		// but that is odd, since Android should already be considered an implementation of HumanoidForm
		// HumanoidForm.super.reproduce();
		
		// The following produces: "no enclosing instance of the type ArtificialHumanoidForm is available in scope"
		// Naturally, it is not possible to add "implements ArtificialHumanoidForm"
		// In fact, you would think that extending ArtificialHumanoidForm would have the same effect, but somehow it does not.
		super.reproduce();
	}
}

public class DefaultMethodReuseTest
{             
    public static void main(String[] args) 
    {       
        new Android().reproduce();         
    }   
}
