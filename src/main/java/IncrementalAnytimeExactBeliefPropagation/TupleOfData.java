package IncrementalAnytimeExactBeliefPropagation;


public class TupleOfData{
	int id = -1;
	String typeOfComputationUsed = ""; // SGDPLL or Bounded S-BP
	String graphicalModelName = "";
	int parameter[] = new int[5]; 
	int iteration = -1;
	double minAndMaxProbabilityofQueryequalsTrue = -1;
	double maxAndMaxProbabilityofQueryequalsTrue = -1;
	double IntervalLength = -1;
	int numberOfExtremePoints = -1;
	boolean allExplored = true;
	double time = -1;
	double totalTime = 0;
	
	
	public TupleOfData() {
	}
}
