package IncrementalAnytimeExactBeliefPropagation;

import static IncrementalAnytimeExactBeliefPropagation.ModelGenerator.IsingModel;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bounds;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
import com.sri.ai.util.base.Pair;

import IncrementalAnytimeExactBeliefPropagation.Model.BFS;
import IncrementalAnytimeExactBeliefPropagation.Model.Model;

public class Tests {
	
	public static void main(String[] args) {
		//Theory initialization
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		Context context = new TrueContext(theory);	
		context = context.extendWithSymbolsAndTypes("A","Boolean");
		
		
		Model m = new Model(IsingModel(4,3, context, parse("Boolean")),theory,true);
		
		//m = new Model(ModelGenerator.lineModel(6, context, parse("Boolean")),theory,true);
		
		//m = new Model(ModelGenerator.nTreeModel(3, 3, context, parse("Boolean")),theory,true);
		
		
//		Iterator<PartitionTree> BFSExpander = new BFS(m);
//		
//		while(BFSExpander.hasNext()){
//			PartitionTree p = BFSExpander.next();
//			println("node : " + p.node);
//			println("parent: "+ p.parent.node);
//			
//			println("children");
//			for(PartitionTree pv : p.children){
//				println(pv.node.getValue());
//			}
//		}
		
		testFunction("me", m, true);
		
		
//		modelName = "Line Model";
//		m = new Model(lineModel(10, context, parse("Boolean")),theory, true);
//		
		
	}

	private static void testFunction(String modelName, Model m, boolean printAll) {
		Iterator<PartitionTree> BFSExpander = new BFS(m);
		IncrementalAnytymeBeliefPropagationWithSeparatorConditioning sbp = new IncrementalAnytymeBeliefPropagationWithSeparatorConditioning(m,BFSExpander);
		println("Exploring " + modelName);
		Bound inferenceResult = null;
		double totalTime = 0;
		while(!sbp.AllExplored()){
			long tStart = System.currentTimeMillis();
			inferenceResult = sbp.ExpandAndComputeInference();
			long tEnd = System.currentTimeMillis();
			long tDelta = tEnd - tStart;
			double time = tDelta / 1000.0;	
			totalTime += time;
			
			//ModelGenerator.printModel(m, false);
			if(printAll){
				println("Number of ExtremePoints : "+inferenceResult.getArguments().size());
				Pair<Double, Double> minAndMaxProbabilityofQueryequalsTrue = ModelGenerator.MaxMinProbability(inferenceResult, m);
				println("Minimal probability of Query = true : " +
						minAndMaxProbabilityofQueryequalsTrue.first +
						"\nMaximal probability of Query = true :" +
						minAndMaxProbabilityofQueryequalsTrue.second +
						"\nLength of interval (that is, (max - min)) : " + (minAndMaxProbabilityofQueryequalsTrue.second - minAndMaxProbabilityofQueryequalsTrue.first) +
						"\nTime to compute this iteration:" + time + ". Toatal time : " + totalTime);
				println("----------------- AllExplored : " + m.AllExplored() + "-----------------");
			}
		}

		if(!printAll)
			println(inferenceResult);
		println("Computation with SGDPLL");
		long tStart = System.currentTimeMillis();
		Expression LVE = ModelGenerator.LVECalculation(m);
		long tEnd = System.currentTimeMillis();
		long tDelta = tEnd - tStart;
		double time = tDelta / 1000.0;	
		
		println(LVE + "\n"+
				"\nTime to compute:" + time);
	}
	
	public static List<TupleOfData> testing(String modelName, Model m, Integer... parameter){
		List<TupleOfData> result = new ArrayList<TupleOfData>();
		
		int id = 0;
		m.clearExploredGraph();
		Iterator<PartitionTree> BFSExpander = new BFS(m);
		IncrementalAnytymeBeliefPropagationWithSeparatorConditioning sbp = new IncrementalAnytymeBeliefPropagationWithSeparatorConditioning(m,BFSExpander);
		while(BFSExpander.hasNext()){
			
			TupleOfData t = new TupleOfData();
			
			long tStart = System.currentTimeMillis();
			Bound inferenceResult = sbp.ExpandAndComputeInference();
			long tEnd = System.currentTimeMillis();
			long tDelta = tEnd - tStart;
			t.time= tDelta / 1000.0;	
			t.typeOfComputationUsed = "S-BP";
			t.graphicalModelName = modelName;
			t.id = id++;
			t.numberOfExtremePoints = inferenceResult.getArguments().size();
			Pair<Double, Double> minAndMaxProbabilityofQueryequalsTrue = ModelGenerator.MaxMinProbability(inferenceResult, m);
			t.minAndMaxProbabilityofQueryequalsTrue = minAndMaxProbabilityofQueryequalsTrue.first;
			t.maxAndMaxProbabilityofQueryequalsTrue = minAndMaxProbabilityofQueryequalsTrue.second;
			t.IntervalLength = t.maxAndMaxProbabilityofQueryequalsTrue - t.minAndMaxProbabilityofQueryequalsTrue; 
			t.allExplored = m.AllExplored();
			
			for (int i = 0; i < parameter.length && i < t.parameter.length; i++) {
				t.parameter[i] = parameter[i];
			}
			
			result.add(t);
			
			println("....");
		}
		
		TupleOfData t = new TupleOfData();
		
		long tStart = System.currentTimeMillis();
		Expression inferenceLVE = ModelGenerator.LVECalculation(m);
		Bound EncapsulatingInference = Bounds.makeSingleElementBound(inferenceLVE, true);
		Pair<Double, Double> minAndMaxProbabilityofQueryequalsTrue = ModelGenerator.MaxMinProbability(EncapsulatingInference, m);
		long tEnd = System.currentTimeMillis();
		long tDelta = tEnd - tStart;
		t.time= tDelta / 1000.0;	
		t.minAndMaxProbabilityofQueryequalsTrue = minAndMaxProbabilityofQueryequalsTrue.first;
		t.maxAndMaxProbabilityofQueryequalsTrue = minAndMaxProbabilityofQueryequalsTrue.second;
		
		t.typeOfComputationUsed = "SGDPLL";
		t.graphicalModelName = modelName;
		t.id = id++;
		t.numberOfExtremePoints = 0;
		t.IntervalLength = 0; 
		t.allExplored = true;
		
		for (int i = 0; i < parameter.length && i < t.parameter.length; i++) {
			t.parameter[i] = parameter[i];
		}
		
		result.add(t);	

		println("------------------------------------------------------------");
		return result;
	}
	
	public static void testingAndWritingToFile(String filename, List<List<TupleOfData>> testedModels){
		try{
		    PrintWriter writer = new PrintWriter(filename, "UTF-8");
		    //print head of dataset
		    writer.println("Id,"
		    		+ "typeOfComputationUsed,"
		    		+ "graphicalModelName,"
		    		+ "minAndMaxProbabilityofQueryequalsTrue,"
		    		+ "maxAndMaxProbabilityofQueryequalsTrue,"
		    		+ "IntervalLength,"
		    		+ "numberOfExtremePoints,"
		    		+ "allExplored,"
		    		+ "time,"
		    		+ "Parameter 1,"
		    		+ "Parameter 2,"
		    		+ "Parameter 3,"
		    		+ "Parameter 4,"
		    		+ "Parameter 5,");
		    //printLines
		    for(List<TupleOfData> l : testedModels){		    
			    for(TupleOfData t : l){
			    		writer.print(t.id + "," +
			    					t.typeOfComputationUsed +","+
			    					t.graphicalModelName + "," +
			    					t.minAndMaxProbabilityofQueryequalsTrue + "," +
			    					t.maxAndMaxProbabilityofQueryequalsTrue + "," +
			    					t.IntervalLength + "," +
			    					t.numberOfExtremePoints + "," +
			    					t.allExplored + "," +
			    					t.time + ",");
			    		for (int i = 0; i < t.parameter.length; i++) {
			    			writer.print(t.parameter[i] + ",");
			    		}
			    		writer.println();
			    }    
		    }
		    writer.close();
		    
		} catch (IOException e) {
		   // do something
		}
	}

}