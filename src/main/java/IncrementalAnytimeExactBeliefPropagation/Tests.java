package IncrementalAnytimeExactBeliefPropagation;

import static IncrementalAnytimeExactBeliefPropagation.ModelGenerator.IsingModel;
import static IncrementalAnytimeExactBeliefPropagation.ModelGenerator.lineModel;
import static IncrementalAnytimeExactBeliefPropagation.ModelGenerator.nTreeModel;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

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
import com.sri.ai.util.base.Triple;

import IncrementalAnytimeExactBeliefPropagation.Model.BFS;
import IncrementalAnytimeExactBeliefPropagation.Model.Model;

public class Tests {
	static Theory theory;
	static Context context;
	static int ID = 0;
	
	public static void main(String[] args) {
		//Theory initialization
		theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		context = new TrueContext(theory);	
		context = context.extendWithSymbolsAndTypes("A","Boolean");

		//Testing BFS Expander
		//testingBFS();
		
		//Testing on standard output (output results on screen)
		//testingAndPrintingOnScreen();
		
		testingAndPrintingOnFile();
		
	}

	public static void testingAndPrintingOnFile() {
		
		String modelName = "IsingModel4X4";
		ModelGenerator.resetRandomGenerator();
		Triple<Set<Expression>,Context,Expression> factors = IsingModel(4,4, context, parse("Boolean"));
		boolean printSGDPLL = true;
		double thresholder = 0.0001;
		int n = 5;
		
		testingTheSameModelNTimesAndThenPrintingToFileVersionsOneAndTwoOfTheAlgorithm(factors,n,modelName,printSGDPLL,thresholder);
		
		println("The End");
		//descricao de como usar a classe
		//imprimir em texto e passar pro ppt
		//deixar rodando o 4 por 4 (moienar com 5 de cada)
	}

	public static void testingAndWritingToFile() {
		
		ModelGenerator.resetRandomGenerator();
		
		List<List<TupleOfData>> modelsToprintInFile = new ArrayList<>();
		
		int nLines = 4;
		int nCols = 4;
		Model m = new Model(IsingModel(nLines, nCols, context, parse("Boolean")),theory,true);
		modelsToprintInFile.add(testingAndExportingListWithDataFunction("IsingModel", m, true,true, 0,nLines, nCols));
		
		int nFactors = 12;
		m = new Model(ModelGenerator.lineModel(nFactors, context, parse("Boolean")),theory,true);
		modelsToprintInFile.add(testingAndExportingListWithDataFunction("lineModel", m, true,true, 0,nFactors));
		
		int nLevels = 4;
		int nChildren = 2;
		m = new Model(ModelGenerator.nTreeModel(nLevels, nChildren, context, parse("Boolean")),theory,true);
		modelsToprintInFile.add(testingAndExportingListWithDataFunction("nTreeModel", m, true, true,0,nLevels, nChildren));
		
		writingToFile("SomeTests.csv", modelsToprintInFile);
	}

	public static void testingTheSameModelNTimesAndThenPrintingToFileVersionsOneAndTwoOfTheAlgorithm(Triple<Set<Expression>,Context,Expression> factors,
			int n,String fileNameWithoutExtension, boolean printSGDPLL,double thresholder) {
	
	List<List<TupleOfData>> modelsToprintInFile = new ArrayList<>();
	
	for(int i = 0; i < n; i++){
		Model m = new Model(factors,theory,true);
		modelsToprintInFile.add(testingAndExportingListWithDataFunction(fileNameWithoutExtension + "Incremental", m, true, printSGDPLL, thresholder));
		println(i);
	}
	
	for(int i = 0; i < n; i++){		
		ModelGenerator.resetRandomGenerator();
		Model m = new Model(factors,theory,true);
		modelsToprintInFile.add(testingAndExportingListWithDataFunction(fileNameWithoutExtension + "NONIncremental", m, false, printSGDPLL, thresholder));
		println(i);
	}
	
	writingToFile(fileNameWithoutExtension + ".csv", modelsToprintInFile);
}


	public static void testingAndPrintingOnScreen() {
		Model m = new Model(IsingModel(7,2, context, parse("Boolean")),theory,true);

		testFunction("IsingModel", m, true);
		
		m = new Model(lineModel(8, context, parse("Boolean")),theory,true);

		testFunction("lineModel", m, true);
		
		m = new Model(nTreeModel(9, 2, context, parse("Boolean")),theory,true);

		testFunction("nTreeModel", m, true);
	}

	public static void testingBFS() {
		Model m = new Model(IsingModel(4,3, context, parse("Boolean")),theory,true);
		Iterator<PartitionTree> BFSExpander = new BFS(m);
		
		while(BFSExpander.hasNext()){
			PartitionTree p = BFSExpander.next();
			println("node : " + p.node);
			println("parent: "+ p.parent.node);
			
			println("children");
			for(PartitionTree pv : p.children){
				println(pv.node.getValue());
			}
		}
	}

	/**
	 * This function tests a given model and prints the results on the screen 
	 * @param modelName
	 * @param m
	 * @param printAll - says if you wanna all the steps to be printed or just the final iteration
	 */
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
				println("ExtremePoints : "+inferenceResult.getArguments());
				Pair<Double, Double> minAndMaxProbabilityofQueryequalsTrue = ModelGenerator.MaxMinProbability(inferenceResult, m);
				println("Minimal probability of Query = true : " +
						minAndMaxProbabilityofQueryequalsTrue.first +
						"\nMaximal probability of Query = true :" +
						minAndMaxProbabilityofQueryequalsTrue.second +
						"\nLength of interval (that is, (max - min)) : " + (minAndMaxProbabilityofQueryequalsTrue.second - minAndMaxProbabilityofQueryequalsTrue.first) +
						"\nTime to compute this iteration:" + time + ". Toatal time : " + totalTime);
				println("----------------- AllExplored : " + sbp.AllExplored() + "-----------------");
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
	
	/**
	 * This tests a model and, instead of printing information, stores its in a list of data structures
	 * each element of the list corresponds to a iteration of the algorithm
	 * @param modelName
	 * @param m
	 * @param parameter
	 * @return
	 */
	public static List<TupleOfData> testingAndExportingListWithDataFunction(String modelName, Model m, boolean Incrementalversion, boolean printSGDPLL,double thresholder, Integer... parameter){
		List<TupleOfData> result = new ArrayList<TupleOfData>();
		
		int id = 0;
		m.clearExploredGraph();
		Iterator<PartitionTree> BFSExpander = new BFS(m);
		IncrementalAnytymeBeliefPropagationWithSeparatorConditioning sbp = new IncrementalAnytymeBeliefPropagationWithSeparatorConditioning(m,BFSExpander);
		
		double tTotalTime = 0;
		double error = 1;
		if(thresholder >= 1){
			thresholder = 0;
		}
		while(BFSExpander.hasNext() && error > thresholder){
			
			TupleOfData t = new TupleOfData();
			
			long tStart = System.currentTimeMillis();
			Bound inferenceResult = Incrementalversion? sbp.ExpandAndComputeInference() : sbp.ExpandAndComputeInferenceByRebuildingPartitionTree();
			long tEnd = System.currentTimeMillis();
			long tDelta = tEnd - tStart;
			t.time = tDelta /1000.0;
			
			tTotalTime += tDelta / 1000.0;
			t.totalTime +=  tTotalTime;
			
			t.typeOfComputationUsed = "S-BP";
			t.graphicalModelName = modelName;
			t.id = ID++;
			t.iteration = id++;
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
			error = t.IntervalLength;
			println("...." + error);
		}
		
		if(printSGDPLL && !BFSExpander.hasNext()){
			TupleOfData t = new TupleOfData();
			long tStart = System.currentTimeMillis();
			Expression inferenceLVE = ModelGenerator.LVECalculation(m);
			long tEnd = System.currentTimeMillis();
			long tDelta = tEnd - tStart;
			Bound EncapsulatingInference = Bounds.makeSingleElementBound(inferenceLVE, true);
			Pair<Double, Double> minAndMaxProbabilityofQueryequalsTrue = ModelGenerator.MaxMinProbability(EncapsulatingInference, m);
			t.time= tDelta / 1000.0;
			t.totalTime = t.time;
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
			println("SGDPLL computed");
		}
		println("------------------------- Done -----------------------------------");
		return result;
	}
	
	/**
	 * This prints in a file the content of trying many different models.
	 * the idea is to test many different models (with teh function {@code testing}) and have them printed in the same .csv  file
	 * @param filename
	 * @param testedModels
	 */
	public static void writingToFile(String filename, List<List<TupleOfData>> testedModels){
		try{
		    PrintWriter writer = new PrintWriter(filename, "UTF-8");
		    //print head of dataset
		    writer.println("Id,"
		    		+ "TypeOfComputationUsed,"
		    		+ "GraphicalModelName,"
		    		+ "Iteration,"
		    		+ "MinAndMaxProbabilityofQueryequalsTrue,"
		    		+ "MaxAndMaxProbabilityofQueryequalsTrue,"
		    		+ "IntervalLength,"
		    		+ "NumberOfExtremePoints,"
		    		+ "AllExplored,"
		    		+ "Time,"
		    		+ "Total Time,"
		    		+ "Parameter 1,"
		    		+ "Parameter 2,"
		    		+ "Parameter 3,"
		    		+ "Parameter 4,"
		    		+ "Parameter 5");
		    //printLines
		    for(List<TupleOfData> l : testedModels){		    
			    for(TupleOfData t : l){
			    		writer.print(t.id + "," +
			    					t.typeOfComputationUsed +","+
			    					t.graphicalModelName + "," +
			    					t.iteration + "," +
			    					t.minAndMaxProbabilityofQueryequalsTrue + "," +
			    					t.maxAndMaxProbabilityofQueryequalsTrue + "," +
			    					t.IntervalLength + "," +
			    					t.numberOfExtremePoints + "," +
			    					t.allExplored + "," +
			    					t.time+ "," +
			    					t.totalTime);
			    		for (int i = 0; i < t.parameter.length; i++) {
			    			writer.print("," + t.parameter[i] );
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