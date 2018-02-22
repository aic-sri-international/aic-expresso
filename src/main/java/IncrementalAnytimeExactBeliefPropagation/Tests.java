package IncrementalAnytimeExactBeliefPropagation;

import static IncrementalAnytimeExactBeliefPropagation.ModelGenerator.LVECalculation;
import static IncrementalAnytimeExactBeliefPropagation.ModelGenerator.isingModel;
import static IncrementalAnytimeExactBeliefPropagation.ModelGenerator.lineModel;
import static IncrementalAnytimeExactBeliefPropagation.ModelGenerator.nTreeModel;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.bounds.Bounds.makeSingleElementBound;
import static com.sri.ai.util.Util.println;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.junit.Test;

import IncrementalAnytimeExactBeliefPropagation.Model.BFS;
import IncrementalAnytimeExactBeliefPropagation.Model.Model;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.bounds.Bound;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.theory.tuple.TupleTheory;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.Triple;

public class Tests {
	static Theory theory;
	static Context context;
	static int ID = 0;

	@Test
	public void test() {
		// Theory initialization
		theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		context = new TrueContext(theory);	
		context = context.extendWithSymbolsAndTypes("A", "Boolean");

		// Testing BFS Expander
		println("Testing BFS.");
		testingBFS();

		// Testing on standard output (output results on screen)
		println("Testing and printing on screen.");
		testingAndPrintingOnScreen();

		println("Running main test.");
		runTest();		
	}	

	private static void testingAndPrintingOnFile() {

		String modelName = "IsingModel3X4";
		ModelGenerator.resetRandomGenerator();
		Triple<Set<Expression>,Context,Expression> factors = isingModel(3,4, context, parse("Boolean"));

		boolean printSGDPLL = true;
		double threshold = 0.0001;
		int n = 5;

		testingTheSameModelNTimesAndThenPrintingToFileVersionsOneAndTwoOfTheAlgorithm(factors,n,modelName,printSGDPLL,threshold);

		println("The End");

		modelName = "IsingModel4X4";
		ModelGenerator.resetRandomGenerator();
		factors = isingModel(4, 4, context, parse("Boolean"));
		printSGDPLL = true;
		threshold = 0.0001;
		n = 5;

		testingTheSameModelNTimesAndThenPrintingToFileVersionsOneAndTwoOfTheAlgorithm(
				factors, n, modelName, printSGDPLL, threshold);

		println("The End");

		modelName = "IsingModel6X2";
		ModelGenerator.resetRandomGenerator();
		factors = isingModel(6, 2, context, parse("Boolean"));
		printSGDPLL = true;
		threshold = 0.0001;
		n = 5;

		testingTheSameModelNTimesAndThenPrintingToFileVersionsOneAndTwoOfTheAlgorithm(
				factors, n, modelName, printSGDPLL, threshold);

		println("The End");

		modelName = "lineModel20";
		ModelGenerator.resetRandomGenerator();
		factors = lineModel(20, context, parse("Boolean"));
		printSGDPLL = true;
		threshold = 0.0001;
		n = 5;

		testingTheSameModelNTimesAndThenPrintingToFileVersionsOneAndTwoOfTheAlgorithm(
				factors, n, modelName, printSGDPLL, threshold);

		println("The End");
	}

	public static void testingTheSameModelNTimesAndThenPrintingToFileVersionsOneAndTwoOfTheAlgorithm(
			Triple<Set<Expression>,Context,Expression> factors,
			int n,
			String fileNameWithoutExtension, 
			boolean printSGDPLL,
			double threshold) {
	
		List<List<TupleOfData>> modelsToprintInFile = new ArrayList<>();
	
		for(int i = 0; i < n; i++){
			Model m = new Model(factors,theory,true);
			modelsToprintInFile.add(testingAndExportingListWithDataFunction(fileNameWithoutExtension + "Incremental", m, true, printSGDPLL, threshold));
			println(i);
		}
	
		for (int i = 0; i < n; i++) {		
			ModelGenerator.resetRandomGenerator();
			Model m = new Model(factors,theory,true);
			modelsToprintInFile.add(testingAndExportingListWithDataFunction(fileNameWithoutExtension + "NONIncremental", m, false, printSGDPLL, threshold));
			println(i);
		}
	
		writingToFile(fileNameWithoutExtension + ".csv", modelsToprintInFile);
	}

	private static void testingAndPrintingOnFile2() {

		ModelGenerator.resetRandomGenerator();

		List<List<TupleOfData>> modelsToprintInFile = new ArrayList<>();

		int nLines = 4;
		int nCols = 4;
		Model m = new Model(isingModel(nLines, nCols, context, parse("Boolean")),theory,true);
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

	public static void runTest() {

		int nLines = 4;
		int nCols = 4;
		int n = 3; 
		boolean printSGDPLL = true;
		double threshold = 0.00001;


		Triple<Set<Expression>,Context,Expression> factors = 
				isingModel(nLines, nCols, context, parse("Boolean"));

		testingDifferentModelsWithTheSameStructure(
				factors, 
				n, 
				"IsingModel4X4", 
				printSGDPLL, 
				threshold);
		
		//		
		//		nLines = 7;
		//		nCols = 2;
		//		n = 10;
		//		printSGDPLL = false;
		//		factors = IsingModel(nLines, nCols, context, parse("Boolean"));
		//		testingDifferentModelsWithTheSameStructure(factors, n, "IsingModel6X2", printSGDPLL, threshold);
		//		
		//		
		//		nLines = 20;
		//		nCols = 20;
		//		threshold = .001;
		//		n = 10;
		//		factors = IsingModel(nLines, nCols, context, parse("Boolean"));
		//		testingDifferentModelsWithTheSameStructure(factors, n, "IsingModel20X20", printSGDPLL, threshold);
		//		
		//		


		//		
		//		int nLevels = 4;
		//		int nChildren = 2;
		//		factors = nTreeModel(nLevels, nChildren, context, parse("Boolean"));
		//		testingDifferentModelsWithTheSameStructure(factors, n, "BinaryTree4", printSGDPLL, threshold);

	}

	public static void testingDifferentModelsWithTheSameStructure(
			Triple<Set<Expression>,Context,Expression> factors,
			int n,
			String fileNameWithoutExtension, 
			boolean printSGDPLL,
			double threshold) {

		List<List<TupleOfData>> modelsToPrintInFile = new ArrayList<>();

		int seed = 1;
		ModelGenerator.setSeed(seed);
		for(int i = 0; i < n; i++){
			Model model = new Model(factors, theory, true);
			modelsToPrintInFile.add(
					testingAndExportingListWithDataFunction(
							fileNameWithoutExtension + "Incremental", 
							model, 
							true /* incremental */, 
							printSGDPLL, 
							threshold));
			println(i);
			//		seed+=10;
		}

		writingToFile(fileNameWithoutExtension + ".csv", modelsToPrintInFile);
	}

	/**
	 * This tests a model and, instead of printing information, 
	 * stores its in a list of data structures.
	 * Each element of the list corresponds to a iteration of the algorithm.
	 * @param modelName
	 * @param model
	 * @param parameter
	 * @return
	 */
	public static List<TupleOfData> testingAndExportingListWithDataFunction(
			String modelName, 
			Model model, 
			boolean incrementalversion, 
			boolean printSGDPLL,
			double threshold, 
			Integer... parameter){
		
		List<TupleOfData> result = new ArrayList<TupleOfData>();
	
		model.clearExploredGraph();
	
		Iterator<PartitionTree> bfsExpander = new BFS(model);
		final IncrementalAnytimeBeliefPropagationWithSeparatorConditioning sbp = 
				new IncrementalAnytimeBeliefPropagationWithSeparatorConditioning(model, bfsExpander);
	
		double totalTime = 0;
		double error = 1;
		if (threshold >= 1) {
			threshold = 0;
		}
		
		int id = 0;
		while (bfsExpander.hasNext() && error > threshold) {
			TupleOfData t = 
					doInferenceAndStoreInformation(
							() -> incrementalversion? sbp.expandAndComputeInference() : sbp.expandAndComputeInferenceByRebuildingPartitionTree(),
									sbp.getModel(),
									modelName,
									id++,
									totalTime,
									"S-BP",
									parameter);
			totalTime = t.totalTime;
			result.add(t);
			error = t.IntervalLength;
			println("...." + error);
		}
	
		if (printSGDPLL && !bfsExpander.hasNext()) {
			TupleOfData t = 
					doInferenceAndStoreInformation(
							() -> makeSingleElementBound(LVECalculation(model), true),
							model, 
							modelName, 
							id++,
							totalTime,
							"SGDPLL",
							parameter) ;
			totalTime = t.totalTime;
			result.add(t);	
			println("SGDPLL computed");
	
			// computing S-BP in the whole model (it is weirdly being faster than SGDPLL in almost any case!) 
			model.clearExploredGraph();
			bfsExpander = new BFS(model);
	
	
			t = 
					doInferenceAndStoreInformation(
							()-> sbp.inferenceOverEntireModel(),
							sbp.getModel(),
							modelName,id++,
							0,"S-BP over Entire Model",
							parameter);
	
			result.add(t);	
			println("S-BP over Entire Model computed");
		}
	
		println("------------------------- Done -----------------------------------");
		return result;
	}

	private static void testingAndPrintingOnScreen() {
		Model model = new Model(isingModel(7, 2, context, parse("Boolean")), theory, true);

		solvesModelBySBPAndBruteForce("IsingModel", model, true);

		model = new Model(lineModel(8, context, parse("Boolean")),theory, true);

		solvesModelBySBPAndBruteForce("lineModel", model, true);

		model = new Model(nTreeModel(9, 2, context, parse("Boolean")),theory, true);

		solvesModelBySBPAndBruteForce("nTreeModel", model, true);
	}

	/**
	 * Tests a given model and prints the results on the screen 
	 * @param modelName
	 * @param model
	 * @param printDetails - indicates whether to print all iterations or just the final result.
	 */
	private static void solvesModelBySBPAndBruteForce(String modelName, Model model, boolean printDetails) {
		Iterator<PartitionTree> bfsExpander = new BFS(model);
		IncrementalAnytimeBeliefPropagationWithSeparatorConditioning sbp = new IncrementalAnytimeBeliefPropagationWithSeparatorConditioning(model, bfsExpander);
		println("Exploring " + modelName);
		Bound inferenceResult = null;
		double totalTime = 0;
		while (!sbp.isAllExplored()) {
			long tStart = System.currentTimeMillis();
			inferenceResult = sbp.expandAndComputeInference();
			long tEnd = System.currentTimeMillis();
			long tDelta = tEnd - tStart;
			double time = tDelta / 1000.0;	
			totalTime += time;
	
			// ModelGenerator.printModel(m, false);
			if (printDetails) {
				println("Number of extreme points: " + inferenceResult.getArguments().size());
				println("Extreme points: " + inferenceResult.getArguments());
				Pair<Double, Double> minAndMaxProbabilityofQueryequalsTrue = ModelGenerator.MaxMinProbability(inferenceResult, model);
				println("Minimal probability of query = true: " +
						minAndMaxProbabilityofQueryequalsTrue.first +
						"\nMaximal probability of query = true:" +
						minAndMaxProbabilityofQueryequalsTrue.second +
						"\nLength of interval (that is, (max - min)): " + (minAndMaxProbabilityofQueryequalsTrue.second - minAndMaxProbabilityofQueryequalsTrue.first) +
						"\nTime to compute this iteration: " + time + ". Total time: " + totalTime);
				println("----------------- Exploration is completed: " + sbp.isAllExplored() + "-----------------");
				println();
			}
		}
	
		if ( ! printDetails) {
			println(inferenceResult);
		}
		
		println("Computation by brute force with expresso: ");
		long start = System.currentTimeMillis();
		Expression bruteForceResult = ModelGenerator.LVECalculation(model);
		long end = System.currentTimeMillis();
		long timeInMilliseconds = end - start;
		double time = timeInMilliseconds / 1000.0;	
	
		println(bruteForceResult + "\n" + "\nTime to compute:" + time);
	}

	private static void testingBFS() {
		Model m = new Model(isingModel(4,3, context, parse("Boolean")),theory, true);
		Iterator<PartitionTree> bfsExpander = new BFS(m);

		while (bfsExpander.hasNext()) {
			PartitionTree p = bfsExpander.next();
			println("node : " + p.node);
			if (p.parent != null) {
				println("parent: " + p.parent.node);
			}

			println("children: ");
			for (PartitionTree pv : p.children) {
				println(pv.node.getValue());
			}
		}
	}

	/**
	 * This prints in a file the content of trying many different models.
	 * the idea is to test many different models (with teh function {@code testing}) and have them printed in the same .csv  file
	 * @param filename
	 * @param testedModels
	 */
	public static void writingToFile(String filename, List<List<TupleOfData>> testedModels) {
		try{
			PrintWriter writer = new PrintWriter(filename, "UTF-8");
			// print head of dataset
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
			// printLines
			for (List<TupleOfData> l : testedModels) {		    
				for (TupleOfData t : l) {
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

	private static TupleOfData doInferenceAndStoreInformation(NullaryFunction<Bound> doInference,
			Model m,
			String modelName, int id, double tTotalTime,
			String typeOfComputationUsed,
			Integer... parameter){
		TupleOfData t = new TupleOfData();

		long tStart = System.currentTimeMillis();
		Bound inferenceResult;

		inferenceResult = doInference.apply();

		long tEnd = System.currentTimeMillis();
		long tDelta = tEnd - tStart;
		t.time = tDelta /1000.0;

		tTotalTime += tDelta / 1000.0;
		t.totalTime +=  tTotalTime;

		t.typeOfComputationUsed = typeOfComputationUsed;
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
		return t;
	}

}