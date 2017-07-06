package anytimeExactBeliefPropagation;

import static anytimeExactBeliefPropagation.ModelGenerator.IsingModel;
import static anytimeExactBeliefPropagation.ModelGenerator.lineModel;
import static anytimeExactBeliefPropagation.ModelGenerator.nTreeModel;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;

import java.util.Iterator;

import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
import com.sri.ai.util.base.Pair;

import anytimeExactBeliefPropagation.Model.BFS;
import anytimeExactBeliefPropagation.Model.Model;
import anytimeExactBeliefPropagation.Model.Node.FactorNode;

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

		String modelName = "Ising Model";
		Model m = new Model(IsingModel(4, 4, context, parse("Boolean")),theory, true);
		
		testFunction(modelName, m,true);
		
		modelName = "Line Model";
		m = new Model(lineModel(10, context, parse("Boolean")),theory, true);
		
		testFunction(modelName, m,true);
		
		modelName = "Binary Tree Model";
		m = new Model(nTreeModel(5, 2, context, parse("Boolean")),theory, true);
		
		testFunction(modelName, m,true);
		
		modelName = "Random Model";
		m = new Model(ModelGenerator.randomModel(10, 10, context, parse("Boolean")),theory, true);
		
		testFunction(modelName, m,true);

	}

	private static void testFunction(String modelName, Model m, boolean printAll) {
		Iterator<FactorNode> BFSExpander = new BFS(m);
		BeliefPropagationWithConditioning sbp = new BeliefPropagationWithConditioning(m);
		println("Exploring " + modelName);
		Bound inferenceResult = null;
		while(BFSExpander.hasNext()){
			inferenceResult = sbp.ExpandAndComputeInference(BFSExpander);
			//ModelGenerator.printModel(m, false);
			if(printAll){
				println("Number of ExtremePoints : "+inferenceResult.getArguments().size());
				Pair<Double, Double> minAndMaxProbabilityofQueryequalsTrue = ModelGenerator.MaxMinProbability(inferenceResult, m);
				println("Minimal probability of Query = true : " +
						minAndMaxProbabilityofQueryequalsTrue.first +
						"\nMaximal probability of Query = true :" +
						minAndMaxProbabilityofQueryequalsTrue.second +
						"\nLength of interval (that is, (max - min)) : " + (minAndMaxProbabilityofQueryequalsTrue.second - minAndMaxProbabilityofQueryequalsTrue.first));
				println("----------------- AllExplored : " + m.AllExplored() + "-----------------");
			}
		}

		if(!printAll)
			println(inferenceResult);
		println("Computation with SGDPLL");
		println(ModelGenerator.LVECalculation(m) + "\n");
	}
	
	
	//public Pair<double,T> computeTimeAndReturnResults(T t){
		
	//}
}



/*		////Testing Nodes
VariableNode variable = new VariableNode(makeSymbol("A"), true, theory, context);
FactorNode factor = new FactorNode(parse("if A then 1 else 2"), true, theory, context);

println(variable + "\n" + factor + "\n");
*/		

/*		//////Testing IsingModel creation
Pair<Set<Expression>,Context> IsingM = ModelGenerator.IsingModel(2, 2, theory, context, parse("Boolean"));
ModelGenerator.printModel(IsingM.first);
context = IsingM.second;
println("");
//
println("Printing Ising Model");
Model m = new Model(IsingM.first, theory, context, true, parse("A_0_0"));
ModelGenerator.printModel(m,true);
println("");

////// Testing ExpandModel - Using a naive choice of expansion for that
println("Printing Naive expansion");
Iterator<FactorNode> NaiveGraphExpander = m.getEntireGraph().getBs().iterator();

while(NaiveGraphExpander.hasNext()){
	m.ExpandModel(NaiveGraphExpander);
	ModelGenerator.printModel(m, false);
	println("-----------------");
}
*/		
/*		//Testing if contains works for equivalent but not equal nodes. (that is, if hashCode overwritten went well)
Collection<VariableNode> vars = m.getEntireGraph().getAs();
boolean f = vars.contains(m.getQuery());
println(f);
*/
/*
//Testing BFS
println("Printing BFS expansion");
m.clearExploredGraph();
Iterator<FactorNode> BFSExpander = new BFS<FactorNode, VariableNode>(m.getEntireGraph(), m.getQuery());

while(BFSExpander.hasNext()){
	m.ExpandModel(BFSExpander);
	ModelGenerator.printModel(m, false);
	println("-----------------");
}

*/
/*		//Testing Partition
//// Testing creating Partition
PartitionTree p = new PartitionTree(m.getQuery());
p.CreatePartitionTreeWithBFS(m);
p.printTree(false);
//// Testing  all partition
p = new PartitionTree(m.getQuery(),m);
p.printTree(true);
//Testing computing separator

println("Testing computing separator");
BeliefPropagationWithConditioning sbp = new BeliefPropagationWithConditioning(m);
sbp.partitionTree.printTree(true);

println(sbp.ComputeSeparator(sbp.partitionTree));

//Testing BP
println(sbp.inference());
println(ModelGenerator.LVECalculation(m));

//Testing lineModel
ModelGenerator.printModel(ModelGenerator.lineModel(5, theory, context, parse("Boolean")).first);
//TODO Bound product should accept sets as input
ModelGenerator.printModel(ModelGenerator.nTreeModel(4, 2, theory, context,parse("Boolean")).first);
//One can implement its own Iterator
//Iterator<FactorNode> NaiveGraphExpander = new Iterator<FactorNode>() {
//	
//	Collection<FactorNode> factors = m.getEntireGraph().getBs();
//	
//	@Override
//	public boolean hasNext() {
//		return factors.size()>0;
//	}
//
//	@Override
//	public FactorNode next() {
//		
//		return null;
//	}
//	
//}; 
*/		


/*		Pair<Set<Expression>,Context> tree = ModelGenerator.nTreeModel(3, 3, theory, context, parse("Boolean"));
Model m = new Model(tree.first, theory, tree.second, true, parse("A_0_0"));


Iterator<FactorNode> BFSExpander = new BFS<FactorNode, VariableNode>(m.getEntireGraph(), m.getQuery());

while(BFSExpander.hasNext()){
	m.ExpandModel(BFSExpander);
//	ModelGenerator.printModel(m, false);
	println("-----------------");
}


BeliefPropagationWithConditioning sbp = new BeliefPropagationWithConditioning(m);
ModelGenerator.printModel(tree.first);
println(sbp.inference());
println(ModelGenerator.LVECalculation(m));
println("d");
*/
