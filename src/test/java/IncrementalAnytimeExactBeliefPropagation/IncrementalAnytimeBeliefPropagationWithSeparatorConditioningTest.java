package IncrementalAnytimeExactBeliefPropagation;

import static IncrementalAnytimeExactBeliefPropagation.ModelGenerator.LVECalculation;
import static IncrementalAnytimeExactBeliefPropagation.ModelGenerator.isingModel;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.sgdpllt.library.bounds.Bounds.makeSingleElementBound;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.*;

import java.util.Iterator;
import java.util.Set;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
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
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.Triple;

import IncrementalAnytimeExactBeliefPropagation.Model.BFS;
import IncrementalAnytimeExactBeliefPropagation.Model.Model;

public class IncrementalAnytimeBeliefPropagationWithSeparatorConditioningTest {

	@Test
	public void test() {
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		Context context = new TrueContext(theory);	
		context = context.extendWithSymbolsAndTypes("A","Boolean");
	
		ModelGenerator.resetRandomGenerator();
		Triple<Set<Expression>,Context,Expression> factors = isingModel(2,2, context, parse("Boolean"));
		Model m = new Model(factors,theory,true);

		Iterator<PartitionTree> bfsExpander = new BFS(m);
		IncrementalAnytimeBeliefPropagationWithSeparatorConditioning sbp = new IncrementalAnytimeBeliefPropagationWithSeparatorConditioning(m, bfsExpander);
		Bound inferenceResult = null;

		NullaryFunction<Bound> doInference = ()->makeSingleElementBound(LVECalculation(m), true);
		inferenceResult = doInference.apply();
		Pair<Double, Double> SGDPLLPairResult = ModelGenerator.MaxMinProbability(inferenceResult, m);
		Double SGDPLLResult = SGDPLLPairResult.first;
		println(SGDPLLResult);
		
		
		
		
		Double LastBoundFirstElement = 0.0;
		Double LastBoundSecondElement = 0.0;
		
		while (!sbp.isAllExplored()) {
			inferenceResult = sbp.expandAndComputeInference();

			Pair<Double, Double> minAndMaxProbabilityofQueryequalsTrue = ModelGenerator.MaxMinProbability(inferenceResult, m);

			//Bounds included in [0,1]
			assertTrue(minAndMaxProbabilityofQueryequalsTrue.first >= 0);
			assertTrue(minAndMaxProbabilityofQueryequalsTrue.first <= minAndMaxProbabilityofQueryequalsTrue.second);
			assertTrue(minAndMaxProbabilityofQueryequalsTrue.second <= 1);
			
			//Bounds contained SGDPLL result
			assertTrue(minAndMaxProbabilityofQueryequalsTrue.first <= SGDPLLResult);
			assertTrue(SGDPLLResult <= minAndMaxProbabilityofQueryequalsTrue.second);
			
			LastBoundFirstElement = minAndMaxProbabilityofQueryequalsTrue.first;
			LastBoundSecondElement = minAndMaxProbabilityofQueryequalsTrue.second;
		}
		
		//Final Bound equals SGDPLL result
		assertEquals(LastBoundFirstElement, SGDPLLResult);
		assertEquals(LastBoundSecondElement, SGDPLLResult);
}

}
