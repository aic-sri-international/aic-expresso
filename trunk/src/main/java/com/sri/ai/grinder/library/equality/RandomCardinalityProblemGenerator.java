package com.sri.ai.grinder.library.equality;

import java.util.List;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.util.collect.EZIterator;

/**
 * An iterator of random cardinality expressions, generated according to certain parameters
 * (see {@link #RandomCardinalityProblemGenerator(int, int, int, int)} for their description).
 * 
 * @author braz
 *
 */
public class RandomCardinalityProblemGenerator extends EZIterator<Expression> {
	
	private Random random;
	private int numberOfVariables;
	private RandomEqualityFormulaGenerator randomFormulaGenerator;

	/**
	 * Creates an iterator over random cardinality expressions.
	 * This is done by sampling a random formula F from {@link RandomEqualityFormulaGenerator},
	 * picking a random set of indices I, and returning {{ (on I) (I) | F }}.
	 * 
	 * @param random a {@ink Random} number generator.
	 * @param numberOfVariables the (maximum) number of variables in the formula.
	 * @param numberOfConstants the (maximum) number of constants in the formula.
	 * @param depth the depth of the formula (all its sub-expressions with have depth equal to <code>depth - 1</code>).
	 * @param breadth the number of sub-expressions of conjunctions and disjunctions.
	 */
	public RandomCardinalityProblemGenerator(Random random, int numberOfVariables, int numberOfConstants, int depth, int breadth) {
		super();
		this.random = random;
		this.numberOfVariables = numberOfVariables;
		this.randomFormulaGenerator = new RandomEqualityFormulaGenerator(random, numberOfVariables, numberOfConstants, depth, breadth);
	}

	@Override
	protected Expression calculateNext() {
		Expression formula       = randomFormulaGenerator.next();
		int numberOfIndices      = random.nextInt(numberOfVariables);
		List<Expression> indices = randomFormulaGenerator.getVariables().subList(0, numberOfIndices);
		Expression tuple         = Tuple.make(indices);
		Expression set           = IntensionalSet.makeMultiSetFromIndexExpressionsList(indices, tuple, formula);
		Expression cardinality   = Expressions.apply(FunctorConstants.CARDINALITY, set);
		return cardinality;
	}

	/** A simple test sampling 10 random cardinality expressions and printing them to the standard output. */
	public static void main(String[] args) {
		RandomCardinalityProblemGenerator iterator = new RandomCardinalityProblemGenerator(new Random(), 10, 5, 2, 2);
		for (int i = 0; i != 10; i++) {
			System.out.println(iterator.next());	
		}
	}
}
