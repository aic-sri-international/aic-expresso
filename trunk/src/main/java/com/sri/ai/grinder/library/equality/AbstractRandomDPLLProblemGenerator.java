package com.sri.ai.grinder.library.equality;

import java.util.List;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.collect.EZIterator;

/**
 * An iterator of random DPLL-style problem expressions, generated according to certain parameters
 * (see {@link #AbstractRandomDPLLProblemGenerator(Random, int, int, int, int, int)} for their description).
 * 
 * @author braz
 *
 */
public abstract class AbstractRandomDPLLProblemGenerator extends EZIterator<Expression> {

	protected Random random;
	protected int numberOfVariables;
	protected int minimumNumberOfIndices;
	protected RandomEqualityFormulaGenerator randomFormulaGenerator;

	protected abstract Expression makeProblem(Expression formula, List<Expression> indices);

	/**
	 * Creates an iterator over random DPLL-style problem expressions.
	 * This is done by sampling a random formula F from {@link RandomEqualityFormulaGenerator},
	 * picking a random set of supportedIndices I, and returning {{ (on I) (I) | F }}.
	 * 
	 * @param random a {@link Random} number generator.
	 * @param numberOfVariables the (maximum) number of variables in the formula.
	 * @param numberOfConstants the (maximum) number of constants in the formula.
	 * @param minimumNumberOfIndices minimum number of variables used as supportedIndices (maximum is <i>all</i> variables).
	 * @param depth the depth of the formula (all its sub-expressions with have depth equal to <code>depth - 1</code>).
	 * @param breadth the number of sub-expressions of conjunctions and disjunctions.
	 */
	public AbstractRandomDPLLProblemGenerator(Random random, int numberOfVariables, int numberOfConstants, int minimumNumberOfIndices, int depth, int breadth) {
		super();
		this.random = random;
		this.numberOfVariables = numberOfVariables;
		this.minimumNumberOfIndices = minimumNumberOfIndices;
		this.randomFormulaGenerator = new RandomEqualityFormulaGenerator(random, numberOfVariables, numberOfConstants, depth, breadth);
	}

	@Override
	protected Expression calculateNext() {
		Expression formula       = randomFormulaGenerator.next();
		int numberOfIndices      = random.nextInt(numberOfVariables - minimumNumberOfIndices + 1) + minimumNumberOfIndices;
		List<Expression> indices = randomFormulaGenerator.getVariables().subList(0, numberOfIndices);
		Expression problem = makeProblem(formula, indices);
		return problem;
	}
}