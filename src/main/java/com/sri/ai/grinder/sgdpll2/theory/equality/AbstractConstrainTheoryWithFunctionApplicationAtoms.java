package com.sri.ai.grinder.sgdpll2.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.pickUniformly;

import java.util.Collection;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.sgdpll2.core.constraint.AbstractConstraintTheory;
import com.sri.ai.util.collect.PredicateIterator;

public abstract class AbstractConstrainTheoryWithFunctionApplicationAtoms extends AbstractConstraintTheory {

	/**
	 * Indicates whether the theory can safely assume that all applications of its theory functors are literals in it.
	 */
	protected boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory;

	/**
	 * The simplifier for the theory; must simplify short-circuited applications of the theory functors at a minimum.
	 */
	protected Simplifier simplifier;
	
	/**
	 * The strings of the theory functors
	 */
	protected Collection<String> theoryFunctors;

	/**
	 * Indicates whether an argument to the theory functors is a valid argument to form a literal in this theory.
	 * By default, theory functor arguments are only tested by this method if {@link #assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory} is true.
	 * On the other hand, {@link #isSuitableFor(Expression, RewritingProcess)} always uses this test
	 * for deciding whether this theory is suitable for a variable (which is passed as the argument here).
	 * @param expression
	 * @param process
	 * @return
	 */
	protected abstract boolean isValidArgument(Expression expression, RewritingProcess process);

	/**
	 * Must takes a non-trivial atom in the theory and return its negation.
	 * @param atom
	 * @return
	 */
	protected abstract Expression getNonTrivialAtomNegation(Expression atom);

	public AbstractConstrainTheoryWithFunctionApplicationAtoms(
			boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory,
			Simplifier simplifier,
			Collection<String> theoryFunctors) {
		super();
		this.assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory = assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory;
		this.simplifier = simplifier;
		this.theoryFunctors = theoryFunctors;
	}

	/**
	 * Indicates whether an application is a function application of one of the theory functors.
	 * @param expression
	 * @return
	 */
	protected boolean isApplicationOfTheoryFunctor(Expression expression) {
		boolean result = expression.getFunctor() != null && theoryFunctors.contains(expression.getFunctor().toString());
		return result;
	}

	@Override
	public Expression simplify(Expression expression, RewritingProcess process) {
		Expression result = simplifier.apply(expression, process);
		return result;
	}

	/**
	 * Implements decision about whether theory is suitable for a variable by checking if it is
	 * a valid argument for a functor in the theory using {@link #isValidArgument(Expression, RewritingProcess)}. 
	 */
	@Override
	public boolean isSuitableFor(Expression variable, RewritingProcess process) {
		boolean result = isValidArgument(variable, process);
		return result;
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression, RewritingProcess process) {
		boolean result = isApplicationOfTheoryFunctor(expression) || theoryFunctors.contains(expression.toString()); 
		return result;
	}

	/**
	 * Implements decision of whether an expression is a non-trivial literal by checking
	 * if it is a function application of one of the theory functors and,
	 * if {@link #assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory} is true,
	 * whether its arguments are valid according to {@link #isValidArgument(Expression, RewritingProcess)}.
	 */
	@Override
	public boolean isNonTrivialLiteral(Expression expression, RewritingProcess process) {
		boolean result;
	
		boolean hasTheoryFunctor = isApplicationOfTheoryFunctor(expression);
		
		if (assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory) {
			result = hasTheoryFunctor;
		}
		else {
			// the following is good, but expensive
			result = hasTheoryFunctor
					&&
					forAll(expression.getArguments(), e -> isValidArgument(e, process));
		}
		
		return result;
	}

	/**
	 * Makes a random atom by uniformly picking among the theory functors and testing variables.
	 */
	@Override
	public Expression makeRandomAtomOn(String variable, Random random, RewritingProcess process) {
		Map<String, String> variablesAndTypes = getVariableNamesAndTypeNamesForTesting();
		String typeName = variablesAndTypes.get(variable);
		Set<String> allVariables = variablesAndTypes.keySet();
		PredicateIterator<String> isNameOfVariableOfSameType = PredicateIterator.make(allVariables, s -> variablesAndTypes.get(s).equals(typeName));
		Expression otherTerm;
		if (random.nextBoolean()) {
			otherTerm = makeSymbol(pickUniformly(isNameOfVariableOfSameType, random));
		}
		else {
			otherTerm = process.getType(typeName).sampleConstant(random);
		}
		
		String functor = pickUniformly(theoryFunctors, random);
		
		Expression possiblyTrivialAtom =
				random.nextBoolean()?
						apply(functor, variable, otherTerm) : apply(functor, otherTerm, variable);
						
		Expression result = simplify(possiblyTrivialAtom, process);
				
		return result;
	}

	/**
	 * Default implementation taking care of negations (simply returning the negation's argument)
	 * and <code>true</code> and <code>false</code> constants,
	 * referring to {@link #getNonTrivialAtomNegation(Expression atom, RewritingProcess process)}
	 * for all remaining cases.
	 * Throws an error if none of these cases applies.
	 */
	@Override
	public Expression getLiteralNegation(Expression literal, RewritingProcess process) {
		Expression result = getNonTrivialAtomNegation(literal); 
		
		if (result == null) {
			if (literal.hasFunctor(NOT) && isApplicationOfTheoryFunctor(literal.get(0))) {
				result = literal;
			}
			else if (literal.equals(TRUE)) {
				result = FALSE;
			} 
			else if (literal.equals(FALSE)) {
				result = TRUE;
			} 
			else {
				throw new Error("Invalid literal: " + literal);
			}
		}
		
		return result;
	}
}