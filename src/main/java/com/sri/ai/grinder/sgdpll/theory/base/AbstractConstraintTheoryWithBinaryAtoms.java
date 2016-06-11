package com.sri.ai.grinder.sgdpll.theory.base;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.util.Util.check;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.pickUniformly;

import java.util.Collection;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpll.core.constraint.AbstractConstraintTheory;
import com.sri.ai.grinder.sgdpll.simplifier.api.MapBasedSimplifier;
import com.sri.ai.util.collect.PredicateIterator;

public abstract class AbstractConstraintTheoryWithBinaryAtoms extends AbstractConstraintTheory {

	/**
	 * Indicates whether the theory can safely assume that all applications of its theory functors are atoms in it,
	 * regardless of their argument types (this spares the theory to do the checking).
	 */
	protected boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory;

	/**
	 * The strings of the theory functors
	 */
	protected Collection<String> theoryFunctors;

	/**
	 * Constructor taking the theory's functor strings,
	 * a boolean indicating whether any application of this functors, regardless of their arguments types,
	 * are to be considered atoms in this theory (for efficiency purposes),
	 * and a simplifier for these functions.
	 * @param theoryFunctors
	 * @param assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory
	 * @param simplifier
	 */
	public AbstractConstraintTheoryWithBinaryAtoms(
			Collection<String> theoryFunctors,
			boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory,
			MapBasedSimplifier simplifier) {
		super(simplifier);
		this.assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory = assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory;
		this.theoryFunctors = theoryFunctors;
	}

	/**
	 * Indicates whether an argument to the theory functors is a valid argument to form a literal in this theory.
	 * By default, the type of theory functor arguments is only tested by this method if
	 * {@link #assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory} is true.
	 * On the other hand, {@link #isSuitableFor(Expression, Type)} always uses this test
	 * for deciding whether this theory is suitable for a variable (which is passed as the argument here).
	 * @param expression
	 * @param type TODO
	 * @param context TODO
	 * @return
	 */
	protected abstract boolean isValidArgument(Expression expression, Type type, Context context);

	/**
	 * Must take a non-trivial atom in the theory and return its negation.
	 * @param atom
	 * @return
	 */
	protected abstract Expression getNonTrivialAtomNegation(Expression atom);

	/**
	 * Indicates whether an expression is a function application of one of the theory functors.
	 * @param expression
	 * @return
	 */
	protected boolean isApplicationOfTheoryFunctor(Expression expression) {
		boolean result = expression.getFunctor() != null && theoryFunctors.contains(expression.getFunctor().toString());
		return result;
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression, Context context) {
		boolean result = isApplicationOfTheoryFunctor(expression) || theoryFunctors.contains(expression.toString()); 
		return result;
	}

	/**
	 * Implements decision of whether an expression is a non-trivial atom by checking
	 * if it is a function application of one of the theory functors and,
	 * if {@link #assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory} is true,
	 * whether its arguments are valid according to {@link #isValidArgument(Expression, Type, Context)}.
	 */
	@Override
	public boolean isNonTrivialAtom(Expression expression, Context context) {
		boolean result;
	
		boolean hasTheoryFunctor = isApplicationOfTheoryFunctor(expression);
		
		if (assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory) {
			result = hasTheoryFunctor;
		}
		else {
			// the following is good, but expensive
			result = hasTheoryFunctor
					&&
					forAll(expression.getArguments(),
							e -> {
								Expression typeExpression = GrinderUtil.getType(e, context);
								check(() -> typeExpression != null, () -> e + " has undefined type");
								String typeName = typeExpression.toString();
								Type eType = context.getType(typeName);
								return isValidArgument(e, eType, context);
							});
		}
		
		return result;
	}

	/**
	 * Makes a random atom by uniformly picking among the theory functors and testing variables.
	 */
	@Override
	public Expression makeRandomAtomOn(String variable, Random random, Context context) {
		Map<String, Type> variablesAndTypes = getVariableNamesAndTypesForTesting();
		Type type = variablesAndTypes.get(variable);
		Set<String> allVariables = variablesAndTypes.keySet();
		PredicateIterator<String> isNameOfOtherVariableOfSameTypeAsMainVariable =
				PredicateIterator.make(allVariables, s -> {
					Type typeOfOther = variablesAndTypes.get(s);
					return typeOfOther.equals(type) || typeOfOther.toString().equals(type.toString());	
				});
		Expression otherTerm;
		if (random.nextBoolean()) {
			otherTerm = makeSymbol(pickUniformly(isNameOfOtherVariableOfSameTypeAsMainVariable, random));
		}
		else {
			otherTerm = type.sampleUniquelyNamedConstant(random);
		}
		
		String functor = pickUniformly(theoryFunctors, random);
		
		Expression possiblyTrivialAtom =
				random.nextBoolean()?
						apply(functor, variable, otherTerm) : apply(functor, otherTerm, variable);
						
		Expression result = simplify(possiblyTrivialAtom, context);
				
		return result;
	}

	/**
	 * Default implementation taking care of negations (simply returning the negation's argument)
	 * and <code>true</code> and <code>false</code> constants,
	 * referring to {@link #getNonTrivialAtomNegation(Expression atom, Context context)}
	 * for all remaining cases.
	 * Throws an error if none of these cases applies.
	 */
	@Override
	public Expression getLiteralNegation(Expression literal, Context context) {
		Expression result;
		
		if (literal.hasFunctor(NOT) && isApplicationOfTheoryFunctor(literal.get(0))) {
			result = literal.get(0);
		}
		else if (literal.equals(TRUE)) {
			result = FALSE;
		} 
		else if (literal.equals(FALSE)) {
			result = TRUE;
		} 
		else {
			result = getNonTrivialAtomNegation(literal);
		}
		
		return result;
	}
}