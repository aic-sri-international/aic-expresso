package com.sri.ai.grinder.plaindpll.core;

import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.util.Util.throwSafeguardError;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.theory.AbstractOwnRepresentationConstraint;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.util.Util;

/**
 * An abstract {@link Constraint} implementation that lays the groundwork for
 * computing model counts based on the rule of product, that is,
 * iterating over each index, computing the number of possible values for it
 * given the previous choices, and multiplying them all.
 *  
 * @author braz
 *
 */
@SuppressWarnings("serial")
public abstract class AbstractRuleOfProductConstraint extends AbstractOwnRepresentationConstraint {

	static final private Times timesRewriter = new Times();

	public AbstractRuleOfProductConstraint(Collection<Expression> supportedIndices) {
		super(supportedIndices);
	}
	
	public abstract AbstractRuleOfProductConstraint clone();
	
	/**
	 * Given an index x, return one splitter needed for us to be able to
	 * compute this index's number of values, or null if none is needed.
	 * Only required if using default implementation of {@link #pickSplitter(Collection<Expression>, RewritingProcess)} (that is, not overriding it).
	 */
	protected Expression pickSplitterFor(Expression x, RewritingProcess process) {
		throwSafeguardError(
				getClass().getSimpleName(),
				"pickSplitterFor", // thisClassName
				"AbstractConstraintTheory.AbstractConstraint", // superClassName
				"pickSplitter"); // namesOfMethodsWhoseDefaultImplementationUsesThisMethod
		return null; // never used, as safeguardCheck throws an error no matter what.
	}

	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
		for (Expression x : indicesSubSet) {
			Expression splitter = pickSplitterFor(x, process);
			if (splitter != null) {
				return splitter;
			}
		}
		return null;
	}

	protected Collection<Expression> getSplittersToBeSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
		throwSafeguardError(
				getClass().getSimpleName(),
				"getSplittersToBeSatisfied", // thisClassName
				"AbstractConstraintTheory.AbstractConstraint", // superClassName
				"modelCount"); // namesOfMethodsWhoseDefaultImplementationUsesThisMethod
		return null; // never used, as safeguardCheck throws an error no matter what.
	}

	protected Collection<Expression> getSplittersToBeNotSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
		throwSafeguardError(
				getClass().getSimpleName(),
				"getSplittersToBeNotSatisfied", // thisClassName
				"AbstractConstraintTheory.AbstractConstraint", // superClassName
				"modelCount"); // namesOfMethodsWhoseDefaultImplementationUsesThisMethod
		return null; // never used, as safeguardCheck throws an error no matter what.
	}

	@Override
	public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Expression unconditionalCount = computeModelCountGivenConditionsOnVariablesNotIn(indicesSubSet, process);
		Expression result =
				makeModelCountConditionedOnUndeterminedSplittersNotAlreadyImpliedByContextualConstraint(
						unconditionalCount,
						getSplittersToBeSatisfied(indicesSubSet, process), getSplittersToBeNotSatisfied(indicesSubSet, process),
						process);
		return result;
	}
	
	/**
	 * Returns an expression (in the free variables) for the number of possible values for the given index,
	 * assuming that {@link #pickSplitterFor(Expression, RewritingProcess)}
	 * currently returns <code>null</code>,
	 * that is, we do not need anything splitters to be either imposed or negated in order to compute that.
	 * Only required if using default implementation of {@link #computeModelCountGivenConditionsOnFreeVariables(Expression index, RewritingProcess)} (that is, not overriding it).
	 */
	protected Expression computeNumberOfPossibleValuesFor(Expression index, RewritingProcess process) {
		throwSafeguardError(
				getClass().getSimpleName(),
				"computeNumberOfPossibleValuesFor", // thisClassName
				"AbstractConstraintTheory.AbstractConstraint", // superClassName
				"computeModelCountGivenConditionsOnVariablesNotIn"); // namesOfMethodsWhoseDefaultImplementationUsesThisMethod
		return null; // never used, as safeguardCheck throws an error no matter what.
	}

	protected Expression computeModelCountGivenConditionsOnVariablesNotIn(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Util.myAssert(() -> getSupportedIndices().containsAll(indicesSubSet), () -> "in " + getClass().getSimpleName() + ".computeModelCountGivenConditionsOnVariablesNotIn, indicesSubSet must be a sub-set of getIndices(), but " + indicesSubSet + " is not a sub-set of " + getSupportedIndices());

		List<Expression> numberOfPossibleValuesForIndicesSoFar = new LinkedList<Expression>();
		
		for (Expression index : indicesSubSet) {
			Expression numberOfPossibleValuesForIndex = computeNumberOfPossibleValuesFor(index, process);
			numberOfPossibleValuesForIndicesSoFar.add(numberOfPossibleValuesForIndex);
		}
		
		Expression result = Times.make(numberOfPossibleValuesForIndicesSoFar);
		result = AbstractRuleOfProductConstraint.timesRewriter.rewrite(result, process);
		return result;
	}

	/**
	 * Receives the model count for the case in which a certain set of splitter is satisfied, and another is unsatisfied,
	 * and returns conditional model count including the cases in which those conditions are not true
	 * (which entail model count 0),
	 * taking into account the contextual constraint.
	 */
	private Expression makeModelCountConditionedOnUndeterminedSplittersNotAlreadyImpliedByContextualConstraint(
			Expression modelCountGivenUndeterminedSplitters,
			Collection<Expression> splittersToBeSatisfied,
			Collection<Expression> splittersToBeUnsatisfied,
			RewritingProcess process) {
		
		Collection<Expression> undeterminedSplittersThatNeedToBeTrue = DPLLUtil.keepSplittersUnsatisfiedByContextualConstraint(splittersToBeSatisfied, process);
		Collection<Expression> undeterminedSplittersThatNeedToBeFalse = DPLLUtil.keepSplitterTheNegationsOfWhichAreUnsatisfiedByContextualConstraint(splittersToBeUnsatisfied, process);
		
		Expression result = conditionExpressionOnGivenSplitters(
				modelCountGivenUndeterminedSplitters, undeterminedSplittersThatNeedToBeTrue, undeterminedSplittersThatNeedToBeFalse);
		return result;
	}

	/**
	 * Receives an expression and conditions it on a set of splitters required to be true,
	 * and another set of splitters required to be false.
	 */
	private Expression conditionExpressionOnGivenSplitters(
			Expression expression,
			Collection<Expression> splittersThatNeedToBeTrue,
			Collection<Expression> splittersThatNeedToBeFalse) {
		
		Expression result = expression;
		for (Expression splitterToBeNotSatisfied : splittersThatNeedToBeFalse) {
			result = IfThenElse.make(splitterToBeNotSatisfied, ZERO, result, false);
		}
		for (Expression splitterToBeSatisfied : splittersThatNeedToBeTrue) {
			result = IfThenElse.make(splitterToBeSatisfied, result, ZERO, false);
		}
		return result;
	}
}