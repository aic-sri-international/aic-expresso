package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.grinder.plaindpll.core.AbstractConstraintTheory.variableIsChosenAfterOtherTerm;
import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;
import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.core.AbstractRuleOfProductConstraint;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.util.Util;

/** 
 * A default implementation of {@link NonEqualitiesConstraint} that keeps a map from variables to {@link NonEqualitiesConstraintForSingleVariable} objects.
 */	
/**
 * @author braz
 *
 */
@SuppressWarnings("serial")
public abstract class AbstractNonEqualitiesConstraint extends AbstractRuleOfProductConstraint implements NonEqualitiesConstraint  {

	protected EqualityConstraintTheory theory;

	/**
	 * The main data structure of the class; maps variables to their individual NonEqualitiesConstraintForSingleVariable instances.
	 */
	protected Map<Expression, NonEqualitiesConstraintForSingleVariable> map = new LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable>();

	public AbstractNonEqualitiesConstraint(EqualityConstraintTheory theory, Collection<Expression> supportedIndices) {
		super(supportedIndices);
		this.theory = theory;
	}
	
	@Override
	public DisequalitiesConstraint clone() {
		DisequalitiesConstraint newNonEqualitiesConstraint = cloneWithoutNonEqualitiesForSingleVariables();
		copyClonedEntries(newNonEqualitiesConstraint);
		return newNonEqualitiesConstraint;
		// OPTIMIZATION perhaps for very large maps it makes sense to use an implementation
		// that keeps the original map around and clones the values as needed.
		// This implementation would be a little complicated/expensive so I am not sure it is worth it at this point.
	}

	/**
	 * Clones each entry of another object's {@link #map} to this own's.
	 * @param newNonEqualitiesConstraint
	 */
	protected void copyClonedEntries(DisequalitiesConstraint newNonEqualitiesConstraint) {
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : map.entrySet()) {
			NonEqualitiesConstraintForSingleVariable newEntryValue = entry.getValue().clone();
			newNonEqualitiesConstraint.map.put(entry.getKey(), newEntryValue);
		}
	}

	/**
	 * Makes a new object of this class (without putting anything in the inner map).
	 * @return a new instance.
	 */
	abstract protected DisequalitiesConstraint cloneWithoutNonEqualitiesForSingleVariables();

	public TermTheory getTermTheory() {
		return getTheory().getTermTheory();
	}

	@Override
	protected Expression pickSplitterFor(Expression index, RewritingProcess process) {
		Expression result = nonEqualitiesConstraintFor(index, process).pickSplitterGivenExternalConstraint(list(index), this, process);
		return result;
	}

	protected NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintFor(Expression variable, RewritingProcess process) {
		myAssert(() -> isVariableTerm(variable, process), () -> "nonEqualitiesConstraintFor must be invoked for a variable but was invoked on " + variable);
		NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintForTerm =
				getValuePossiblyCreatingIt(map, variable, key -> makeNonEqualitiesConstraintForVariable(key));
		return nonEqualitiesConstraintForTerm;
	}

	@Override
	public NonEqualitiesConstraintForSingleVariable removeNonEqualitiesForGivenVariableDestructively(Expression variable) {
		return map.remove(variable);
	}
	
	/**
	 * Indicates whether this constraint is such that a literal to be stored in a particular single-variable constraint
	 * must also be communicated to other single-variable constraints for simplification.
	 */
	abstract protected boolean singleVariableConstraintsMustBeInformedOfLiteralsStoredOnTheirSiblings();

	@Override
	public boolean directlyImpliesNonTrivialLiteral(Expression literal, RewritingProcess process) {
		myAssert(() -> literal.numberOfArguments() == 2, () -> (new Object(){}).getClass().getEnclosingMethod() + " requires binary literal but got " + literal);
		boolean result;
		Expression term1 = literal.get(0);
		Expression term2 = literal.get(1);
		Expression laterOneInChoosingOrder = getLaterOneInChoosingOrder(term1, term2, process);
		NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintForSingleVariable = nonEqualitiesConstraintFor(laterOneInChoosingOrder, process);
		result = nonEqualitiesConstraintForSingleVariable.directlyImpliesNonTrivialLiteral(literal, process);
		return result;
	}

	/**
	 * Equivalent to {@link #directlyImpliesLiteral(Expression, RewritingProcess)} on a disequality literal,
	 * but more efficient and not requiring the construction of a literal.
	 */
	@Override
	public boolean directlyImpliesDisequality(Expression term1, Expression term2, RewritingProcess process) {
		boolean result;
		if (term1.equals(term2)) {
			result = false;
		}
		else if (process.isUniquelyNamedConstant(term1) && process.isUniquelyNamedConstant(term2)) {
			result = true;
		}
		else {
			result = directlyImpliesNonTrivialDisequality(term1, term2, process);
		}
		return result;
	}

	@Override
	public boolean directlyImpliesNonTrivialDisequality(Expression term1, Expression term2, RewritingProcess process) {
		boolean result;
		if (firstTermComesLaterInChoiceOrder(term1, term2, process)) {
			result = directlyImpliesDisequalityBetweenVariableComingLaterInChoosingOrderAndAnotherTerm(term1, term2, process);
		}
		else {
			result = directlyImpliesDisequalityBetweenVariableComingLaterInChoosingOrderAndAnotherTerm(term2, term1, process);
		}
		return result;
	}

	private boolean directlyImpliesDisequalityBetweenVariableComingLaterInChoosingOrderAndAnotherTerm(Expression laterVariable, Expression anotherTerm, RewritingProcess process) {
		boolean result;
		NonEqualitiesConstraintForSingleVariable constraintsOnTerm1 = map.get(laterVariable); // directly consult map instead of using nonEqualitiesConstraintForSingleVariable to avoid unnecessary default construction of single variable constraint
		if (constraintsOnTerm1 != null && constraintsOnTerm1.directlyImpliesDisequalityOfVariableAnd(anotherTerm, process)) {
			result = true;
		}
		else {
			result = false;
		}
		return result;
	}

	/**
	 * Default implementation that passes the splitter to the appropriate {@link NonEqualitiesConstraintForSingleVariable}
	 * using {@link NonEqualitiesConstraintForSingleVariable#incorporateDestructively(boolean, Expression, com.sri.ai.grinder.plaindpll.api.Constraint, RewritingProcess)},
	 * and then informs others, via {@link NonEqualitiesConstraintForSingleVariable#informDestructively(Expression, RewritingProcess)},
	 * but only if {@link #singleVariableConstraintsMustBeInformedOfLiteralsStoredOnTheirSiblings()} returns <code>true</code>.
	 */
	@Override
	public void incorporateNonTrivialNormalizedSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Expression variable  = splitter.get(0);
		Expression otherTerm = splitter.get(1);
		NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintForSingleVariableToBeUsed;
		if (firstTermComesLaterInChoiceOrder(variable, otherTerm, process)) {
			nonEqualitiesConstraintForSingleVariableToBeUsed = nonEqualitiesConstraintFor(variable, process);
		}
		else { // otherTerm must be a variable because it comes later than variable in ordering, and only other variables do that
			nonEqualitiesConstraintForSingleVariableToBeUsed = nonEqualitiesConstraintFor(otherTerm, process);
		}
		nonEqualitiesConstraintForSingleVariableToBeUsed.incorporateDestructively(splitterSign, splitter, this, process);
		
//		// TODO: notify other single-variable constraints if needed
//		if (singleVariableConstraintsMustBeInformedOfLiteralsStoredOnTheirSiblings()) {
//			Map<Expression, NonEqualitiesConstraintForSingleVariable> newEntries = new LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable>();
//			for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : map.entrySet()) {
//				
//			}
//		}
	}

	@Override
	public void incorporateDisequalityDestructively(Expression term1, Expression term2, RewritingProcess process) {
		if (term1.equals(term2)) {
			throw new Contradiction();
		}
		else if (getTermTheory().isVariableTerm(term1, process) || getTermTheory().isVariableTerm(term2, process)) {
			incorporateNonTrivialDisequalityOfVariableAndAnotherTermDestructively(term1, term2, process);
		}
	}

	protected void incorporateNonTrivialDisequalityOfVariableAndAnotherTermDestructively(Expression variable, Expression otherTerm, RewritingProcess process) {
		if (firstTermComesLaterInChoiceOrder(variable, otherTerm, process)) {
			incorporateFirstTermAsDisequalOfSecondTermDestructively(variable, otherTerm, process);
		}
		else { // term2 must be a variable because either term1 is not a variable, or it is but term2 comes later than term1 in ordering, which means it is a variable
			incorporateFirstTermAsDisequalOfSecondTermDestructively(otherTerm, variable, process);
		}
	}

	private boolean firstTermComesLaterInChoiceOrder(Expression term1, Expression term2, RewritingProcess process) {
		boolean result = isVariableTerm(term1, process) && variableIsChosenAfterOtherTerm(term1, term2, getSupportedIndices(), process);
		return result;
	}

	private Expression getLaterOneInChoosingOrder(Expression term1, Expression term2, RewritingProcess process) {
		return firstTermComesLaterInChoiceOrder(term1, term2, process)? term1 : term2;
	}

	private boolean isVariableTerm(Expression term, RewritingProcess process) {
		return getTheory().isVariableTerm(term, process);
	}

	protected abstract void incorporateFirstTermAsDisequalOfSecondTermDestructively(Expression term1, Expression term2, RewritingProcess process);

	/**
	 * Given a function mapping each term either to itself or to another term meant to represent it
	 * (determined, most likely, by a system of equalities somewhere),
	 * apply it to the present constraint, possibly destructively if that means much better performance.
	 * Terms with distinct representatives should not appear in the resulting constraint.
	 * @param getRepresentative
	 * @param process
	 */
	@Override
	public void updateRepresentativesDestructively(Function<Expression, Expression> getRepresentative, RewritingProcess process) {
		// copies entries to a list because original one is altered during iteration.
		List<Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable>> entriesHolder = new LinkedList<Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable>>(map.entrySet());
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : entriesHolder) {
			NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintForSingleVariable = entry.getValue();
			nonEqualitiesConstraintForSingleVariable.updateRepresentativesDestructively(getRepresentative, this, process);
		}
	}

	/**
	 * Indicates whether the constraints represented ever product splitters that need to be satisfied (default is true).
	 * @return
	 */
	abstract protected boolean constraintsDoNotProduceSplittersToBeSatisfied();
	
	/**
	 * Indicates whether the constraints represented ever product splitters that need to be <i>not</i> satisfied (default is false).
	 * @return
	 */
	abstract protected boolean constraintsDoNotProduceSplittersToBeNotSatisfied();

	protected Collection<Expression> getSplittersToBeSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Collection<Expression> result = new LinkedList<Expression>();
		if ( ! constraintsDoNotProduceSplittersToBeSatisfied()) {
			for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : map.entrySet()) {
				Util.myAssert(() -> getTermTheory().isVariableTerm(entry.getKey(), process), () -> "Key in map for NonEqualitiesConstraints is not a variable, but " + entry.getKey());
				Expression variable = entry.getKey();
				if ( ! indicesSubSet.contains(variable)) { // if variable is free
					NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintForSingleVariable = entry.getValue();
					List<Expression> subResult = nonEqualitiesConstraintForSingleVariable.getSplittersToBeSatisfied();
					result.addAll(subResult);
				}
			}
		}
		return result;
	}

	protected Collection<Expression> getSplittersToBeNotSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Collection<Expression> result = new LinkedHashSet<Expression>();
		if ( ! constraintsDoNotProduceSplittersToBeNotSatisfied()) {
			for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : map.entrySet()) {
				Util.myAssert(() -> getTermTheory().isVariableTerm(entry.getKey(), process), () -> "Key in map for NonEqualitiesConstraints is not a variable, but " + entry.getKey());
				Expression variable = entry.getKey();
				if ( ! indicesSubSet.contains(variable)) { // if variable is free
					NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintForSingleVariable = entry.getValue();
					List<Expression> subResult = nonEqualitiesConstraintForSingleVariable.getSplittersToBeNotSatisfied();
					result.addAll(subResult);
				}
			}
		}
		else {
			result = emptyList();
		}
		return result;
	}

	@Override
	protected Expression computeNumberOfPossibleValuesFor(Expression index, RewritingProcess process) {
		return nonEqualitiesConstraintFor(index, process).modelCount(list(index), process);
	}
	
	abstract protected NonEqualitiesConstraintForSingleVariable makeNonEqualitiesConstraintForVariable(Expression variable);

	@Override
	public EqualityConstraintTheory getTheory() {
		return theory;
	}

	@Override
	public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
		Expression result;
		Expression term1 = splitter.get(0);
		Expression term2 = splitter.get(1);
		if (firstTermComesLaterInChoiceOrder(term1, term2, process)) {
			result = nonEqualitiesConstraintFor(term1, process).normalizeSplitterGivenConstraint(splitter, process);
		}
		else {
			result = nonEqualitiesConstraintFor(term2, process).normalizeSplitterGivenConstraint(splitter, process);
		}
		return result;
	}

	@Override
	public Expression normalizeExpressionWithoutLiterals(Expression expression, RewritingProcess process) {
		return expression;
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> conjuncts = new LinkedList<Expression>();
		conjuncts.addAll(map.values());
		Expression result = And.make(conjuncts);
		return result;
	}
}