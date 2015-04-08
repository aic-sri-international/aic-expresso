package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.plaindpll.core.AbstractConstraintTheory.variableIsChosenAfterOtherTerm;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoSetOrSameIfNoDistinctElementInstances;
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
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.core.AbstractRuleOfProductConstraint;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryPredicate;

/** 
 * A default implementation of {@link NonEqualitiesConstraint} that keeps a map from variables to {@link NonEqualitiesConstraintForSingleVariable} objects.
 */	
@SuppressWarnings("serial")
public class DefaultNonEqualitiesConstraint extends AbstractRuleOfProductConstraint implements NonEqualitiesConstraint  {

	private EqualityConstraintTheory theory;
	
	private Map<Expression, NonEqualitiesConstraintForSingleVariable> map = new LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable>();

	public DefaultNonEqualitiesConstraint(EqualityConstraintTheory theory, Collection<Expression> supportedIndices) {
		super(supportedIndices);
		this.theory = theory;
	}
	
	@Override
	public DefaultNonEqualitiesConstraint clone() {
		DefaultNonEqualitiesConstraint newNonEqualitiesConstraint = new DefaultNonEqualitiesConstraint(theory, supportedIndices);
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : map.entrySet()) {
			NonEqualitiesConstraintForSingleVariable newEntryValue = entry.getValue().clone();
			newNonEqualitiesConstraint.map.put(entry.getKey(), newEntryValue);
		}
		return newNonEqualitiesConstraint;
		// OPTIMIZATION perhaps for very large maps it makes sense to use an implementation
		// that keeps the original map around and clones the values as needed.
		// This implementation would be a little complicated/expensive so I am not sure it is worth it at this point.
	}

	public TermTheory getTermTheory() {
		return getTheory().getTermTheory();
	}

	@Override
	protected Expression pickSplitterFor(Expression index, RewritingProcess process) {
		BinaryPredicate<Expression, Expression> disequalityDirectlyImpliedExternally = (t1, t2) -> directlyImpliesDisequality(t1, t2, process);
		Expression result = nonEqualitiesConstraintFor(index, process).pickSplitter(list(index), disequalityDirectlyImpliedExternally, process);
		return result;
	}

	private NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintFor(Expression variable, RewritingProcess process) {
		Util.myAssert(() -> isVariableTerm(variable, process), () -> "nonEqualitiesConstraintFor must be invoked for a variable but was invoked on " + variable);
		NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintForTerm =
				Util.getValuePossiblyCreatingIt(map, variable, key -> makeNonEqualitiesConstraintForVariable(key));
		return nonEqualitiesConstraintForTerm;
	}

	@Override
	public boolean directlyImpliesNonTrivialLiteral(Expression literal, RewritingProcess process) {
		myAssert(() -> literal.hasFunctor(DISEQUALITY), "DefaultNonEqualitiesConstraint.directlyImplies must receive disequalities only");
		boolean result = directlyImpliesNonTrivialDisequality(literal.get(0), literal.get(1), process);
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
			result = directlyImpliesDisequalityBetweenVariableComingLaterInChoiceOrderAndAnotherTerm(term1, term2, process);
		}
		else {
			result = directlyImpliesDisequalityBetweenVariableComingLaterInChoiceOrderAndAnotherTerm(term2, term1, process);
		}
		return result;
	}

	private boolean directlyImpliesDisequalityBetweenVariableComingLaterInChoiceOrderAndAnotherTerm(Expression laterVariable, Expression anotherTerm, RewritingProcess process) {
		boolean result;
		NonEqualitiesConstraintForSingleVariable constraintsOnTerm1 = map.get(laterVariable);
		if (constraintsOnTerm1 != null && constraintsOnTerm1.directlyImpliesDisequalityOfVariableAnd(anotherTerm, process)) {
			result = true;
		}
		else {
			result = false;
		}
		return result;
	}

	@Override
	public void incorporateDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		myAssert( ! splitterSign, "DefaultNonEqualitiesConstraint.incorporatePossiblyDestructively must receive negative equalities (disequalities) only (for now)");
		Expression variable  = splitter.get(0);
		Expression otherTerm = splitter.get(1);
		addNonTrivialDisequalityOfVariableAndAnotherTermDestructively(variable, otherTerm, process);
	}

	private void incorporateDisequalityDestructively(Expression term1, Expression term2, RewritingProcess process) {
		if (term1.equals(term2)) {
			throw new Contradiction();
		}
		else if (getTermTheory().isVariableTerm(term1, process) || getTermTheory().isVariableTerm(term2, process)) {
			addNonTrivialDisequalityOfVariableAndAnotherTermDestructively(term1, term2, process);
		}
	}

	private void addNonTrivialDisequalityOfVariableAndAnotherTermDestructively(Expression variable, Expression otherTerm, RewritingProcess process) {
		if (firstTermComesLaterInChoiceOrder(variable, otherTerm, process)) {
			addFirstTermAsDisequalOfSecondTermDestructively(variable, otherTerm, process);
		}
		else { // term2 must be a variable because either term1 is not a variable, or it is but term2 comes later than term1 in ordering, which means it is a variable
			addFirstTermAsDisequalOfSecondTermDestructively(otherTerm, variable, process);
		}
	}

	private boolean firstTermComesLaterInChoiceOrder(Expression term1, Expression term2, RewritingProcess process) {
		boolean result = isVariableTerm(term1, process) && variableIsChosenAfterOtherTerm(term1, term2, getSupportedIndices(), process);
		return result;
	}

	private boolean isVariableTerm(Expression term, RewritingProcess process) {
		return getTheory().isVariableTerm(term, process);
	}

	public void addFirstTermAsDisequalOfSecondTermDestructively(Expression term1, Expression term2, RewritingProcess process) {
		NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForTerm1 = nonEqualitiesConstraintFor(term1, process);
		BinaryPredicate<Expression, Expression> disequalityDirectlyImpliedExternally = (t1, t2) -> directlyImpliesDisequality(t1, t2, process);
		disequalitiesConstraintForTerm1.incorporateDestructively(false, Equality.make(term1, term2), disequalityDirectlyImpliedExternally, process);
	}

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
			DisequalitiesConstraintForSingleVariable disequalitiesConstraintForSingleVariable = (DisequalitiesConstraintForSingleVariable) entry.getValue();
			
			Expression variable = disequalitiesConstraintForSingleVariable.getVariable();
			Collection<Expression> disequals = disequalitiesConstraintForSingleVariable.getDisequals();
			
			Expression variableRepresentative = variable.replaceAllOccurrences(getRepresentative, process); // NOTE: variableRepresentative needs not be a variable itself.
			Collection<Expression> disequalsRepresentatives = mapIntoSetOrSameIfNoDistinctElementInstances(disequals, t -> t.replaceAllOccurrences(getRepresentative, process));
		
			if (variableRepresentative != variable || disequalsRepresentatives != disequals) { // otherwise, nothing to do
				map.remove(variable); // note that an entry for this same key *may* be re-created if some other term being updated gets updated to it
				for (Expression disequalRepresentative : disequalsRepresentatives) {
					incorporateDisequalityDestructively(variableRepresentative, disequalRepresentative, process);
				}
			}
		}
	}

	/**
	 * Indicates whether the constraints represented ever product splitters that need to be satisfied (default is true).
	 * @return
	 */
	protected boolean constraintsDoNotProduceSplittersToBeSatisfied() {
		return true;
	}
	
	/**
	 * Indicates whether the constraints represented ever product splitters that need to be <i>not</i> satisfied (default is false).
	 * @return
	 */
	protected boolean constraintsDoNotProduceSplittersToBeNotSatisfied() {
		return false;
	}

	protected Collection<Expression> getSplittersToBeSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Collection<Expression> result = new LinkedList<Expression>();
		if ( ! constraintsDoNotProduceSplittersToBeSatisfied()) {
			for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : map.entrySet()) {
				Util.myAssert(() -> getTermTheory().isVariableTerm(entry.getKey(), process), () -> "Key in map for NonEqualitiesConstraints is not a variable, but " + entry.getKey());
				Expression variable = entry.getKey();
				if ( ! indicesSubSet.contains(variable)) { // if variable is free
					NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForSingleVariable = entry.getValue();
					List<Expression> subResult = disequalitiesConstraintForSingleVariable.getSplittersToBeSatisfied();
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
					NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForSingleVariable = entry.getValue();
					List<Expression> subResult = disequalitiesConstraintForSingleVariable.getSplittersToBeNotSatisfied();
					result.addAll(subResult);
				}
			}
		}
		else {
			result = emptyList();
		}
		return result;
	}

	protected Expression computeNumberOfPossibleValuesFor(Expression index, RewritingProcess process) {
		Expression result = nonEqualitiesConstraintFor(index, process).modelCount(list(index), process);
		return result;
	}
	
	protected NonEqualitiesConstraintForSingleVariable makeNonEqualitiesConstraintForVariable(Expression variable) {
		NonEqualitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, getTheory(), getSupportedIndices());
		return result;
	}

	@Override
	public EqualityConstraintTheory getTheory() {
		return theory;
	}

	@Override
	public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
		return splitter;
		// TODO: needs to be generalized for literals other than disequalities
	}

	@Override
	public Expression normalizeExpressionWithoutLiterals(Expression expression, RewritingProcess process) {
		return expression;
	}

	@Override
	protected void applyNormalizedSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		throw new Error((new Object(){}).getClass().getEnclosingMethod() + " not implemented yet.");
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> conjuncts = new LinkedList<Expression>();
		conjuncts.addAll(map.values());
		Expression result = And.make(conjuncts);
		return result;
	}
}