package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.util.Util.addAllForEachEntry;
import static com.sri.ai.util.Util.getTransformedSubMap;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.removeAll;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.core.AbstractRuleOfProductConstraint;
import com.sri.ai.grinder.plaindpll.core.AbstractTheory;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;


/** 
 */	
@SuppressWarnings("serial")
public class DefaultNonEqualitiesConstraint extends AbstractRuleOfProductConstraint implements NonEqualitiesConstraint  {

	private EqualityTheory theory;
	
	private LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable> map = new LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable>();

	public DefaultNonEqualitiesConstraint(EqualityTheory theory, Collection<Expression> supportedIndices) {
		super(supportedIndices);
		this.theory = theory;
	}
	
	@Override
	public DefaultNonEqualitiesConstraint clone() {
		DefaultNonEqualitiesConstraint result = new DefaultNonEqualitiesConstraint(theory, supportedIndices);
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : map.entrySet()) {
			NonEqualitiesConstraintForSingleVariable newEntryValue = (NonEqualitiesConstraintForSingleVariable) entry.getValue().clone();
			newEntryValue.setNonEqualitiesConstraintDestructively(result);
			result.map.put(entry.getKey(), newEntryValue);
		}
		return result;
		// TODO: implement a copy-on-write scheme
	}

	@Override
	protected Expression provideSplitterRequiredForComputingNumberOfValuesFor(Expression index, RewritingProcess process) {
		Expression result = nonEqualitiesConstraintFor(index, process).pickSplitter(list(index), process);
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

		boolean result;
		Expression term1 = literal.get(0);
		Expression term2 = literal.get(1);
		
		// at least one of the representatives is a variable, or the literal would be trivial.
		if (firstTermComesLaterInChoiceOrder(term1, term2, process)) {
			result = nonEqualitiesConstraintFor(term1, process).directlyImpliesNonTrivialLiteral(literal, process);
		}
		else {
			Expression literalWithRepresentative2First = apply(DISEQUALITY, term2, term1);
			result = nonEqualitiesConstraintFor(term2, process).directlyImpliesNonTrivialLiteral(literalWithRepresentative2First, process);
			// TODO: perhaps we should extend Constraint for an interface that accepts directlyImpliesDisequalityAgainst(representative1, process)
			// or something similar, in order to avoid the unnecessary construction of the literal, as well as the check about which of the literal's terms
			// is equal to the single variable of the constraint, which we already know at this point.
		}
		
		return result;
	}

	@Override
	public DefaultNonEqualitiesConstraint incorporatePossiblyDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		myAssert( ! splitterSign, "DefaultNonEqualitiesConstraint.incorporatePossiblyDestructively must receive disequalities only for now");
		Expression variable  = splitter.get(0);
		Expression otherTerm = splitter.get(1);
		if (isVariableTerm(variable, process) || isVariableTerm(otherTerm, process)) {
			if (firstTermComesLaterInChoiceOrder(variable, otherTerm, process)) {
				addFirstTermAsDisequalOfSecondTermDestructively(variable, otherTerm, process);
			}
			else { // term2 must be a variable because either term1 is not a variable, or it is but term2 comes later than term1 in ordering, which means it is a variable
				addFirstTermAsDisequalOfSecondTermDestructively(otherTerm, variable, process);
			}
		}
		// else they are both constants, and distinct ones, so no need to do anything.
		return this;
	}

	private boolean firstTermComesLaterInChoiceOrder(Expression representative1, Expression representative2, RewritingProcess process) {
		return isVariableTerm(representative1, process) &&
				AbstractTheory.variableIsChosenAfterOtherTerm(representative1, representative2, getSupportedIndices(), process);
	}

	private boolean isVariableTerm(Expression term, RewritingProcess process) {
		return getTheory().isVariableTerm(term, process);
	}

	public void addFirstTermAsDisequalOfSecondTermDestructively(Expression term1, Expression term2, RewritingProcess process) {
		NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForTerm1 = nonEqualitiesConstraintFor(term1, process);
		disequalitiesConstraintForTerm1.incorporatePossiblyDestructively(false, Equality.make(term1, term2), process);
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
	public DefaultNonEqualitiesConstraint updateRepresentativesPossiblyDestructively(Function<Expression, Expression> getRepresentative, RewritingProcess process) {
		
		// Go over all entries of disequality map, and if entry requires updating,
		// add it to a map from each term to NonEqualitiesForSingleTerm,
		// keeping also track of which variables got updated.
		// If multiple variables are updated to same term, take the union of their NonEqualitiesForSingleTerm.
		Function<Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable>, Pair<Expression, NonEqualitiesForSingleTerm>>
		getUpdatedTermAndNonEqualities = entry -> entry.getValue().updatedTermAndNonEqualitiesPair(getRepresentative, process);
		
		BinaryFunction<NonEqualitiesForSingleTerm, NonEqualitiesForSingleTerm, NonEqualitiesForSingleTerm>
		unionOfNonEqualitiesIfNeeded = (previous, more) -> addAllForEachEntry(previous, more);
		
		Pair<Map<Expression, NonEqualitiesForSingleTerm>, Set<Expression>>
		updatedNonEqualitiesByTermAndDeletedVariables
		= getTransformedSubMap(map, getUpdatedTermAndNonEqualities, unionOfNonEqualitiesIfNeeded);
		// TODO: it could be more efficient to apply disequalities right inside the above call, instead of storing pairs and then applying.
		// It would also detect contradictions sooner.

		// Notes:
		// One might wonder why we did not represent the non-equal elements after the update
		// in a DisequalitiesConstraintForSingleVariable object, and then just add this new
		// object to the disequalities map.
		// There are two reasons for this:
		// - DisequalitiesConstraintForSingleVariable requires the elements non-equal to its variable
		// to contain only terms that come *before* the variable in the term ordering;
		// updating representatives may result in some non-equal terms to come *after* the
		// (also now possibly updated) variable;
		// therefore, these non-equal elements will have to be re-constrained to be non-equal from
		// the updated variable one by one anyway, and keeping them in a DisequalitiesConstraintForSingleVariable
		// would not be useful as they will probably go separate anyway;
		// - the variable may be updated to a constant, and DisequalitiesConstraintForSingleVariable
		// is only defined for variables.
		
		// now we update the disequalities map if needed:

		Map<Expression, NonEqualitiesForSingleTerm> updatedNonEqualitiesByTerm = updatedNonEqualitiesByTermAndDeletedVariables.first;
		Set<Expression> deletedVariables = updatedNonEqualitiesByTermAndDeletedVariables.second;

		// we start by removing the modified entries
		removeAll(map, deletedVariables);

		// and we add the new disequalities. Note we cannot just put them in the map as they are, because of choosing order
		for (Map.Entry<Expression, NonEqualitiesForSingleTerm> updatedEntry : updatedNonEqualitiesByTerm.entrySet()) {
			for (Expression disequal : updatedEntry.getValue().get(DISEQUALITY)) {
				applyRepresentativesDisequalityDestructively(updatedEntry.getKey(), disequal, process);
			}
		}
		
		return this;
	}

	/** Assumes disequality does not turn constraint into contradiction */
	private void applyRepresentativesDisequalityDestructively(Expression term1, Expression term2, RewritingProcess process) {
		if (getTermTheory().isVariableTerm(term1, process) || getTermTheory().isVariableTerm(term2, process)) {
			if (firstTermComesLaterInChoiceOrder(term1, term2, process)) {
				addFirstTermAsDisequalOfSecondTermDestructively(term1, term2, process);
			}
			else { // term2 must be a variable because either term1 is not a variable, or it is but term2 comes later than term1 in ordering, which means it is a variable
				addFirstTermAsDisequalOfSecondTermDestructively(term2, term1, process);
			}
		}
		// else they are both constants, and distinct ones, so no need to do anything.
	}

	public void getNonEqualitiesSplittersToBeSatisfied(Collection<Expression> indicesSubSet, Collection<Expression> result, RewritingProcess process) {
		// TODO: when nonEqualities gets consolidated into a single Constraint object, make sure it has a method getSplittersToBeSatisfied
		// that does not iterate over all variables for disequalities, since we know in advance they do not provide splitters of this sort.
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

	/**
	 * @return
	 */
	public TermTheory getTermTheory() {
		return getTheory().getTermTheory();
	}

	public Collection<Expression> getNonEqualitiesSplittersToBeNotSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Collection<Expression> result = new LinkedHashSet<Expression>();
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : map.entrySet()) {
			Util.myAssert(() -> getTermTheory().isVariableTerm(entry.getKey(), process), () -> "Key in map for NonEqualitiesConstraints is not a variable, but " + entry.getKey());
			Expression variable = entry.getKey();
			if ( ! indicesSubSet.contains(variable)) { // if variable is free
				NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForSingleVariable = entry.getValue();
				List<Expression> subResult = disequalitiesConstraintForSingleVariable.getSplittersToBeNotSatisfied();
				result.addAll(subResult);
			}
		}
		return result;
	}

	protected Collection<Expression> getSplittersToBeSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Collection<Expression> result = new LinkedList<Expression>();
		getNonEqualitiesSplittersToBeSatisfied(indicesSubSet, result, process);
		return result;
	}

	protected Collection<Expression> getSplittersToBeNotSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
		return getNonEqualitiesSplittersToBeNotSatisfied(indicesSubSet, process);
	}

	protected Expression computeNumberOfPossibleValuesFor(Expression index, RewritingProcess process) {
		Expression result = nonEqualitiesConstraintFor(index).modelCount(list(index), process);
		return result;
	}
	
	private NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintFor(Expression term) {
		NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintForTerm =
				Util.getValuePossiblyCreatingIt(map, term, key -> makeNonEqualitiesConstraintForVariable(key));
		return nonEqualitiesConstraintForTerm;
	}

	protected NonEqualitiesConstraintForSingleVariable makeNonEqualitiesConstraintForVariable(Expression variable) {
		NonEqualitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, this);
		// TODO: need to change parent constraint of DisequalitiesConstraintForSingleVariable to this one, not the equality constraint.
		return result;
	}

	@Override
	public EqualityTheory getTheory() {
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
		// TODO Auto-generated method stub
		
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> conjuncts = new LinkedList<Expression>();
		conjuncts.addAll(map.values());
		Expression result = And.make(conjuncts);
		return result;
	}
}