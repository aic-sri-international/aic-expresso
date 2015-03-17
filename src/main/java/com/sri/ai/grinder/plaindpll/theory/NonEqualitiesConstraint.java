package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.util.Util.addAllForEachEntry;
import static com.sri.ai.util.Util.getTransformedSubMap;
import static com.sri.ai.util.Util.list;
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
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.api.Theory;
import com.sri.ai.grinder.plaindpll.core.AbstractRuleOfProductConstraint;
import com.sri.ai.grinder.plaindpll.theory.EqualityTheory.EqualityConstraint;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;


/** 
 */	
@SuppressWarnings("serial")
public class NonEqualitiesConstraint extends AbstractRuleOfProductConstraint implements Map<Expression, NonEqualitiesConstraintForSingleVariable>, Constraint {

	private LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable> map = new LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable>();

	public NonEqualitiesConstraint(Collection<Expression> supportedIndices, EqualityTheory.EqualityConstraint parentConstraint) {
		super(supportedIndices);
		this.parentConstraint = parentConstraint;
	}
	
	public NonEqualitiesConstraint copyWithNewParent(Constraint newParent) {
		NonEqualitiesConstraint result = new NonEqualitiesConstraint(supportedIndices, (EqualityConstraint) newParent);
		result.parentConstraint = newParent;
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : entrySet()) {
			NonEqualitiesConstraintForSingleVariable newEntryValue = entry.getValue().copyWithNewParent(newParent);
			result.put(entry.getKey(), newEntryValue);
		}
		return result;
		// TODO: change parents of NonEqualitiesConstraintForSingleVariable to the NonEqualitiesConstraint instead of the EqualityConstraint
		// TODO: implement a copy-on-write scheme
	}
	
	@Override
	protected Expression provideSplitterRequiredForComputingNumberOfValuesFor(Expression index, RewritingProcess process) {
		Expression result = nonEqualitiesConstraintFor(index, process).pickSplitter(list(index), process);
		return result;
	}

	private NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintFor(Expression variable, RewritingProcess process) {
		assert ((EqualityConstraint) parentConstraint).getTermTheory().isVariableTerm(variable, process) : "nonEqualitiesConstraintFor must be invoked for a variable but was invoked on " + variable;
		NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintForTerm =
				Util.getValuePossiblyCreatingIt(this, variable, key -> makeNonEqualitiesConstraintForVariable(key));
		return nonEqualitiesConstraintForTerm;
	}

	protected boolean representativesAreExplicitlyConstrainedToBeDisequal(Expression representative1, Expression representative2, RewritingProcess process) {
		boolean result = false;
		boolean representative1IsUniquelyNamedConstant = process.isUniquelyNamedConstant(representative1);
		boolean representative2IsUniquelyNamedConstant = process.isUniquelyNamedConstant(representative2);
		
		Expression splitter = apply(EQUALITY, representative1, representative2);
		
		if (representative1IsUniquelyNamedConstant && representative2IsUniquelyNamedConstant) {
			result = ! representative1.equals(representative2);
		}
		else if ( ! representative1IsUniquelyNamedConstant &&
				nonEqualitiesConstraintFor(representative1, process).normalizeSplitterGivenConstraint(splitter, process)
				!= splitter) {
			result = true;
		}
		else if ( ! representative2IsUniquelyNamedConstant &&
				nonEqualitiesConstraintFor(representative2, process).normalizeSplitterGivenConstraint(splitter, process)
				!= splitter) {
			result = true;
		}
		
		// this method looks weird right now because we are in the process of generalizing it from DisequalitiesConstraintForSingleVariable to NonEqualitiesConstraint.
		// Eventually this whole method will be a normalizeSplitterGivenConstraint for a Constraint implementation that gathers NonEqualitiesConstraints for all variables.
		
		return result;
	}

	public void addFirstTermAsDisequalOfSecondTermDestructively(Expression term1, Expression term2, RewritingProcess process) {
		NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForTerm1 = nonEqualitiesConstraintFor(term1, process);
		disequalitiesConstraintForTerm1.incorporatePossiblyDestructively(false, Equality.make(term1, term2), process);
	}

	public void updateRepresentativesInDisequalitiesMap(RewritingProcess process) {
		
		// Go over all entries of disequality map, and if entry requires updating,
		// add it to a map from each term to NonEqualitiesForSingleTerm,
		// keeping also track of which variables got updated.
		// If multiple variables are updated to same term, take the union of their NonEqualitiesForSingleTerm.
		Function<Entry<Expression, NonEqualitiesConstraintForSingleVariable>, Pair<Expression, NonEqualitiesForSingleTerm>>
		getUpdatedTermAndNonEqualities = entry -> entry.getValue().updatedTermAndNonEqualitiesPair(process);
		
		BinaryFunction<NonEqualitiesForSingleTerm, NonEqualitiesForSingleTerm, NonEqualitiesForSingleTerm>
		unionOfNonEqualitiesIfNeeded = (previous, more) -> addAllForEachEntry(previous, more);
		
		Pair<Map<Expression, NonEqualitiesForSingleTerm>, Set<Expression>>
		updatedNonEqualitiesByTermAndDeletedVariables
		= getTransformedSubMap(this, getUpdatedTermAndNonEqualities, unionOfNonEqualitiesIfNeeded);

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
		removeAll(this, deletedVariables);

		// and we add the new disequalities. Note we cannot just put them in the map as they are, because of choosing order
		for (Map.Entry<Expression, NonEqualitiesForSingleTerm> updatedEntry : updatedNonEqualitiesByTerm.entrySet()) {
			for (Expression disequal : updatedEntry.getValue().get(DISEQUALITY)) {
				applyRepresentativesDisequalityDestructively(updatedEntry.getKey(), disequal, process);
			}
		}
	}

	/** Assumes disequality does not turn constraint into contradiction */
	private void applyRepresentativesDisequalityDestructively(Expression term1, Expression term2, RewritingProcess process) {
		if (getTermTheory().isVariableTerm(term1, process) || getTermTheory().isVariableTerm(term2, process)) {
			if (getTermTheory().isVariableTerm(term1, process) && EqualityTheory.variableIsChosenAfterOtherTerm(term1, term2, supportedIndices, process)) {
				addFirstTermAsDisequalOfSecondTermDestructively(term1, term2, process);
			}
			else { // term2 must be a variable because either term1 is not a variable, or it is but term2 comes later than term1 in ordering, which means it is a variable
				addFirstTermAsDisequalOfSecondTermDestructively(term2, term1, process);
			}
		}
		// else they are both constants, and distinct ones, so no need to do anything.
	}

	public void getNonEqualitiesSplittersToBeSatisfied(Collection<Expression> indicesSubSet, Collection<Expression> result, RewritingProcess process) {
		// TODO: when nonEqualitiesConstraint gets consolidated into a single Constraint object, make sure it has a method getSplittersToBeSatisfied
		// that does not iterate over all variables for disequalities, since we know in advance they do not provide splitters of this sort.
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : entrySet()) {
			assert getTermTheory().isVariableTerm(entry.getKey(), process);
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
	private TermTheory getTermTheory() {
		return ((EqualityTheory.EqualityConstraint) parentConstraint).getTermTheory();
	}

	public Collection<Expression> getNonEqualitiesSplittersToBeNotSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Collection<Expression> result = new LinkedHashSet<Expression>();
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : entrySet()) {
			assert getTermTheory().isVariableTerm(entry.getKey(), process);
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
				Util.getValuePossiblyCreatingIt(this, term, key -> makeNonEqualitiesConstraintForVariable(key));
		return nonEqualitiesConstraintForTerm;
	}

	protected NonEqualitiesConstraintForSingleVariable makeNonEqualitiesConstraintForVariable(Expression variable) {
		NonEqualitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, (EqualityConstraint) parentConstraint);
		// TODO: need to change parent constraint of DisequalitiesConstraintForSingleVariable to this one, not the equality constraint.
		return result;
	}

	@Override
	public Theory getTheory() {
		return parentConstraint.getTheory();
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

	@Override
	public NonEqualitiesConstraint clone() {
		NonEqualitiesConstraint newOne = new NonEqualitiesConstraint(supportedIndices, (EqualityConstraint) parentConstraint);
		newOne.map = new LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable>();
		newOne.map.putAll(map);
		return newOne;
	}

	// Map
	
	@Override
	public void clear() {
		map.clear();
	}

	@Override
	public boolean containsKey(Object arg0) {
		return map.containsKey(arg0);
	}

	@Override
	public boolean containsValue(Object arg0) {
		return map.containsValue(arg0);
	}

	@Override
	public Set<java.util.Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable>> entrySet() {
		return map.entrySet();
	}

	@Override
	public NonEqualitiesConstraintForSingleVariable get(Object arg0) {
		return map.get(arg0);
	}

	@Override
	public boolean isEmpty() {
		return map.isEmpty();
	}

	@Override
	public Set<Expression> keySet() {
		return map.keySet();
	}

	@Override
	public NonEqualitiesConstraintForSingleVariable put(Expression arg0, NonEqualitiesConstraintForSingleVariable arg1) {
		return map.put(arg0, arg1);
	}

	@Override
	public void putAll(Map<? extends Expression, ? extends NonEqualitiesConstraintForSingleVariable> arg0) {
		map.putAll(arg0);
	}

	@Override
	public NonEqualitiesConstraintForSingleVariable remove(Object arg0) {
		return map.remove(arg0);
	}

	@Override
	public int size() {
		return map.size();
	}

	@Override
	public Collection<NonEqualitiesConstraintForSingleVariable> values() {
		return map.values();
	}
}