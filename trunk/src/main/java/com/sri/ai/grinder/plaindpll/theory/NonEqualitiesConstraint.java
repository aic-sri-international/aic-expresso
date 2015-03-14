package com.sri.ai.grinder.plaindpll.theory;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.Theory;
import com.sri.ai.grinder.plaindpll.theory.EqualityTheory;
import com.sri.ai.grinder.plaindpll.theory.EqualityTheory.EqualityConstraint;


/** 
 */	
@SuppressWarnings("serial")
public class NonEqualitiesConstraint extends AbstractConstraint implements Map<Expression, NonEqualitiesConstraintForSingleVariable>, Constraint {

	private LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable> map = new LinkedHashMap<Expression, NonEqualitiesConstraintForSingleVariable>();

	public NonEqualitiesConstraint(EqualityTheory.EqualityConstraint parentConstraint) {
		this.parentConstraint = parentConstraint;
	}
	
	public NonEqualitiesConstraint copyWithNewParent(Constraint newParent) {
		NonEqualitiesConstraint result = new NonEqualitiesConstraint((EqualityConstraint) newParent);
		result.parentConstraint = newParent;
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : entrySet()) {
			NonEqualitiesConstraintForSingleVariable newEntryValue = entry.getValue().copyWithNewParent(newParent);
			result.put(entry.getKey(), newEntryValue);
		}
		return result;
		// TODO: change parents of NonEqualitiesConstraintForSingleVariable to the NonEqualitiesConstraint instead of the EqualityConstraint
		// TODO: implement a copy-on-write scheme
	}
	
	public void getNonEqualitiesSplittersToBeSatisfied(Collection<Expression> indicesSubSet, Collection<Expression> result, RewritingProcess process) {
		// TODO: when nonEqualitiesConstraint gets consolidated into a single Constraint object, make sure it has a method getSplittersToBeSatisfied
		// that does not iterate over all variables for disequalities, since we know in advance they do not provide splitters of this sort.
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : entrySet()) {
			assert ((EqualityTheory.EqualityConstraint) parentConstraint).getTermTheory().isVariableTerm(entry.getKey(), process);
			Expression variable = entry.getKey();
			if ( ! indicesSubSet.contains(variable)) { // if variable is free
				NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForSingleVariable = entry.getValue();
				List<Expression> subResult = disequalitiesConstraintForSingleVariable.getSplittersToBeSatisfied();
				result.addAll(subResult);
			}
		}
	}

	public Collection<Expression> getNonEqualitiesSplittersToBeNotSatisfied(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Collection<Expression> result = new LinkedHashSet<Expression>();
		for (Map.Entry<Expression, NonEqualitiesConstraintForSingleVariable> entry : entrySet()) {
			assert ((EqualityTheory.EqualityConstraint) parentConstraint).getTermTheory().isVariableTerm(entry.getKey(), process);
			Expression variable = entry.getKey();
			if ( ! indicesSubSet.contains(variable)) { // if variable is free
				NonEqualitiesConstraintForSingleVariable disequalitiesConstraintForSingleVariable = entry.getValue();
				List<Expression> subResult = disequalitiesConstraintForSingleVariable.getSplittersToBeNotSatisfied();
				result.addAll(subResult);
			}
		}
		return result;
	}

	@Override
	public Theory getTheory() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<Expression> getSupportedIndices() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Constraint incorporate(boolean splitterSign, Expression splitter, RewritingProcess process) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Expression normalize(Expression expression, RewritingProcess process) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> conjuncts = new LinkedList<Expression>();
		conjuncts.addAll(map.values());
		Expression result = And.make(conjuncts);
		return result;
	}

	@Override
	public Expression clone() {
		NonEqualitiesConstraint newOne = new NonEqualitiesConstraint((EqualityConstraint) parentConstraint);
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