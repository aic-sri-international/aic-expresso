package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.Equality.isEquality;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.mapIntoSetOrSameIfNoDistinctElementInstances;
import static com.sri.ai.util.Util.myAssert;
import static java.lang.Math.max;
import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.grinder.plaindpll.theory.EqualityTheory.EqualityConstraint;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * An implementation of {@link NonEqualitiesForSingleTerm} in which the only constraint is disequality;
 * its parent constraint must be a {@link EqualityConstraint}.
 * @author braz
 *
 */
@SuppressWarnings("serial")
public class DisequalitiesConstraintForSingleVariable extends AbstractNonEqualitiesConstraintForSingleVariable {
	private boolean ownMyDisequals; // allows "borrowing" of another object's fields until and if we need to write to them. Eventually it would more elegant to use a Collection implementation that keeps track of it itself.
	private Collection<Expression> disequals;
	private Collection<Expression> uniquelyValuedDisequals; // disequals constrained to be disequal from all uniquely-valued disequals added before themselves. If this set reaches variable's domain size, there will be no value left for it and an inconsistency is indicated.

	public DisequalitiesConstraintForSingleVariable(Expression variable, EqualityConstraint parentConstraint) {
		super(variable, parentConstraint);
		this.ownMyDisequals = true;
		this.disequals = Util.set();
		this.uniquelyValuedDisequals = Util.set();
	}

	/**
	 * Parent constraint must be an {@link EqualityConstraint}.
	 */
	@Override
	public DisequalitiesConstraintForSingleVariable copyWithNewParent(Constraint parentConstraint) {
		Util.myAssert(() -> parentConstraint instanceof EqualityConstraint, () -> getClass() + "'s parent constraint must be an EqualityConstraint");
		return (DisequalitiesConstraintForSingleVariable) super.copyWithNewParent(parentConstraint);
	}

	@Override
	public DisequalitiesConstraintForSingleVariable clone() {
		DisequalitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, (EqualityConstraint) parentConstraint);
		result.cachedIndexDomainSize = cachedIndexDomainSize;
		result.ownMyDisequals = false;
		result.disequals = disequals;
		result.uniquelyValuedDisequals = uniquelyValuedDisequals;
		return result;
	}
	
	public void addNonEqualityConstraintDestructively(String functor, Expression term, RewritingProcess process) throws Contradiction {
		if ( ! ownMyDisequals) {
			disequals = new LinkedHashSet<Expression>(disequals);
			if (getIndexDomainSize(process) != -1) {
				uniquelyValuedDisequals = new LinkedHashSet<Expression>(uniquelyValuedDisequals);
			}
			ownMyDisequals = true;
		}
		disequals.add(term);
		updateUniqueValuedDisequals(term, process);
	}

	@Override
	public DisequalitiesConstraintForSingleVariable incorporatePossiblyDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Util.myAssert(() -> ! splitterSign && isEquality(splitter), () -> getClass() + " only allowed to take negative equality literals (disequalities) but got " + (splitterSign? "" : "not ") + " " + splitter);
		Util.myAssert(() -> splitter.get(0).equals(variable), () -> getClass() + " must only take splitters in which the first argument is the same as the main variable");
		addNonEqualityConstraintDestructively(DISEQUALITY, splitter.get(1), process);
		return this;
	}
	
	private void updateUniqueValuedDisequals(Expression term, RewritingProcess process) throws Contradiction {
		if (getIndexDomainSize(process) != -1) {
			if (forAll(uniquelyValuedDisequals, u -> areConstrainedToBeDisequal(u, term, process))) {
				uniquelyValuedDisequals.add(term);
			}
			if (uniquelyValuedDisequals.size() >= getIndexDomainSize(process)) {
				throw new Contradiction();
			}
		}
	}
	
	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
		if ( ! indicesSubSet.isEmpty()) { // if empty, no splitters are needed. If not empty, it must be a set with this.index as only element.
			for (Expression disequal : disequals) {
				if (getTheory().isVariableTerm(disequal, process)) { // we can restrict y to variables because at least one of y or t must be a variable (otherwise they would be two constants and we already know those are disequal).
					Expression splitter  = getSplitterTowardsEnsuringVariableIsDisequalFromAllOtherTermsInCollection(disequal, process);
					if (splitter != null) {
						return splitter;
					}
				}
			}
		}
		
		return null;
	}

	private Expression getSplitterTowardsEnsuringVariableIsDisequalFromAllOtherTermsInCollection(Expression disequal, RewritingProcess process) {
		for (Expression anotherDisequal : disequals) {
			if ( ! anotherDisequal.equals(disequal)) {
				Expression splitter = getTermTheory().getSplitterTowardDisunifyingDistinctTerms(disequal, anotherDisequal, process); // if function applications, we need to disunify arguments first, for instance.
				if (splitter != null) {
					return splitter; // need to disunify first
				}
				else if ( ! areConstrainedToBeDisequal(disequal, anotherDisequal, process)) { // already disunified
					splitter = getTheory().makeSplitterFromFunctorAndTwoDistinctTermsOneOfWhichIsAVariable(EQUALITY, disequal, anotherDisequal, getSupportedIndices(), process);
					return splitter;
				}
			}
		}
		return null;
	}

	private boolean areConstrainedToBeDisequal(Expression disequal, Expression anotherDisequal, RewritingProcess process) {
		boolean result = ((EqualityConstraint)parentConstraint).termsAreExplicitlyConstrainedToBeDisequal(disequal, anotherDisequal, process);
		return result;
	}

	@Override
	public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Expression numberOfPossibleValuesForIndex;
		long numberOfNonAvailableValues = disequals.size();
		long typeSize = getIndexDomainSize(process);
		if (typeSize == -1) {
			Expression indexType = process.getContextualSymbolType(variable);
			if (indexType == null) {
				indexType = new DefaultSyntacticFunctionApplication(TYPE, variable);
			}
			Expression indexTypeCardinality = apply(CARDINALITY, indexType);
			numberOfPossibleValuesForIndex = Minus.make(indexTypeCardinality, makeSymbol(numberOfNonAvailableValues));
		}
		else {
			numberOfPossibleValuesForIndex = makeSymbol(max(0, typeSize - numberOfNonAvailableValues));
		}
		return numberOfPossibleValuesForIndex;
	}

	@Override
	public boolean directlyImplies(Expression literal, RewritingProcess process) {
		myAssert(() -> literal.hasFunctor(DISEQUALITY), "DisequalitiesConstraintForSingleVariable.directlyImplies must receive disequality constraints only.");
		boolean result;
		
		if (literal.get(0).equals(variable) && disequals.contains(literal.get(1))) {
			result = true;
		}
		else if (literal.get(1).equals(variable) && disequals.contains(literal.get(0))) {
			result = true;
		}
		else {
			result = false;
		}
		
		return result;
	}

	@Override
	public Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process) {
		Expression result;
		
		if (splitter.get(0).equals(variable) && disequals.contains(splitter.get(1))) {
			result = FALSE;
		}
		else if (splitter.get(1).equals(variable) && disequals.contains(splitter.get(0))) {
			result = FALSE;
		}
		else {
			result = splitter;
		}
		
		return result;
	}

	/**
	 * Returns a pair of the variable and non-equality constraints map after updating representatives in their
	 * representation, or <code>null</code> if there are no changes.
	 * @param process
	 * @return
	 * @throws Contradiction
	 */
	public Pair<Expression, NonEqualitiesForSingleTerm> updatedTermAndNonEqualitiesPair(Function<Expression, Expression> getRepresentative, RewritingProcess process) throws Contradiction {
		Pair<Expression, NonEqualitiesForSingleTerm> result = new Pair<Expression, NonEqualitiesForSingleTerm>();
		result.first = variable.replaceAllOccurrences(getRepresentative, process);
		
		Function<Expression, Expression> replaceAllTermsByRepresentatives = e -> e.replaceAllOccurrences(getRepresentative, process);
		Collection<Expression> newDisequals = mapIntoSetOrSameIfNoDistinctElementInstances(disequals, replaceAllTermsByRepresentatives);
		if (result.first == variable && newDisequals == disequals) {
			result = null;
		}
		else {
			if (newDisequals.contains(result.first)) {
				throw new Contradiction();
			}
			result.second = new LinkedHashNonEqualitiesForSingleVariable();
			result.second.put(DISEQUALITY, newDisequals);
		}
			
		return result;
	}

	public List<Expression> getSplittersToBeSatisfied() {
		return emptyList();
	}

	public List<Expression> getSplittersToBeNotSatisfied() {
		List<Expression> result = new LinkedList<Expression>();
		for (Expression disequal : disequals) {
			Expression splitter = apply(EQUALITY, variable, disequal); // making it with apply instead of Equality.make sidesteps simplifications, which will not occur in this case because otherwise this condition would have either rendered the constraint a contradiction, or would have been eliminated from it
			result.add(splitter);
		}
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> arguments = mapIntoList(disequals, d -> apply(DISEQUALITY, variable, d));
		Expression result = And.make(arguments);
		return result;
	}
}