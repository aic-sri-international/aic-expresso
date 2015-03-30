package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.Equality.isEquality;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;
import static com.sri.ai.util.Util.getIndexOfFirstSatisfyingPredicateOrMinusOne;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static java.lang.Math.max;
import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.util.collect.ArrayHashSet;
import com.sri.ai.util.collect.ArraySet;
import com.sri.ai.util.collect.CopyOnWriteArraySet;

/**
 * An implementation of {@link NonEqualitiesForSingleTerm} in which the only constraint is disequality;
 * it must be informed of its containing {@link NonEqualitiesConstraint}.
 * @author braz
 *
 */
@SuppressWarnings("serial")
public class DisequalitiesConstraintForSingleVariable extends AbstractNonEqualitiesConstraintForSingleVariable {
	private ArraySet<Expression> disequals;
	private ArraySet<Expression> uniquelyValuedDisequals; // disequals constrained to be disequal from all uniquely-valued disequals added before themselves. If this set reaches variable's domain size, there will be no value left for it and an inconsistency is indicated.
	private Expression lastUniquelyValuedDisequal;
	private DisequalitiesSplitterSearchLowerBound splitterSearchLowerBound;
	private boolean allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound; // see occurrences of field for comments on the meaning of this field
	
	/**
	 * Constructs a set of disequality constraints for variable to be used inside nonEqualitiesConstraint.
	 * @param variable
	 * @param nonEqualitiesConstraint
	 */
	public DisequalitiesConstraintForSingleVariable(Expression variable, NonEqualitiesConstraint nonEqualitiesConstraint) {
		super(variable, nonEqualitiesConstraint);
		this.disequals = new ArrayHashSet<Expression>(); // TODO have constructor take expected capacity
		this.uniquelyValuedDisequals = new ArrayHashSet<Expression>();
		this.lastUniquelyValuedDisequal = null;
		this.nonEqualitiesConstraint = nonEqualitiesConstraint;
		this.splitterSearchLowerBound = null;
		this.allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound = true;
	}

	@Override
	public DisequalitiesConstraintForSingleVariable clone() {
		DisequalitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, nonEqualitiesConstraint);
		result.cachedIndexDomainSize = cachedIndexDomainSize;
		result.disequals = new CopyOnWriteArraySet<Expression>(disequals, ArrayHashSet.class);
		result.uniquelyValuedDisequals = new CopyOnWriteArraySet<Expression>(uniquelyValuedDisequals, ArrayHashSet.class);
		result.lastUniquelyValuedDisequal = lastUniquelyValuedDisequal;
		result.splitterSearchLowerBound = splitterSearchLowerBound == null? null : new DisequalitiesSplitterSearchLowerBound(splitterSearchLowerBound);
		result.allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound = allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound;
		return result;
	}

	@Override
	public Expression getVariable() {
		return variable;
	}
	
	public Collection<Expression> getDisequals() {
		return Collections.unmodifiableCollection(disequals);
	}
	
	@Override
	public void incorporateDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		myAssert(() -> ! splitterSign && isEquality(splitter), () -> getClass() + " only allowed to take negative equality literals (disequalities) but got " + (splitterSign? "" : "not ") + " " + splitter);
		myAssert(() -> splitter.get(0).equals(variable), () -> getClass() + " must only take splitters in which the first argument is the same as the main variable");
		Expression term = splitter.get(1);
		incorporateDisequalDestructively(term, process);
	}

	public void incorporateDisequalDestructively(Expression term, RewritingProcess process) throws Contradiction {
		boolean termIsNewDisequal = disequals.add(term);
		if (termIsNewDisequal) {
			updateUniqueValuedDisequalsWithNewDisequal(term, process);
		}
	}
	
	private void updateUniqueValuedDisequalsWithNewDisequal(Expression newDisequal, RewritingProcess process) throws Contradiction {
		int index;
		if ((index = getIndexOfFirstTermWithoutDirectlyImpliedDisequalityWithAnotherTerm(uniquelyValuedDisequals, newDisequal, process)) == -1) {
			uniquelyValuedDisequals.add(newDisequal);
			if (uniquelyValuedDisequals.size() == getVariableDomainSize(process)) { // Note that this works when getIndexDomainSize is -1 (no domain size information) because then the condition is always false
				// in principle, comparison is >=, but uniquelyValuedDisequals's size increases one at a time and == is cheaper.
				throw new Contradiction();
			}
			lastUniquelyValuedDisequal = newDisequal;

			// myAssert(allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound, "If all disequals are uniquely valued so far, we should not have allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound set to false yet");
			if (disequals.size() == uniquelyValuedDisequals.size()) {
				// We can speed up future searches for splitters by indicating that
				// you can start from (index1, index2) equal to (disequals.size(), disequals.size() - 1).
				// All pairs coming before that are uniquely valued and will not form a splitter.
				splitterSearchLowerBound = new DisequalitiesSplitterSearchLowerBound(disequals.size(), disequals.size() - 1);
			}
		}
		else {
			if (allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound) { // this update is only allowed the first time (see last line of block)
				// 'index' indicates where first splitter will be, so initialize splitter search data accordingly to save time
				// NOTE: remember 'index' is an index for uniquelyValuedDisequals; however, the fact that we did not have a splitter yet
				// means that all terms added so far were uniquely valued, and therefore uniquelyValuedDisequals is equal to 'disequals' at this point,
				// so 'index' applies to 'disequals' as well.
				int index1 = disequals.size() - 1; // index of current 'newDisequal'
				int index2 = index; // index of the term before newDisequal that is not necessarily disequal to it
				splitterSearchLowerBound = new DisequalitiesSplitterSearchLowerBound(index1, index2);
				allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound = false; // from now on we must not make such updated because we may skip the current splitter candidate.
			}
		}
	}
	
	private int getIndexOfFirstTermWithoutDirectlyImpliedDisequalityWithAnotherTerm(Collection<Expression> terms, Expression anotherTerm, RewritingProcess process) {
		int result = getIndexOfFirstSatisfyingPredicateOrMinusOne(terms, term -> ! nonEqualitiesConstraint.directlyImpliesDisequality(term, anotherTerm, process));
		return result;
	}
	
	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {

		Expression result;
		
		if (indicesSubSet.isEmpty()) { // if empty, no splitters are needed. If not empty, indicesSubSet is necessarily a set with this.variable as only element.
			result = null;
		}
		else {
			if (splitterSearchLowerBound == null) { // never found a splitter during setup, won't find it now
				result = null;
			}
			else {
				result = splitterSearchLowerBound.getCurrentSplitter(disequals, uniquelyValuedDisequals, nonEqualitiesConstraint, (t1, t2) -> getSplitterTowardDisequalityOfTwoTerms(t1, t2, process), process);
			}
		}

		return result;
	}

	private Expression getSplitterTowardDisequalityOfTwoTerms(Expression term1, Expression term2, RewritingProcess process) {
		Expression result = null;
		Expression splitter = getTermTheory().getSplitterTowardDisunifyingDistinctTerms(term1, term2, process); // if function applications, we need to disunify arguments first, for instance.
		if (splitter != null) {
			result = splitter;
		}
		else if (! nonEqualitiesConstraint.directlyImpliesDisequality(term1, term2, process)) { // already disunified but not disequal
			splitter = getTheory().makeSplitterFromFunctorAndVariableAndTermDistinctFromVariable(EQUALITY, term1, term2, getSupportedIndices(), process);
			result = splitter;
		}
		return result;
	}

	@Override
	public boolean directlyImpliesNonTrivialLiteral(Expression literal, RewritingProcess process) {
		boolean result;
		
		if (literal.hasFunctor(EQUALITY)) {
			result = false; // no equalities are implied
		}
		else if (literal.get(0).equals(variable) && disequals.contains(literal.get(1))) {
			result = true; 
		}
		else if (literal.get(1).equals(variable) && disequals.contains(literal.get(0))) {
			result = true;
		}
		else {
			result = false; // it is a disequality, but not implied.
		}
		
		return result;
	}

	@Override
	public boolean directlyImpliesDisequalityOfVariableAnd(Expression term, RewritingProcess process) {
		boolean result = disequals.contains(term);
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

	@Override
	public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
		// TODO: cache this operation because DisequalitiesConstraintForSingleVariable may be shared among many NonEqualitiesConstraints.
		Expression numberOfPossibleValuesForIndex;
		long numberOfNonAvailableValues = disequals.size();
		long variableDomainSize = getVariableDomainSize(process);
		if (variableDomainSize == -1) {
			Expression variableDomain = getVariableDomain(process);
			Expression variableDomainCardinality = apply(CARDINALITY, variableDomain);
			numberOfPossibleValuesForIndex = Minus.make(variableDomainCardinality, makeSymbol(numberOfNonAvailableValues));
		}
		else {
			numberOfPossibleValuesForIndex = makeSymbol(max(0, variableDomainSize - numberOfNonAvailableValues));
		}
		return numberOfPossibleValuesForIndex;
	}

	/**
	 * @param process
	 * @return
	 */
	private Expression getVariableDomain(RewritingProcess process) {
		Expression variableType = process.getContextualSymbolType(variable);
		if (variableType == null) {
			variableType = new DefaultSyntacticFunctionApplication(TYPE, variable);
		}
		return variableType;
	}

	public List<Expression> getSplittersToBeSatisfied() {
		return emptyList();
	}

	public List<Expression> getSplittersToBeNotSatisfied() {
		// TODO: OPTIMIZATION:
		// Cache this! Because DisequalitiesConstraintForSingleVariable is immutable after setup, you only need to run it once!
		List<Expression> result = mapIntoList(disequals, disequal -> apply(EQUALITY, variable, disequal)); // making it with apply instead of Equality.make sidesteps simplifications, which will not occur in this case because otherwise this condition would have either rendered the constraint a contradiction, or would have been eliminated from it
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> arguments = mapIntoList(disequals, d -> apply(DISEQUALITY, variable, d));
		Expression result = And.make(arguments);
		return result;
	}
}

