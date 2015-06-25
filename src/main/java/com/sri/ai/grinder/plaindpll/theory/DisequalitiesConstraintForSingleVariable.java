package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.Equality.isEquality;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.util.Util.fatalError;
import static com.sri.ai.util.Util.getIndexOfFirstSatisfyingPredicateOrMinusOne;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.mapIntoSetOrSameIfNoDistinctElementInstances;
import static com.sri.ai.util.Util.myAssert;
import static java.lang.Math.max;
import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.collect.ArrayHashSet;
import com.sri.ai.util.collect.ArraySet;
import com.sri.ai.util.collect.CopyOnWriteArraySet;

/**
 * An implementation of {@link NonEqualitiesForSingleTerm} in which the only constraint is disequality.
 * @author braz
 *
 */
@SuppressWarnings("serial")
public class DisequalitiesConstraintForSingleVariable extends AbstractNonEqualitiesConstraintForSingleVariable {
	private ArraySet<Expression> disequals;
	private ArraySet<Expression> uniquelyValuedDisequals; // disequals constrained to be disequal from all uniquely-valued disequals added before themselves. If this set reaches variable's domain size, there will be no value left for it and an inconsistency is indicated.
	private DisequalitiesSplitterSearchLowerBound splitterSearchLowerBound;
	private boolean allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound; // see occurrences of field for comments on the meaning of this field
	
	/**
	 * Constructs a set of disequality constraints for variable to be used inside nonEqualitiesConstraint.
	 * @param variable
	 * @param nonEqualitiesConstraint
	 */
	public DisequalitiesConstraintForSingleVariable(Expression variable, EqualityConstraintTheory theory, Collection<Expression> supportedIndices) {
		super(variable, theory, supportedIndices);
		this.disequals = new ArrayHashSet<Expression>();
		this.uniquelyValuedDisequals = new ArrayHashSet<Expression>();
		this.splitterSearchLowerBound = null;
		this.allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound = true;
	}

	@Override
	public DisequalitiesConstraintForSingleVariable clone() {
		DisequalitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, theory, supportedIndices);
		result.cachedIndexDomainSize = cachedIndexDomainSize;
		result.disequals = new CopyOnWriteArraySet<Expression>(disequals, () -> new ArrayHashSet<Expression>());
		result.uniquelyValuedDisequals = new CopyOnWriteArraySet<Expression>(uniquelyValuedDisequals, () -> new ArrayHashSet<Expression>());
		result.splitterSearchLowerBound = splitterSearchLowerBound == null? null : new DisequalitiesSplitterSearchLowerBound(splitterSearchLowerBound);
		result.allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound = allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound;
		return result;
	}

	public Collection<Expression> getDisequals() {
		return Collections.unmodifiableCollection(disequals);
	}
	
	@Override
	public void incorporateDestructively(boolean splitterSign, Expression splitter, Constraint externalConstraint, RewritingProcess process) {
		myAssert(() -> ! splitterSign && isEquality(splitter), () -> getClass() + " only allowed to take negative equality literals (disequalities) but got " + (splitterSign? "" : "not ") + " " + splitter);
		Expression term;
		if (splitter.get(0).equals(variable)) {
			term = splitter.get(1);
		}
		else if (splitter.get(1).equals(variable)) {
			term = splitter.get(0);
		}
		else {
			fatalError(getClass() + " must only take splitters with one of its arguments the same as the main variable");
			term = null; // never used.
		}
		incorporateDisequalDestructively(term, externalConstraint, process);
	}

	private void incorporateDisequalDestructively(Expression term, Constraint externalConstraint, RewritingProcess process) throws Contradiction {
		boolean termIsNewDisequal = disequals.add(term);
		if (termIsNewDisequal) {
			updateUniqueValuedDisequalsWithNewDisequal(term, externalConstraint, process);
		}
	}
	
	private void updateUniqueValuedDisequalsWithNewDisequal(Expression newDisequal, Constraint externalConstraint, RewritingProcess process) throws Contradiction {
		
		int indexOfFirstNotNecessarilyDisequalTermToNewDisequal;
		int newDisequalIndex = disequals.size() - 1;
		int result = getIndexOfFirstSatisfyingPredicateOrMinusOne(uniquelyValuedDisequals, term -> ! externalConstraint.directlyImpliesLiteral(apply(DISEQUALITY, term, newDisequal), process));
//		int result = getIndexOfFirstSatisfyingPredicateOrMinusOne(uniquelyValuedDisequals, term -> ! disequalityDirectlyImpliedExternally.apply(term, newDisequal));
		if ((indexOfFirstNotNecessarilyDisequalTermToNewDisequal = result) == -1) {
			uniquelyValuedDisequals.add(newDisequal);
			if (uniquelyValuedDisequals.size() == getVariableDomainSize(process)) { // Note that this works when getIndexDomainSize is -1 (no domain size information) because then the condition is always false
				// in principle, comparison is >=, but uniquelyValuedDisequals's size increases one at a time and == is cheaper.
				throw new Contradiction();
			}

			// myAssert(allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound, "If all disequals are uniquely valued so far, we should not have allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound set to false yet");
			if (disequals.size() == uniquelyValuedDisequals.size()) {
				// We can speed up future searches for splitters by indicating that
				// you can start from (index1, index2) equal to (disequals.size(), disequals.size() - 1).
				// All pairs coming before that are uniquely valued and will not form a splitter.
				splitterSearchLowerBound = new DisequalitiesSplitterSearchLowerBound(disequals.size(), newDisequalIndex);
			}
		}
		else {
			if (allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound) { // this update is only allowed the first time (see last line of block)
				// 'index' indicates where first splitter candidate is, so set splitter search lower separator accordingly to save time
				// NOTE: remember 'indexOfFirstNotNecessarilyDisequalTermToNewDisequal' is an index for uniquelyValuedDisequals,
				// and that the lower separator is expressed in indices on 'disequals'.
				// However, the fact that we did not have a splitter so far
				// means that all terms added so far were uniquely valued, and therefore uniquelyValuedDisequals is equal to 'disequals' at this point,
				// so 'indexOfFirstNotNecessarilyDisequalTermToNewDisequal' applies to 'disequals' as well.
				splitterSearchLowerBound = new DisequalitiesSplitterSearchLowerBound(newDisequalIndex, indexOfFirstNotNecessarilyDisequalTermToNewDisequal);
				allowInsertedNotUniquelyValuedDisequalToAdvanceLowerBound = false; // from now on we must not make such updates because that may lead us to inadvertently skip a splitter candidate.
			}
		}
	}
	
	@Override
	public Expression pickSplitterGivenExternalConstraint(Collection<Expression> indicesSubSet, Constraint externalConstraint, RewritingProcess process) {

		Expression result;
		
		if (indicesSubSet.isEmpty()) { // if empty, no splitters are needed. If not empty, indicesSubSet is necessarily a set with this.variable as only element.
			result = null;
		}
		else {
			if (splitterSearchLowerBound == null) { // never found a splitter during setup, won't find it now
				result = null;
			}
			else {
				BinaryFunction<Expression, Expression, Expression> getSplitterTowardDisequalityOfTwoTerms = (t1, t2) -> getSplitterTowardDisequalityOfTwoTerms(t1, t2, externalConstraint, process);
				result = splitterSearchLowerBound.getCurrentSplitter(disequals, uniquelyValuedDisequals, externalConstraint, getSplitterTowardDisequalityOfTwoTerms, process);
			}
		}

		return result;
	}

	private Expression getSplitterTowardDisequalityOfTwoTerms(
			Expression term1, Expression term2, Constraint externalConstraint, RewritingProcess process) {
		
		Expression result = null;
		Expression splitter = getTermTheory().getSplitterTowardDisunifyingDistinctTerms(term1, term2, process); // if function applications, we need to disunify arguments first, for instance.
		if (splitter != null) {
			result = splitter;
		}
		else if (! externalConstraint.directlyImpliesLiteral(apply(DISEQUALITY, term1, term2), process)) { // already disunified but not disequal
			splitter = getConstraintTheory().makeSplitterFromFunctorAndVariableAndTermDistinctFromVariable(EQUALITY, term1, term2, getSupportedIndices(), process);
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
		myAssert(() -> splitter.hasFunctor(EQUALITY), () -> getClass() + " may only receive equality splitters.");
		
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

	private Expression cachedNumberOfPossibleValuesForIndex = null;
	
	@Override
	public Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process) {
		// caching this operation because DisequalitiesConstraintForSingleVariable may be shared among many NonEqualitiesConstraints.
		if (cachedNumberOfPossibleValuesForIndex == null) {
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
			cachedNumberOfPossibleValuesForIndex = numberOfPossibleValuesForIndex;
		}
		return cachedNumberOfPossibleValuesForIndex;
	}

	public List<Expression> getSplittersToBeSatisfied() {
		return emptyList();
	}

	private List<Expression> cachedSplittersToBeNotSatisfied = null;
	
	public List<Expression> getSplittersToBeNotSatisfied() {
		// caching this operation because DisequalitiesConstraintForSingleVariable may be shared among many NonEqualitiesConstraints.
		if (cachedSplittersToBeNotSatisfied == null) {
			cachedSplittersToBeNotSatisfied = mapIntoList(disequals, disequal -> apply(EQUALITY, variable, disequal)); // making it with apply instead of Equality.make sidesteps simplifications, which will not occur in this case because otherwise this condition would have either rendered the constraint a contradiction, or would have been eliminated from it
		}
		return cachedSplittersToBeNotSatisfied;
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> arguments = mapIntoList(disequals, d -> apply(DISEQUALITY, variable, d));
		Expression result = And.make(arguments);
		return result;
	}

	@Override
	public void updateRepresentativesDestructively(Function<Expression, Expression> getRepresentative, NonEqualitiesConstraint externalConstraint, RewritingProcess process) {
		Expression variable = getVariable();
		Collection<Expression> disequals = getDisequals();
		
		Expression variableRepresentative = variable.replaceAllOccurrences(getRepresentative, process); // NOTE: variableRepresentative needs not be a variable itself.
		Collection<Expression> disequalsRepresentatives = mapIntoSetOrSameIfNoDistinctElementInstances(disequals, t -> t.replaceAllOccurrences(getRepresentative, process));
	
		if (variableRepresentative != variable || disequalsRepresentatives != disequals) { // otherwise, nothing to do
			externalConstraint.removeNonEqualitiesForGivenVariableDestructively(variable); // note that an entry for this same variable *may* be re-created if some other term being updated gets updated to it
			for (Expression disequalRepresentative : disequalsRepresentatives) {
				externalConstraint.incorporateDisequalityDestructively(variableRepresentative, disequalRepresentative, process);
			}
		}
	}

	@Override
	public void informDestructively(Expression literal, RewritingProcess process) {
		// single-variable disequality constraints are not affected by literals (of the type stored in them, !=)
		// stored in other instances in the same {@link NonEqualitiesConstraint},
		// so do nothing.
	}
}

