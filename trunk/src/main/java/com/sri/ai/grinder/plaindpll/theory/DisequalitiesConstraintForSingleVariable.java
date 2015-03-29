package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.Equality.isEquality;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;
import static com.sri.ai.util.Util.getFirstNonNullResultOrNull;
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
import com.sri.ai.util.Util;
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
	
	private static class SplitterSearch {
		public Expression nextSplitter;
		public NonEqualitiesConstraint nonEqualitiesConstraintForNextSplitter;
		public int index1;
		public Expression disequal;
		public boolean disequalIsUniquelyValued;
		public int index2;
		public Expression previousDisequal;
		
		@Override
		public SplitterSearch clone() {
			SplitterSearch result = new SplitterSearch();
			result.nextSplitter = nextSplitter;
			result.nonEqualitiesConstraintForNextSplitter = nonEqualitiesConstraintForNextSplitter;
			result.index1 = index1;
			result.disequal = disequal;
			result.disequalIsUniquelyValued = disequalIsUniquelyValued;
			result.index2 = index2;
			result.previousDisequal = previousDisequal;
			return result;
		}
		
		@Override
		public String toString() {
			return 
			nextSplitter + "\n" +
			nonEqualitiesConstraintForNextSplitter + "\n" +
			index1 + "\n" +
			disequal + "\n" +
			disequalIsUniquelyValued + "\n" +
			index2 + "\n" +
			previousDisequal;
		}

		@Override
		public boolean equals(Object another) {
			if (another instanceof SplitterSearch) {
				SplitterSearch anotherSplitterSearch = (SplitterSearch) another;
				return
						anotherSplitterSearch.nextSplitter == nextSplitter &&
						anotherSplitterSearch.nonEqualitiesConstraintForNextSplitter == nonEqualitiesConstraintForNextSplitter &&
						anotherSplitterSearch.index1 == index1 &&
						anotherSplitterSearch.disequal == disequal &&
						anotherSplitterSearch.disequalIsUniquelyValued == disequalIsUniquelyValued &&
						anotherSplitterSearch.index2 == index2 &&
						anotherSplitterSearch.previousDisequal == previousDisequal;
			}
			return false;
		}
	}
	
	private SplitterSearch splitterSearch = null;
	
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
		
		this.splitterSearch = null; // other splitter search fields do not need values if this is false
	}

	@Override
	public DisequalitiesConstraintForSingleVariable clone() {
		DisequalitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, nonEqualitiesConstraint);
		result.cachedIndexDomainSize = cachedIndexDomainSize;
		result.disequals = new CopyOnWriteArraySet<Expression>(disequals, ArrayHashSet.class);
		result.uniquelyValuedDisequals = new CopyOnWriteArraySet<Expression>(uniquelyValuedDisequals, ArrayHashSet.class);
		result.lastUniquelyValuedDisequal = lastUniquelyValuedDisequal;

		result.splitterSearch = splitterSearch;

		return result;
	}

	@Override
	public DisequalitiesConstraintForSingleVariable cloneWithNewNonEqualitiesConstraint(NonEqualitiesConstraint nonEqualitiesConstraint) {
		DisequalitiesConstraintForSingleVariable result = (DisequalitiesConstraintForSingleVariable) super.cloneWithNewNonEqualitiesConstraint(nonEqualitiesConstraint);
		result.splitterSearch = splitterSearch; // may not be true anymore under new nonEqualitiesConstraint, so needs to be checked when the time comes!
//		if (splitterSearch == null) {
//			System.out.println("SPLITTER SEARCH COPIED: null");	
//			System.out.println("Giving: " + this + " " + System.identityHashCode(this));	
//			System.out.println("Receiving: " + result + " " + System.identityHashCode(result));	
//		}
//		else {
//			System.out.println("SPLITTER SEARCH COPIED:");
//			System.out.println("Giving: " + this + " " + System.identityHashCode(this));	
//			System.out.println("Receiving: " + result + " " + System.identityHashCode(result));	
//			System.out.println("nextSplitter: " + splitterSearch.nextSplitter);
//			System.out.println("nonEqualitiesConstraintForNextSplitter: " + splitterSearch.nonEqualitiesConstraintForNextSplitter);
//			System.out.println("index1: " + splitterSearch.index1);
//			System.out.println("index2: " + splitterSearch.index2);
//		}
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
		Expression splitter;
		int index;
		if ((index = getIndexOfFirstTermWithoutDirectlyImpliedDisequalityWithAnotherTerm(uniquelyValuedDisequals, newDisequal, process)) == -1) {
//		if ((splitter = getSplitterTowardDisequalityOfAllTermsInACollectionAndAnotherTerm(uniquelyValuedDisequals, newDisequal, process)) == null) {
			uniquelyValuedDisequals.add(newDisequal);
			if (uniquelyValuedDisequals.size() == getVariableDomainSize(process)) { // Note that this works when getIndexDomainSize is -1 (no domain size information) because then the condition is always false
				// in principle, comparison is >=, but uniquelyValuedDisequals's size increases one at a time and == is cheaper.
				throw new Contradiction();
			}
			lastUniquelyValuedDisequal = newDisequal;
		}
		else {
			if ( ! currentSplitterIsStillValid(process)) {
				splitterSearch = new SplitterSearch();
				// 'index' indicates where first splitter will be, so initialize splitter search data accordingly to save time
				// NOTE: remember 'index' is an index for uniquelyValuedDisequals; however, the fact that we did not have a splitter yet
				// means that all terms added so far were uniquely valued, and therefore uniquelyValuedDisequals is equal to 'disequals' at this point,
				// so 'index' applies to 'disequals' as well.
				splitterSearch.index1 = disequals.size() - 1; // index of current 'newDisequal'
				updateSplitterSearchAfterUpdateToSplitterSearchIndex1();
				splitterSearch.index2 = index; // index of the term before newDisequal that is not necessarily disequal to it
				updateSplitterSearchAfterUpdateToSplitterSearchIndex2();
			}
//			if ( ! splitterSearchInitialized) { // if we do not have the next splitter already lined up
//				// 'index' indicates where first splitter will be, so initialize splitter search data accordingly to save time
//				// NOTE: remember 'index' is an index for uniquelyValuedDisequals; however, the fact that we did not have a splitter yet
//				// means that all terms added so far were uniquely valued, and therefore uniquelyValuedDisequals is equal to 'disequals' at this point,
//				// so 'index' applies to 'disequals' as well.
//				index1 = disequals.size() - 1; // index of current 'newDisequal'
//				updateSplitterSearchAfterUpdateToSplitterSearchIndex1();
//				index2 = index; // index of the term before newDisequal that is not necessarily disequal to it
//				updateSplitterSearchAfterUpdateToSplitterSearchIndex2();
//				splitterSearchInitialized = true;
//				
//				System.out.println("SAVING SPLITTER SEARCH INFO");	
//				System.out.println("this: " + this + " identity " + System.identityHashCode(this));
//				System.out.println("uniquelyValuedDisequals: " + uniquelyValuedDisequals);	
//				System.out.println("disequals: " + disequals);	
//				System.out.println("nextSplitter will be on : " + disequals.get(index2) + " = " + disequals.get(index1));	
//			}
		}
	}
	
//	private void discardStoredSplitterIfObsolete(RewritingProcess process) {
//		if (nextSplitter != null && nonEqualitiesConstraint != nonEqualitiesConstraintForNextSplitter && nonEqualitiesConstraint.directlyImpliesDisequality(nextSplitter.get(0), nextSplitter.get(1), process)) {
////			System.out.println("Discarding obsolete nextSplitter      : " + nextSplitter);
////			System.out.println("because nonEqualitiesConstraint is now: " + nonEqualitiesConstraint);	
//			nextSplitter = null;
//		}
//	}
	
	private Expression getSplitterTowardDisequalityOfAllTermsInACollectionAndAnotherTerm(Collection<Expression> terms, Expression anotherTerm, RewritingProcess process) {
		Expression result = getFirstNonNullResultOrNull(terms, term -> getSplitterTowardDisequalityOfTwoTerms(term, anotherTerm, process));
		return result;
	}
	
	private int getIndexOfFirstTermWithoutDirectlyImpliedDisequalityWithAnotherTerm(Collection<Expression> terms, Expression anotherTerm, RewritingProcess process) {
		int result = getIndexOfFirstSatisfyingPredicateOrMinusOne(terms, term -> ! nonEqualitiesConstraint.directlyImpliesDisequality(term, anotherTerm, process));
		return result;
	}
	
	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {

		Expression result;
//		discardStoredSplitterIfObsolete(process);
		
//		if (splitterSearch != null
//				&&
//				splitterSearch.nextSplitter != null
//				&& 
//				// constraint under which nextSplitter was computed is still the same, or there is a new one but it does not invalidate it.
//				(nonEqualitiesConstraint == splitterSearch.nonEqualitiesConstraintForNextSplitter
//				|| ! nonEqualitiesConstraint.directlyImpliesDisequality(splitterSearch.nextSplitter.get(0), splitterSearch.nextSplitter.get(1), process))) {
//			result = splitterSearch.nextSplitter;
////			System.out.println("Used nextSplitter: " + nextSplitter);
////			System.out.println("for constraint   : " + this);	
		////			System.out.println("for nonEqualitiesConstraint              : " + nonEqualitiesConstraint);	
		////			System.out.println("nonEqualitiesConstraintForNextSplitter   : " + nonEqualitiesConstraintForNextSplitter);	
		//		}
		//		else 
		if (indicesSubSet.isEmpty()) { // if empty, no splitters are needed. If not empty, indicesSubSet is necessarily a set with this.variable as only element.
			result = null;
		}
		else {
			if (currentSplitterIsStillValid(process)) {
				result = splitterSearch.nextSplitter;
				//		System.out.println("Used nextSplitter: " + nextSplitter);
				//		System.out.println("for constraint   : " + this);	
				//		System.out.println("for nonEqualitiesConstraint              : " + nonEqualitiesConstraint);	
				//		System.out.println("nonEqualitiesConstraintForNextSplitter   : " + nonEqualitiesConstraintForNextSplitter);	
			}
			else {
				splitterSearch = null;
				initializeSplitterSearch();

//				System.out.println("GOING TO COMPUTE newResult for " + System.identityHashCode(this));	
//				showState();	

				Expression justAVariable = findNextSplitter(process); // SUBTLE! findNextSplitter may change the identity of splitterSearch, so a direct assignment is not correct
				splitterSearch.nextSplitter = justAVariable;
				splitterSearch.nonEqualitiesConstraintForNextSplitter = nonEqualitiesConstraint;
				result = splitterSearch.nextSplitter;
//				System.out.println("COMPUTED newResult " + result + " for " + System.identityHashCode(this));	
//				System.out.println("index1: " + splitterSearch.index1);	
//				System.out.println("index2: " + splitterSearch.index2);	

				//			// REDO COMPUTATION FOR DEBUGGING PURPOSES
				//			Expression newResult = result;
				//			splitterSearch = null;
				//			
				//			if (splitterSearch == null) {
				//				initializeSplitterSearch();
				//			}
				//			Expression findNextSplitter = findNextSplitter(process);
				//			splitterSearch.nextSplitter = findNextSplitter;
				//			splitterSearch.nonEqualitiesConstraintForNextSplitter = nonEqualitiesConstraint;
				//			result = splitterSearch.nextSplitter;

				//			if ( ! Util.equals(result, newResult)) {
				//				System.out.println("DISAGREEMENT");	
				//				System.out.println("this: " + this + " identity " + System.identityHashCode(this));	
				//				System.out.println("newResult: " + newResult);	
				//				System.out.println("result   : " + result);	
				//				System.out.println("nonEqualitiesConstraint               : " + nonEqualitiesConstraint);	
				//				System.out.println("nonEqualitiesConstraintForNextSplitter: " + splitterSearch.nonEqualitiesConstraintForNextSplitter);	
				//				System.out.println("indicesSubSet: " + indicesSubSet);	
				//				System.exit(-1);
				//			}
			}
		}

		return result;
	}

	/**
	 * @param process
	 * @return
	 */
	private boolean currentSplitterIsStillValid(RewritingProcess process) {
		boolean valid = splitterSearch != null
				&&
				splitterSearch.nextSplitter != null
				&& 
				// constraint under which nextSplitter was computed is still the same, or there is a new one but it does not invalidate it.
				(nonEqualitiesConstraint == splitterSearch.nonEqualitiesConstraintForNextSplitter
				|| ! nonEqualitiesConstraint.directlyImpliesDisequality(splitterSearch.nextSplitter.get(0), splitterSearch.nextSplitter.get(1), process));
		return valid;
	}

	private void initializeSplitterSearch() {
		splitterSearch = new SplitterSearch();
		if (disequals.isEmpty()) {
			splitterSearch.index1 = 0;
		}
		else {
			splitterSearch.index1 = 1; // this is necessary so that index2 gets set to 0 and previousDisequal gets a valid value
		}
		updateSplitterSearchAfterUpdateToSplitterSearchIndex1();
	}

	private boolean splitterSearchNotOver() {
		return splitterSearch.index1 != disequals.size();
	}

	private void incrementSplitterSearch() {
		splitterSearch.index2++;
		if (splitterSearch.index2 == splitterSearch.index1) {
			splitterSearch.index2 = 0;
			incrementSplitterSearchIndex1();
		}
		updateSplitterSearchAfterUpdateToSplitterSearchIndex2();
	}
	
	private boolean splitterSearchIndex1NotOver() {
		return splitterSearch.index1 != disequals.size();
	}

	private void incrementSplitterSearchIndex1() {
		splitterSearch.index1++;
		updateSplitterSearchAfterUpdateToSplitterSearchIndex1();
	}

	private void updateSplitterSearchAfterUpdateToSplitterSearchIndex1() {
		if (splitterSearch.index1 != disequals.size()) {
			splitterSearch.disequal = disequals.get(splitterSearch.index1);
			splitterSearch.disequalIsUniquelyValued = uniquelyValuedDisequals.contains(splitterSearch.disequal);
			splitterSearch.index2 = 0;
			updateSplitterSearchAfterUpdateToSplitterSearchIndex2();
		}
	}

	private boolean splitterSearchIndex2NotOver() {
		return splitterSearch.index2 != splitterSearch.index1;
	}

	private void incrementSplitterSearchIndex2() {
		splitterSearch.index2++;
		updateSplitterSearchAfterUpdateToSplitterSearchIndex2();
	}

	private void updateSplitterSearchAfterUpdateToSplitterSearchIndex2() {
		if (splitterSearchIndex2NotOver()) {
			splitterSearch.previousDisequal = disequals.get(splitterSearch.index2);
		}
	}

	/**
	 * @param process
	 * @return
	 */
	private Expression findNextSplitter(RewritingProcess process) {
		Expression newResult = null;
//		String aux0 = splitterSearch.toString();
//		SplitterSearch aux = splitterSearch.clone();
		while (newResult == null && splitterSearchNotOver()) {
			if ( ! (splitterSearch.disequalIsUniquelyValued && uniquelyValuedDisequals.contains(splitterSearch.previousDisequal))) { // if they are both uniquely valued, we know they are constrained to be disequal
				newResult = getSplitterTowardDisequalityOfTwoTerms(splitterSearch.disequal, splitterSearch.previousDisequal, process);
			}
			incrementSplitterSearch();
		}
//		splitterSearch = aux;
//		if ( ! splitterSearch.toString().equals(aux0)) {
//			System.out.println("Not equal!!!");	
//			System.exit(-1);
//		}
		
//		Expression result = null;
//		while (result == null && splitterSearchIndex1NotOver()) {
//			result = getSplitterTowardsEnsuringDisequalAtIndex1IsDisequalFromAllOtherDisequalsBeforeIt(process);
//			incrementSplitterSearchIndex1();
//		}
//
//		System.out.println("findSplitter: " + result);
		
		return newResult;
	}

	private Expression getSplitterTowardsEnsuringDisequalAtIndex1IsDisequalFromAllOtherDisequalsBeforeIt(RewritingProcess process) {
		Expression result = null;
		while (result == null && splitterSearchIndex2NotOver()) {
			if ( ! (splitterSearch.disequalIsUniquelyValued && uniquelyValuedDisequals.contains(splitterSearch.previousDisequal))) { // if they are both uniquely valued, we know they are constrained to be disequal
				result = getSplitterTowardDisequalityOfTwoTerms(splitterSearch.disequal, splitterSearch.previousDisequal, process);
			}
			incrementSplitterSearchIndex2();
		}
		return result;
	}

	/**
	 * @param term1
	 * @param term2
	 * @param process
	 * @return
	 */
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

