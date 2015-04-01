package com.sri.ai.grinder.plaindpll.theory;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.BinaryPredicate;
import com.sri.ai.util.collect.ArraySet;

/**
 * The search for splitters on a set of disequalities from a single variable happens by iterating over each of them
 * (term1, with index equal to index1) and checking it against all terms before it (term2, with index equal to index2).
 * index2 must always be less than index1, and when index1 is equal to the number of disequals, the search is over.
 * There is a total lexicographic order on these pairs defined in the following way:
 * <p>
 * (index1, index2) > (index1', index2') if and only if index1 > index1' or index1 == index1' and index2 > index2'. 
 * <p>
 * {@link DisequalitiesSplitterSearchLowerBound}s are objects representing a lower bound on the indices (index1, index2) for the next splitter to be found.
 * They can also be used to find the next valid splitter.
 * They are meant to be shared among such constraints that descend from each other as much as possible, as to minimize the search.
 * For this reason, they do not carry a reference to a {@link DisequalitiesConstraintForSingleVariable} they belong to,
 * but instead receive data about the one they are being currently used for.
 * 
 * @author braz
 *
 */
class DisequalitiesSplitterSearchLowerBound {
	private Expression currentSplitter;
	private int index1;
	private int index2; 

	/** Creates splitter search and sets the position from which to start the next search. */
	public DisequalitiesSplitterSearchLowerBound(int index1, int index2) {
		this.index1 = index1;
		this.index2 = index2;
	}
	
	/** Creates splitter search and sets a current splitter for the given position. */
	public DisequalitiesSplitterSearchLowerBound(
			int index1, int index2,
			ArraySet<Expression> disequals, ArraySet<Expression> uniquelyValuedDisequals,
			BinaryFunction<Expression, Expression, Expression> splitterMaker) {
		
		currentSplitter = splitterMaker.apply(disequals.get(index1), disequals.get(index2));
		this.index1 = index1;
		this.index2 = index2;
		goToNextPosition(disequals, uniquelyValuedDisequals);
	}
	
	public DisequalitiesSplitterSearchLowerBound(DisequalitiesSplitterSearchLowerBound another) {
		copy(another);
	}

	private void copy(DisequalitiesSplitterSearchLowerBound another) {
		this.currentSplitter = another.currentSplitter;
		this.index1 = another.index1;
		this.index2 = another.index2;
	}

	public Expression getCurrentSplitter(
			ArraySet<Expression> disequals, ArraySet<Expression> uniquelyValuedDisequals,
			BinaryPredicate<Expression, Expression> disequalityDirectlyImpliedExternally,
			BinaryFunction<Expression, Expression, Expression> getSplitterTowardDisequalityOfTwoTerms,
			RewritingProcess process) {
		if (needsToBeSearched(disequalityDirectlyImpliedExternally, process)) {
			goToNextSplitter(disequals, uniquelyValuedDisequals, getSplitterTowardDisequalityOfTwoTerms);
		}
		return currentSplitter;
	}
	
	private boolean needsToBeSearched(BinaryPredicate<Expression, Expression> disequalityDirectlyImpliedExternally, RewritingProcess process) {
		// needs to be search if it has not been searched yet, or if it's obsolete for the current non-equalities
		boolean result = currentSplitter == null || ! nonNullCurrentSplitterStillValid(disequalityDirectlyImpliedExternally, process);
		return result;
	}

	private boolean nonNullCurrentSplitterStillValid(BinaryPredicate<Expression, Expression> disequalityDirectlyImpliedExternally, RewritingProcess process) {
		boolean result = ! disequalityDirectlyImpliedExternally.apply(currentSplitter.get(0), currentSplitter.get(1));
		return result;
	}

	private void goToNextSplitter(
			ArraySet<Expression> disequals, ArraySet<Expression> uniquelyValuedDisequals,
			BinaryFunction<Expression, Expression, Expression> getSplitterTowardDisequalityOfTwoTerms) {
		
		currentSplitter = null;
		while (currentSplitter == null && hasNextPosition(disequals)) {
			Expression disequal = disequals.get(index1);
			Expression previousDisequal = disequals.get(index2);
			if ( ! (uniquelyValuedDisequals.contains(disequal) && uniquelyValuedDisequals.contains(previousDisequal))) { // if they are both uniquely valued, we know they are constrained to be disequal
				currentSplitter = getSplitterTowardDisequalityOfTwoTerms.apply(disequal, previousDisequal);
			}
			goToNextPosition(disequals, uniquelyValuedDisequals);
		}
	}

	private boolean hasNextPosition(Collection<Expression> disequals) {
		return index1 != disequals.size();
	}

	private void goToNextPosition(ArraySet<Expression> disequals, ArraySet<Expression> uniquelyValuedDisequals) {
		index2++;
		if (index2 == index1) {
			index2 = 0;
			index1++;
		}
	}

	@Override
	public String toString() {
		return 
		"splitter: " + currentSplitter + "\n" +
		"index1: " + index1 + "\n" +
		"index2: " + index2;
	}

	@Override
	public boolean equals(Object another) {
		if (another instanceof DisequalitiesSplitterSearchLowerBound) {
			DisequalitiesSplitterSearchLowerBound anotherSplitterSearch = (DisequalitiesSplitterSearchLowerBound) another;
			return
					anotherSplitterSearch.currentSplitter == currentSplitter &&
					anotherSplitterSearch.index1 == index1 &&
					anotherSplitterSearch.index2 == index2;
		}
		return false;
	}
}