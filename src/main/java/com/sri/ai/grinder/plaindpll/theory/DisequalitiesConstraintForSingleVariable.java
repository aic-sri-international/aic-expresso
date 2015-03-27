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
import static com.sri.ai.util.Util.getFirstNonNullResultOrNull;
import static com.sri.ai.util.Util.iterateTillPastBothElementsByIdentity;
import static com.sri.ai.util.Util.iterateTillPastElementByIdentity;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static java.lang.Math.max;
import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.CopyOnWriteCollection;

/**
 * An implementation of {@link NonEqualitiesForSingleTerm} in which the only constraint is disequality;
 * it must be informed of its containing {@link NonEqualitiesConstraint}.
 * @author braz
 *
 */
@SuppressWarnings("serial")
public class DisequalitiesConstraintForSingleVariable extends AbstractNonEqualitiesConstraintForSingleVariable {
	private Collection<Expression> disequals;
	private Collection<Expression> uniquelyValuedDisequals; // disequals constrained to be disequal from all uniquely-valued disequals added before themselves. If this set reaches variable's domain size, there will be no value left for it and an inconsistency is indicated.
	private Expression lastUniquelyValuedDisequal;
	private Expression nextSplitter;
	private NonEqualitiesConstraint nonEqualitiesConstraintForNextSplitter;
	
	/**
	 * Constructs a set of disequality constraints for variable to be used inside nonEqualitiesConstraint.
	 * @param variable
	 * @param nonEqualitiesConstraint
	 */
	public DisequalitiesConstraintForSingleVariable(Expression variable, NonEqualitiesConstraint nonEqualitiesConstraint) {
		super(variable, nonEqualitiesConstraint);
		this.disequals = Util.set();
		this.uniquelyValuedDisequals = new LinkedHashSet<Expression>();
		this.lastUniquelyValuedDisequal = null;
		this.nextSplitter = null;
		this.nonEqualitiesConstraintForNextSplitter = null;
		this.nonEqualitiesConstraint = nonEqualitiesConstraint;
	}

	@Override
	public DisequalitiesConstraintForSingleVariable clone() {
		DisequalitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, nonEqualitiesConstraint);
		result.cachedIndexDomainSize = cachedIndexDomainSize;
		result.disequals = new CopyOnWriteCollection<Expression>(disequals, LinkedHashSet.class);
		result.uniquelyValuedDisequals = new CopyOnWriteCollection<Expression>(uniquelyValuedDisequals, LinkedHashSet.class);
		result.nextSplitter = nextSplitter;
		result.nonEqualitiesConstraintForNextSplitter = nonEqualitiesConstraintForNextSplitter;
		result.lastUniquelyValuedDisequal = lastUniquelyValuedDisequal;
		return result;
	}
	
	@Override
	public DisequalitiesConstraintForSingleVariable cloneWithNewNonEqualitiesConstraint(NonEqualitiesConstraint nonEqualitiesConstraint) {
		DisequalitiesConstraintForSingleVariable result = (DisequalitiesConstraintForSingleVariable) super.cloneWithNewNonEqualitiesConstraint(nonEqualitiesConstraint);
		result.nextSplitter = nextSplitter; // may not be true anymore under new nonEqualitiesConstraint, so needs to be checked when the time comes!
		result.nonEqualitiesConstraintForNextSplitter = nonEqualitiesConstraintForNextSplitter;
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
		if ((splitter = getSplitterTowardDisequalityOfAllTermsInACollectionAndAnotherTerm(uniquelyValuedDisequals, newDisequal, process)) == null) {
			uniquelyValuedDisequals.add(newDisequal);
			if (uniquelyValuedDisequals.size() == getVariableDomainSize(process)) { // Note that this works when getIndexDomainSize is -1 (no domain size information) because then the condition is always false
				// in principle, comparison is >=, but uniquelyValuedDisequals's size increases one at a time and == is cheaper.
				throw new Contradiction();
			}
			lastUniquelyValuedDisequal = newDisequal;
		}
		else {
			discardStoredSplitterIfObsolete(process);
			if (nextSplitter != null) { // we have it, and it is still good
				// keep the one we have; we could simply overwrite it with the new one, but keeping the original behavior (first one) for now.
			}
			else {
				nextSplitter = splitter;
				nonEqualitiesConstraintForNextSplitter = nonEqualitiesConstraint;
//				System.out.println("NEXT SPLITTER SELECTION");	
//				System.out.println("this: " + this);	
//				System.out.println("nextSplitter picked as: " + nextSplitter);	
//				System.out.println("with non-equalities   : " + nonEqualitiesConstraintForNextSplitter);	
			}
		}
	}
	
	private void discardStoredSplitterIfObsolete(RewritingProcess process) {
		if (nextSplitter != null && nonEqualitiesConstraint.directlyImpliesDisequality(nextSplitter.get(0), nextSplitter.get(1), process)) {
//			System.out.println("Discarding obsolete nextSplitter      : " + nextSplitter);
//			System.out.println("because nonEqualitiesConstraint is now: " + nonEqualitiesConstraint);	
			nextSplitter = null;
		}
	}
	
	private Expression getSplitterTowardDisequalityOfAllTermsInACollectionAndAnotherTerm(Collection<Expression> terms, Expression anotherTerm, RewritingProcess process) {
		Expression result = getFirstNonNullResultOrNull(terms, term -> getSplitterTowardDisequalityOfTwoTerms(term, anotherTerm, process));
		return result;
	}
	
	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {

		Expression result;
		discardStoredSplitterIfObsolete(process);
//		if (nextSplitter != null) {
//			result = nextSplitter;
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
			result = null;
			for (Expression disequal : disequals) {
				Expression splitter  = getSplitterTowardsEnsuringDisequalIsDisequalFromAllOtherDisequals(disequal, process);
				if (splitter != null) {
					result = splitter;
					break;
				}
			}
		}

//		if (nextSplitter != null && (result == null || ! nextSplitter.equals(result))) {
//			System.out.println("DISAGREEMENT");	
//			System.out.println("this: " + this);	
//			System.out.println("nextSplitter: " + nextSplitter);	
//			System.out.println("result: " + result);	
//			System.out.println("nonEqualitiesConstraint               : " + nonEqualitiesConstraint);	
//			System.out.println("nonEqualitiesConstraintForNextSplitter: " + nonEqualitiesConstraintForNextSplitter);	
//			System.out.println("indicesSubSet: " + indicesSubSet);	
//			System.exit(-1);
//		}

		return result;
	}

	private Expression getSplitterTowardsEnsuringDisequalIsDisequalFromAllOtherDisequals(Expression disequal, RewritingProcess process) {
		Iterator<Expression> anotherDisequalIterator = disequals.iterator();
		iterateTillPastElementByIdentity(anotherDisequalIterator, disequal); // we only need to look at other disequals past 'disequal'
		Expression result = null;
		while (result == null && anotherDisequalIterator.hasNext()) {
			Expression anotherDisequal = anotherDisequalIterator.next();
			if ( ! uniquelyValuedDisequals.contains(anotherDisequal)) { // we can skip all uniquely valued disequals, because we already know that they are disequal from all other previous disequals in insertion order, and 'disequal' comes before it in insertion order
				result = getSplitterTowardDisequalityOfTwoTerms(disequal, anotherDisequal, process);
			}
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