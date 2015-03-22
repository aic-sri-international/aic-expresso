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
import static java.lang.Math.max;
import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
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
	
	/**
	 * Constructs a set of disequality constraints for variable to be used inside nonEqualitiesConstraint.
	 * @param variable
	 * @param nonEqualitiesConstraint
	 */
	public DisequalitiesConstraintForSingleVariable(Expression variable, NonEqualitiesConstraint nonEqualitiesConstraint) {
		super(variable, nonEqualitiesConstraint);
		this.disequals = Util.set();
		this.uniquelyValuedDisequals = Util.set();
		this.nonEqualitiesConstraint = nonEqualitiesConstraint;
	}

	@Override
	public DisequalitiesConstraintForSingleVariable clone() {
		DisequalitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, nonEqualitiesConstraint);
		result.cachedIndexDomainSize = cachedIndexDomainSize;
		result.disequals = new CopyOnWriteCollection<Expression>(disequals);
		result.uniquelyValuedDisequals = new CopyOnWriteCollection<Expression>(uniquelyValuedDisequals);
		return result;
	}
	
	@Override
	public DisequalitiesConstraintForSingleVariable incorporatePossiblyDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		Util.myAssert(() -> ! splitterSign && isEquality(splitter), () -> getClass() + " only allowed to take negative equality literals (disequalities) but got " + (splitterSign? "" : "not ") + " " + splitter);
		Util.myAssert(() -> splitter.get(0).equals(variable), () -> getClass() + " must only take splitters in which the first argument is the same as the main variable");
		Expression term = splitter.get(1);
		disequals.add(term);
		updateUniqueValuedDisequals(term, process);
		return this;
	}
	
	private void updateUniqueValuedDisequals(Expression term, RewritingProcess process) throws Contradiction {
		if (getIndexDomainSize(process) != -1) {
			if (forAll(uniquelyValuedDisequals, u -> nonEqualitiesConstraint.directlyImpliesLiteral(apply(DISEQUALITY, u, term), process))) {
				uniquelyValuedDisequals.add(term);
				if (uniquelyValuedDisequals.size() >= getIndexDomainSize(process)) {
					throw new Contradiction();
				}
			}
		}
	}
	
	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
		if ( ! indicesSubSet.isEmpty()) { // if empty, no splitters are needed. If not empty, indicesSubSet is necessarily a set with this.variable as only element.
			for (Expression disequal : disequals) {
				if (getTheory().isVariableTerm(disequal, process)) { // we can restrict disequal to variables because at least one of the splitter's arguments must be a variable (otherwise they would be two constants and we already know those are disequal).
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
				else if ( ! nonEqualitiesConstraint.directlyImpliesLiteral(apply(DISEQUALITY, disequal, anotherDisequal), process)) { // already disunified
					splitter = getTheory().makeSplitterFromFunctorAndVariableAndTermDistinctFromVariable(EQUALITY, disequal, anotherDisequal, getSupportedIndices(), process);
					return splitter;
				}
			}
		}
		return null;
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
		// OPTIMIZATION: skip the intermediary representation NonEqualitiesForSingleTerm and get instead a procedure parameter on what to do with each pair.
	}

	public List<Expression> getSplittersToBeSatisfied() {
		return emptyList();
	}

	public List<Expression> getSplittersToBeNotSatisfied() {
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