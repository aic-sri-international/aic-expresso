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
import static com.sri.ai.util.Util.myAssert;
import static java.lang.Math.max;
import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.Collections;
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
	
	public boolean equals(Object another) {
		if (another instanceof DisequalitiesConstraintForSingleVariable) {
			DisequalitiesConstraintForSingleVariable anotherDisequalitiesConstraintForSingleVariable = (DisequalitiesConstraintForSingleVariable) another;
			return disequals.equals(anotherDisequalitiesConstraintForSingleVariable.disequals);
		}
		return false;
	}
	
	public int hashCode() {
		return disequals.hashCode();
	}
	
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
		result.disequals = new CopyOnWriteCollection<Expression>(disequals, LinkedHashSet.class);
		result.uniquelyValuedDisequals = new CopyOnWriteCollection<Expression>(uniquelyValuedDisequals, LinkedHashSet.class);
		return result;
	}
	
	public DisequalitiesConstraintForSingleVariable cloneWithNewVariable(Expression newVariable) {
		DisequalitiesConstraintForSingleVariable result = clone();
		result.variable = newVariable;
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
		disequals.add(term);
		updateUniqueValuedDisequals(term, process);
	}
	
	private void updateUniqueValuedDisequals(Expression term, RewritingProcess process) throws Contradiction {
		if (getIndexDomainSize(process) != -1) {
			if (forAll(uniquelyValuedDisequals, u -> nonEqualitiesConstraint.directlyImpliesDisequality(u, term, process))) {
				uniquelyValuedDisequals.add(term);
				if (uniquelyValuedDisequals.size() == getIndexDomainSize(process)) { // in principle, comparison is >=, but uniquelyValuedDisequals's size increases one at a time and == is cheaper.
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
		// TODO: OPTIMIZATION:
		// Cache this! Because DisequalitiesConstraintForSingleVariable is immutable after setup, you only need to run it once!
		// However, remember that this ultimately depends on the nonEqualitiesConstraint containing it, so its result cannot be reused after changing hands
		// (unless some more sophisticated analysis of introduced disequalities is performed).
	}

	private Expression getSplitterTowardsEnsuringVariableIsDisequalFromAllOtherTermsInCollection(Expression disequal, RewritingProcess process) {
		for (Expression anotherDisequal : disequals) {
			if ( ! anotherDisequal.equals(disequal)) {
				Expression splitter = getTermTheory().getSplitterTowardDisunifyingDistinctTerms(disequal, anotherDisequal, process); // if function applications, we need to disunify arguments first, for instance.
				if (splitter != null) {
					return splitter; // need to disunify first
				}
				else if ( ! nonEqualitiesConstraint.directlyImpliesDisequality(disequal, anotherDisequal, process)) { // already disunified
					splitter = getTheory().makeSplitterFromFunctorAndVariableAndTermDistinctFromVariable(EQUALITY, disequal, anotherDisequal, getSupportedIndices(), process);
					return splitter;
				}
			}
		}
		return null;
		// TODO: OPTIMIZATION:
		// Keep track of last disequal checked against and try to keep that information when incorporating new information.
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