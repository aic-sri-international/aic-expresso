package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;

/**
 * An implementation of {@link NonEqualitiesForSingleTerm} in which the constraints are inequalities and disequalities.
 * @author braz
 *
 */
@SuppressWarnings("serial")
public class InequalitiesAndDisequalitiesConstraintForSingleVariable extends AbstractNonEqualitiesConstraintForSingleVariable {

	private DisequalitiesConstraintForSingleVariable disequalities; // the basis for this constraint, which only adds the inequalities. This still qualifies this class as keeping its "own representation" because its operation is not simply based on a translation of splitters and solutions.
	private Collection<Expression> lowerBounds; // known lower bounds (<=) for variable; if empty, we assume type lower bound.
	private Collection<Expression> upperBounds; // known upper bounds (>=) for variable; if empty, we assume type upper bound.
	private Collection<Expression> strictLowerBounds; // known strict lower bounds (<) for variable
	private Collection<Expression> strictUpperBounds; // known strict upper bounds (>) for variable
	
	public InequalitiesAndDisequalitiesConstraintForSingleVariable(Expression variable, EqualityConstraintTheory theory, Collection<Expression> supportedIndices) {
		super(variable, theory, supportedIndices);
		disequalities = new DisequalitiesConstraintForSingleVariable(cachedInnerExpression, null, supportedIndices);
	}

	@Override
	public Expression pickSplitterGivenExternalConstraint(Collection<Expression> indicesSubSet, Constraint externalConstraint, RewritingProcess process) {
		Expression splitter = null;
		
		// lower bound needs to be unique
		if (lowerBounds.size() > 1) {
			Iterator<Expression> lowerBoundsIterator = lowerBounds.iterator();
			Expression first  = lowerBoundsIterator.next();
			Expression second = lowerBoundsIterator.next();
			splitter = apply(LESS_THAN_OR_EQUAL_TO, first, second); // order is irrelevant, we simply want them to be ordered somehow
			return splitter; // we don't like to use early return statements, but here it will make the code cleaner and faster.
		}
		
		if (upperBounds.size() > 1) {
			Iterator<Expression> upperBoundsIterator = lowerBounds.iterator();
			Expression first  = upperBoundsIterator.next();
			Expression second = upperBoundsIterator.next();
			splitter = apply(LESS_THAN_OR_EQUAL_TO, first, second); // order is irrelevant, we simply want them to be ordered somehow
			return splitter;
		}
		
		splitter = disequalities.pickSplitterGivenExternalConstraint(indicesSubSet, externalConstraint, process);
		if (splitter != null) {
			return splitter;
		}

		// At this point both lowerBounds has at most one element each.
		// If they have a lower bound, it must be constrained to be less than disequals and upper bounds.
		// If they don't have a lower bound, it is the type bound and we don't need to check the following constraints.
		// The analogous facts hold for upper bounds.

		if (lowerBounds.size() == 1) {
			Expression min = getFirst(lowerBounds);
			splitter = getSplitterOfTermNotConstrainedToBeGreaterThanMin(disequalities.getDisequals(), min, externalConstraint, process);
			if (splitter != null) {
				return splitter;
			}

			splitter = getSplitterOfTermNotConstrainedToBeGreaterThanMin(upperBounds, min, externalConstraint, process);
			if (splitter != null) {
				return splitter;
			}
		}

		if (upperBounds.size() == 1) {
			Expression max = getFirst(upperBounds);
			splitter = getSplitterOfTermNotConstrainedToBeLessThanMax(disequalities.getDisequals(), max, externalConstraint, process);
			if (splitter != null) {
				return splitter;
			}

			splitter = getSplitterOfTermNotConstrainedToBeLessThanMax(lowerBounds, max, externalConstraint, process);
			if (splitter != null) {
				return splitter;
			}
		}
		
		return splitter;
	}

	private Expression getSplitterOfTermNotConstrainedToBeGreaterThanMin(Collection<Expression> terms, Expression min, Constraint externalConstraint, RewritingProcess process) {
		Expression splitter;
		Expression disequalNotConstrainedToBeGreaterThanMin = getTermNotConstrainedToBeGreaterThanMinOrNull(terms, min, externalConstraint, process);
		if (disequalNotConstrainedToBeGreaterThanMin != null) {
			splitter = apply(LESS_THAN_OR_EQUAL_TO, min, disequalNotConstrainedToBeGreaterThanMin);
		}
		else {
			splitter = null;
		}
		return splitter;
	}

	private Expression getTermNotConstrainedToBeGreaterThanMinOrNull(Collection<Expression> terms, Expression min, Constraint externalConstraint, RewritingProcess process) {
		Expression disequalNotConstrainedToBeGreaterThanMin = 
				getFirstSatisfyingPredicateOrNull(
						terms,
						d -> ! externalConstraint.directlyImpliesLiteral(apply(GREATER_THAN, min, d), process));
		return disequalNotConstrainedToBeGreaterThanMin;
	}
	
	private Expression getSplitterOfTermNotConstrainedToBeLessThanMax(Collection<Expression> terms, Expression max, Constraint externalConstraint, RewritingProcess process) {
		Expression splitter;
		Expression disequalNotConstrainedToBeLessThanMax = getTermNotConstrainedToBeLessThanMaxOrNull(terms, max, externalConstraint, process);
		if (disequalNotConstrainedToBeLessThanMax != null) {
			splitter = apply(LESS_THAN, disequalNotConstrainedToBeLessThanMax, max);
		}
		else {
			splitter = null;
		}
		return splitter;
	}

	private Expression getTermNotConstrainedToBeLessThanMaxOrNull(Collection<Expression> terms, Expression max, Constraint externalConstraint, RewritingProcess process) {
		Expression disequalNotConstrainedToBeLessThanMax = 
				getFirstSatisfyingPredicateOrNull(
						terms,
						d -> ! externalConstraint.directlyImpliesLiteral(apply(LESS_THAN, max, d), process));
		return disequalNotConstrainedToBeLessThanMax;
	}
	
	@Override
	public void incorporateDestructively(boolean splitterSign, Expression splitter, Constraint externalConstraint, RewritingProcess process) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public boolean directlyImpliesDisequalityOfVariableAnd(Expression term, RewritingProcess process) {
		// TODO Auto-generated method stub
		return false;
	}
	@Override
	public List<Expression> getSplittersToBeSatisfied() {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public List<Expression> getSplittersToBeNotSatisfied() {
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
	protected Expression computeInnerExpression() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Expression getVariable() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public AbstractNonEqualitiesConstraintForSingleVariable clone() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void updateRepresentativesDestructively(Function<Expression, Expression> getRepresentative, NonEqualitiesConstraint externalConstraint, RewritingProcess process) {
		// TODO Auto-generated method stub
		
	}
}

