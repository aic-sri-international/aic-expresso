package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.util.Util.getFirst;

import java.util.Collection;
import java.util.Iterator;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.util.Util;

/**
 * An implementation of {@link NonEqualitiesForSingleTerm} in which the constraints are inequalities and disequalities.
 * @author braz
 *
 */
@SuppressWarnings("serial")
public class InequalitiesAndDisequalitiesConstraintForSingleVariable extends AbstractOwnRepresentationConstraint {

	private DisequalitiesConstraintForSingleVariable disequalities; // the basis for this constraint, which only adds the inequalities.
	private Collection<Expression> lowerBounds; // known lower bounds for index; if empty, we assume type lower bound.
	private Collection<Expression> upperBounds; // known upper bounds for index; if empty, we assume type upper bound.
	
	public InequalitiesAndDisequalitiesConstraintForSingleVariable(Expression variable, EqualityConstraintTheory theory, Collection<Expression> supportedIndices) {
		super(supportedIndices);
		disequalities = new DisequalitiesConstraintForSingleVariable(cachedInnerExpression, null, supportedIndices);
	}

	@Override
	public ConstraintTheory getTheory() {
		return disequalities.getTheory();
	}

	@Override
	public Collection<Expression> getSupportedIndices() {
		return disequalities.getSupportedIndices();
	}

	@Override
	public Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process) {
		Expression splitter;
		
		// lower bound needs to be unique
		if (lowerBounds.size() > 1) {
			Iterator<Expression> lowerBoundsIterator = lowerBounds.iterator();
			Expression first  = lowerBoundsIterator.next();
			Expression second = lowerBoundsIterator.next();
			splitter = apply(LESS_THAN_OR_EQUAL_TO, first, second); // order is irrelevant, we simply want them to be ordered somehow
		}
		else if (upperBounds.size() > 1) {
			Iterator<Expression> upperBoundsIterator = lowerBounds.iterator();
			Expression first  = upperBoundsIterator.next();
			Expression second = upperBoundsIterator.next();
			splitter = apply(LESS_THAN_OR_EQUAL_TO, first, second); // order is irrelevant, we simply want them to be ordered somehow
		}
		else {
			Expression min = getFirst(lowerBounds);
//			Expression disequalLessThanMin = Util.getFirstSatisfyingPredicateOrNull(disequalities.getDisequals(), d -> )
			
		}
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
	public Expression normalizeExpressionWithoutLiterals(Expression expression, RewritingProcess process) {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public AbstractOwnRepresentationConstraint clone() {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	protected void applyNormalizedSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		// TODO Auto-generated method stub
		
	}
	@Override
	protected Expression computeInnerExpression() {
		// TODO Auto-generated method stub
		return null;
	}
	
}

