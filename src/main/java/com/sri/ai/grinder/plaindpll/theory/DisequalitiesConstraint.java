package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;

/** 
 * An extension of {@link AbstractNonEqualitiesConstraint} to represent disequalities.
 */	
@SuppressWarnings("serial")
public class DisequalitiesConstraint extends AbstractNonEqualitiesConstraint  {

	public DisequalitiesConstraint(EqualityConstraintTheory theory, Collection<Expression> supportedIndices) {
		super(theory, supportedIndices);
	}
	
	protected DisequalitiesConstraint make() {
		return new DisequalitiesConstraint(theory, supportedIndices);
	}

	@Override
	public void incorporateNonTrivialSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process) {
		myAssert( () -> !(splitterSign && splitter.hasFunctor(EQUALITY)), () -> getClass() + " should not receive equality literal splitters but received " + splitter); 
		Expression variable  = splitter.get(0);
		Expression otherTerm = splitter.get(1);
		addNonTrivialDisequalityOfVariableAndAnotherTermDestructively(variable, otherTerm, process);
	}

	public void addFirstTermAsDisequalOfSecondTermDestructively(Expression term1, Expression term2, RewritingProcess process) {
		NonEqualitiesConstraintForSingleVariable nonEqualitiesConstraintForTerm1 = nonEqualitiesConstraintFor(term1, process);
		nonEqualitiesConstraintForTerm1.incorporateDestructively(false, Equality.make(term1, term2), this, process);
	}

	/**
	 * Indicates whether the constraints represented ever product splitters that need to be satisfied (default is true).
	 * @return
	 */
	protected boolean constraintsDoNotProduceSplittersToBeSatisfied() {
		return true; // all literals are disequality ones, so there are splitters whose negations need to be satisfied, but not positive ones.
	}
	
	/**
	 * Indicates whether the constraints represented ever product splitters that need to be <i>not</i> satisfied (default is false).
	 * @return
	 */
	protected boolean constraintsDoNotProduceSplittersToBeNotSatisfied() {
		return false;
	}

	protected NonEqualitiesConstraintForSingleVariable makeNonEqualitiesConstraintForVariable(Expression variable) {
		NonEqualitiesConstraintForSingleVariable result = new DisequalitiesConstraintForSingleVariable(variable, getTheory(), getSupportedIndices());
		return result;
	}
}