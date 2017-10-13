package com.sri.ai.grinder.sgdpllt.group;

import static com.sri.ai.expresso.helper.Expressions.ZERO;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;

/**
 * An abstract implementation of groups on numbers, capable of processing symbolic numeric expressions,
 * and leaving the additive operation to be specified by abstract methods.
 * 
 * @author braz
 *
 */
public abstract class AbstractNumericGroup extends AbstractAssociativeCommutativeGroup {

	public AbstractNumericGroup() {
		super();
	}

	@Override
	public abstract Expression additiveIdentityElement();

	@Override
	public abstract Expression add(Expression value1, Expression value2, Context context);

	@Override
	public abstract boolean isAdditiveAbsorbingElement(Expression value);

	/**
	 * Implements the addition of an element to itself n times,
	 * assuming that neither the element or n are conditional, and that n is not the constant zero.
	 * Result does not need to be normalized.
	 * @param valueToBeAdded
	 * @param n
	 * @return
	 */
	protected abstract Expression addNTimesWithUnconditionalValueAndNDistinctFromZero(Expression valueToBeAdded, Expression n);

	@Override
	public Expression addNTimes(Expression valueToBeAdded, Expression n, Context context) {
		Expression result;
		if (n.equals(ZERO)) {
			result = additiveIdentityElement();
		}
		else if (IfThenElse.isIfThenElse(n)) { // it is important that this condition is tested before the next, because n can be conditional on splitters while valueToBeSummed can be conditioned on conditions in the unconditional solution language (such as | Everything | - 1 > 0), and we want splitters to be over non-splitter conditions
			Expression condition  = IfThenElse.condition(n);
			Expression thenBranch = IfThenElse.thenBranch(n);
			Expression elseBranch = IfThenElse.elseBranch(n);
			Expression newThenBranch = addNTimes(valueToBeAdded, thenBranch, context);
			Expression newElseBranch = addNTimes(valueToBeAdded, elseBranch, context);
			result = IfThenElse.make(condition, newThenBranch, newElseBranch, false); // do not simplify to condition so it is normalized
		}
		else if (IfThenElse.isIfThenElse(valueToBeAdded)) {
			Expression condition = IfThenElse.condition(valueToBeAdded);
			Expression thenBranch = IfThenElse.thenBranch(valueToBeAdded);
			Expression elseBranch = IfThenElse.elseBranch(valueToBeAdded);
			
			Expression newThenBranch = addNTimes(thenBranch, n, context);
			Expression newElseBranch = addNTimes(elseBranch, n, context);
			
			result = IfThenElse.make(condition, newThenBranch, newElseBranch);
		}
		else {
			Expression valueToThePowerOfN = addNTimesWithUnconditionalValueAndNDistinctFromZero(valueToBeAdded, n);
			result = context.getTheory().evaluate(valueToThePowerOfN, context);
		}
		return result;
	}
}