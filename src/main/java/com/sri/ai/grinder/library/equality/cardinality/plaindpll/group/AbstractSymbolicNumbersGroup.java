package com.sri.ai.grinder.library.equality.cardinality.plaindpll.group;

import static com.sri.ai.expresso.helper.Expressions.ZERO;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;

/**
 * An abstract implementation of groups on numbers, capable of processing symbolic numeric expressions,
 * and leaving the additive operation to be specified by abstract methods.
 * 
 * @author braz
 *
 */
public abstract class AbstractSymbolicNumbersGroup implements AssociativeCommutativeGroup {

	public AbstractSymbolicNumbersGroup() {
		super();
	}

	@Override
	public abstract Expression additiveIdentityElement();

	@Override
	public abstract Expression add(Expression value1, Expression value2, RewritingProcess process);

	@Override
	public abstract boolean isAdditiveAbsorbingElement(Expression value);

	/**
	 * Implements the addition of an element to itself n times,
	 * assuming that neither the element or n are conditional, and that n is not the constant zero.
	 * @param valueToBeAdded
	 * @param n
	 * @return
	 */
	protected abstract Expression addNTimesWithUnconditionalValueAndNAndNDistinctFromZero(Expression valueToBeAdded, Expression n);

	@Override
	public Expression addNTimes(Expression valueToBeAdded, Expression n, RewritingProcess process) {
		Expression result;
		if (n.equals(ZERO)) {
			result = additiveIdentityElement();
		}
		else if (IfThenElse.isIfThenElse(n)) { // it is important that this condition is tested before the next, because n can be conditional on splitters while valueToBeSummed can be conditioned on conditions in the unconditional solution language (such as | Everything | - 1 > 0), and we want splitters to be over non-splitter conditions
			Expression condition  = IfThenElse.condition(n);
			Expression thenBranch = IfThenElse.thenBranch(n);
			Expression elseBranch = IfThenElse.elseBranch(n);
			Expression newThenBranch = addNTimes(valueToBeAdded, thenBranch, process);
			Expression newElseBranch = addNTimes(valueToBeAdded, elseBranch, process);
			result = IfThenElse.make(condition, newThenBranch, newElseBranch, false); // do not simplify to condition so it is a DPLL solution
		}
		else if (IfThenElse.isIfThenElse(valueToBeAdded)) {
			Expression condition = IfThenElse.condition(valueToBeAdded);
			Expression thenBranch = IfThenElse.thenBranch(valueToBeAdded);
			Expression elseBranch = IfThenElse.elseBranch(valueToBeAdded);
			
			Expression newThenBranch = addNTimes(thenBranch, n, process);
			Expression newElseBranch = addNTimes(elseBranch, n, process);
			
			result = IfThenElse.make(condition, newThenBranch, newElseBranch);
		}
		else {
			result = addNTimesWithUnconditionalValueAndNAndNDistinctFromZero(valueToBeAdded, n);
		}
		return result;
	}
}