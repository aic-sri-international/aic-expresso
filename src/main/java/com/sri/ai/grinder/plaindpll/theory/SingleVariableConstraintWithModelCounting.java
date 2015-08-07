package com.sri.ai.grinder.plaindpll.theory;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.SingleVariableConstraint;

/**
 * 
 * @author braz
 *
 */
public interface SingleVariableConstraintWithModelCounting extends SingleVariableConstraint {
	
	SingleVariableEqualityConstraintWithModelCounting simplifyGiven(Expression externalLiteral, RewritingProcess process);
	
	SingleVariableConstraintWithModelCounting conjoin(Expression literal, RewritingProcess process);

	/**
	 * Picks a splitter whose value is necessary in order to determine the model count of the constraint.
	 * @param process the current rewriting process
	 * @return the splitter
	 */
	Expression pickSplitter(RewritingProcess process);
	
	/**
	 * The number of assignments to the variable that satisfies the constraint.
	 * @param process the current rewriting process
	 * @return an expression representing the model count
	 */
	Expression modelCount(RewritingProcess process);
}