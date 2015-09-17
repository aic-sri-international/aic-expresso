package com.sri.ai.grinder.sgdpll2.core.constraint;

import static com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting.Result.CONSTRAINT_IS_CONTRADICTORY;
import static com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting.Result.LITERAL_IS_FALSE;
import static com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting.Result.LITERAL_IS_TRUE;
import static com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting.Result.LITERAL_IS_UNDEFINED;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint;

/**
 * A helper class containing information regarding the splitting a constraint by a literal in the most efficient way possible.
 * <p>
 * Four results are possible:
 * <ul>
 * <li> {@link Result#LITERAL_IS_TRUE}: literal is implied by constraint 
 * <li> {@link Result#LITERAL_IS_FALSE}: literal negation is implied by constraint 
 * <li> {@link Result#LITERAL_IS_UNDEFINED}: neither literal or its negation is implied by constraint 
 * <li> {@link Result#CONSTRAINT_IS_CONTRADICTORY}: both conjoining the literal and conjoining the literal negation with constraint generate contradictions,
 * and therefore the constraint is contradictory itself. 
 * </ul>
 * 
 * @author braz
 *
 */
@Beta
public class ConstraintSplitting  {

	/**
	 * The four possible result of splitting a constraint by a literal. See {@link ConstraintSplitting}.
	 * @author braz
	 *
	 */
	public static enum Result {
		LITERAL_IS_TRUE,
		LITERAL_IS_FALSE,
		LITERAL_IS_UNDEFINED,
		CONSTRAINT_IS_CONTRADICTORY
	}
	
	private Result result;
	private Constraint constraint;
	private Expression literal;
	private Constraint constraintAndLiteral;
	private Constraint constraintAndLiteralNegation;
	
	/**
	 * Splits given constraint by given literal and stores the result and other information (see methods).
	 * @param constraint
	 * @param literal
	 * @param process
	 */
	public ConstraintSplitting(Constraint constraint, Expression literal, RewritingProcess process) {
		this.constraint = constraint;
		this.literal = literal;
		Expression literalNegation   = constraint.getConstraintTheory().getLiteralNegation(literal);
		constraintAndLiteral         = constraint.conjoin(        literal, process);
		constraintAndLiteralNegation = constraint.conjoin(literalNegation, process);
		
		if (constraintAndLiteral != null) {
			if (constraintAndLiteralNegation != null) {
				result = LITERAL_IS_UNDEFINED;
			}
			else {
				result = LITERAL_IS_TRUE;
			}
		}
		else {
			if (constraintAndLiteralNegation != null) {
				result = LITERAL_IS_FALSE;
			}
			else {
				result = CONSTRAINT_IS_CONTRADICTORY;
			}
		}
	}
	
	/** The result of the splitting. */
	public Result getResult() {
		return result;
	}
	
	/** The original constraint being split. */
	public Constraint getConstraint() {
		return constraint;
	}
	
	/** The original literal. */
	public Expression getLiteral() {
		return literal;
	}
	
	/**
	 * If the literal is either true or false, this method returns
	 * the result of conjoining the constraint with the literal or its negation, respectively.
	 * Otherwise, returns null.
	 * @return
	 */
	public Constraint getConstraintConjoinedWithDefinedValueOfLiteral() {
		switch (getResult()) {
		case LITERAL_IS_TRUE:
			return constraintAndLiteral;
		case LITERAL_IS_FALSE:
			return constraintAndLiteralNegation;
		default:
			return null;
		}
	}
}