package com.sri.ai.grinder.sgdpll.core.constraint;

import static com.sri.ai.grinder.sgdpll.core.constraint.ConstraintSplitting.Result.CONSTRAINT_IS_CONTRADICTORY;
import static com.sri.ai.grinder.sgdpll.core.constraint.ConstraintSplitting.Result.LITERAL_IS_FALSE;
import static com.sri.ai.grinder.sgdpll.core.constraint.ConstraintSplitting.Result.LITERAL_IS_TRUE;
import static com.sri.ai.grinder.sgdpll.core.constraint.ConstraintSplitting.Result.LITERAL_IS_UNDEFINED;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.Constraint;

/**
 * A helper class containing information regarding the splitting a constraint by a literal in the most efficient way possible,
 * optionally under a contextual constraint.
 * <p>
 * The contextual constraint is used to decide which of the two possible fragments is satisfiable or not.
 * It is not conjoined in the constraints provided by {@link #getConstraintAndLiteral()}
 * and {@link #getConstraintAndLiteralNegation()}.
 * <p>
 * Whether satisfiability given the contextual constraint is detected or not
 * depends on the completeness of the particular implementation of the given contextual constraint.
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
public class ConstraintSplitting {

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
	
	private Result     result;
	private Constraint constraint;
	private Expression literal;
	private Constraint constraintAndLiteral;
	private Constraint constraintAndLiteralNegation;

	/**
	 * Splits given constraint by given literal and stores the result and other information (see methods).
	 * @param literal
	 * @param constraint
	 * @param contextualConstraint
	 */
	public ConstraintSplitting(Expression literal, Constraint constraint, Context contextualConstraint) {
		this.constraint = constraint;
		this.literal = literal;
		Expression literalNegation   = constraint.getConstraintTheory().getLiteralNegation(literal, contextualConstraint);
		constraintAndLiteral         = constraint.conjoin(        literal, contextualConstraint);
		constraintAndLiteralNegation = constraint.conjoin(literalNegation, contextualConstraint);
		
		if ( ! constraintAndLiteral.isContradiction()) {
			if ( ! constraintAndLiteralNegation.isContradiction()) {
				result = LITERAL_IS_UNDEFINED;
			}
			else {
				result = LITERAL_IS_TRUE;
			}
		}
		else {
			if ( ! constraintAndLiteralNegation.isContradiction()) {
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
	 * Return conjunction of constraint and literal
	 * (contextual constraint is not conjoined; it is only used to decide whether
	 * conjunction of constraint and literal is satisfiable).
	 * @return
	 */
	public Constraint getConstraintAndLiteral() {
		return constraintAndLiteral;
	}
	
	/**
	 * Return conjunction of constraint and literal negation
	 * (contextual constraint is not conjoined; it is only used to decide whether
	 * conjunction of constraint and literal is satisfiable).
	 * @return
	 */
	public Constraint getConstraintAndLiteralNegation() {
		return constraintAndLiteralNegation;
	}
	
	/**
	 * If the literal is either true or false, this method returns
	 * the result of conjoining the constraint with the literal or its negation, respectively.
	 * Otherwise, throws an Error.
	 * @return
	 */
	public Constraint getConstraintConjoinedWithDefinedValueOfLiteral() {
		switch (getResult()) {
		case LITERAL_IS_TRUE:
			return constraintAndLiteral;
		case LITERAL_IS_FALSE:
			return constraintAndLiteralNegation;
		default:
			throw new Error("Method undefined for undefined literal");
		}
	}
}