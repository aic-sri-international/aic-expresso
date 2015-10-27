package com.sri.ai.grinder.sgdpll2.core.constraint;

import static com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting.Result.CONSTRAINT_IS_CONTRADICTORY;
import static com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting.Result.LITERAL_IS_FALSE;
import static com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting.Result.LITERAL_IS_TRUE;
import static com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting.Result.LITERAL_IS_UNDEFINED;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;

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
	
	private Result     result;
	private Constraint2 constraint;
	private Expression literal;
	private Constraint2 constraintAndLiteral;
	private Constraint2 constraintAndLiteralNegation;

	/**
	 * Splits given constraint by given literal and stores the result and other information (see methods).
	 * @param constraint
	 * @param literal
	 * @param process
	 */
	public ConstraintSplitting(Constraint2 constraint, Expression literal, RewritingProcess process) {
		this(constraint, literal, null, process);
	}
	
	/**
	 * Splits given constraint by given literal under a contextual constraint and stores the result and other information (see methods).
	 * @param constraint
	 * @param literal
	 * @param process
	 */
	public ConstraintSplitting(Constraint2 constraint, Expression literal, Constraint2 contextualConstraint, RewritingProcess process) {
		this.constraint = constraint;
		this.literal = literal;
		Expression literalNegation   = constraint.getConstraintTheory().getLiteralNegation(literal, process);
		constraintAndLiteral         = constraint.conjoin(        literal, process);
		constraintAndLiteralNegation = constraint.conjoin(literalNegation, process);
		
		Constraint2 constraintAndLiteralAndContextualConstraint;
		Constraint2 constraintAndLiteralNegationAndContextualConstraint;

		if (contextualConstraint != null) {
			constraintAndLiteralAndContextualConstraint = contextualConstraint.conjoin(constraintAndLiteral, process);
			constraintAndLiteralNegationAndContextualConstraint = contextualConstraint.conjoin(constraintAndLiteralNegation, process);
		}
		else {
			constraintAndLiteralAndContextualConstraint = constraintAndLiteral;
			constraintAndLiteralNegationAndContextualConstraint = constraintAndLiteralNegation;
		}
		
		if (constraintAndLiteralAndContextualConstraint != null) {
			if (constraintAndLiteralNegationAndContextualConstraint != null) {
				result = LITERAL_IS_UNDEFINED;
			}
			else {
				result = LITERAL_IS_TRUE;
			}
		}
		else {
			if (constraintAndLiteralNegationAndContextualConstraint != null) {
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
	public Constraint2 getConstraint() {
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
	public Constraint2 getConstraintAndLiteral() {
		return constraintAndLiteral;
	}
	
	/**
	 * Return conjunction of constraint and literal negation
	 * (contextual constraint is not conjoined; it is only used to decide whether
	 * conjunction of constraint and literal is satisfiable).
	 * @return
	 */
	public Constraint2 getConstraintAndLiteralNegation() {
		return constraintAndLiteralNegation;
	}
	
	/**
	 * If the literal is either true or false, this method returns
	 * the result of conjoining the constraint with the literal or its negation, respectively.
	 * Otherwise, returns null.
	 * @return
	 */
	public Constraint2 getConstraintConjoinedWithDefinedValueOfLiteral() {
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