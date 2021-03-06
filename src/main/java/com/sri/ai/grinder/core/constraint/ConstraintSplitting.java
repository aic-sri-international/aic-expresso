package com.sri.ai.grinder.core.constraint;

import static com.sri.ai.grinder.core.constraint.ConstraintSplitting.Result.CONSTRAINT_IS_CONTRADICTORY;
import static com.sri.ai.grinder.core.constraint.ConstraintSplitting.Result.LITERAL_IS_FALSE;
import static com.sri.ai.grinder.core.constraint.ConstraintSplitting.Result.LITERAL_IS_TRUE;
import static com.sri.ai.grinder.core.constraint.ConstraintSplitting.Result.LITERAL_IS_UNDEFINED;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.Context;

/**
 * A helper class containing information regarding the splitting a constraint by a literal in the most efficient way possible,
 * optionally under a context.
 * <p>
 * The context is used to decide which of the two possible fragments is satisfiable or not.
 * It is not conjoined in the constraints provided by {@link #getConstraintAndLiteral()}
 * and {@link #getConstraintAndLiteralNegation()}.
 * <p>
 * Whether satisfiability given the context is detected or not
 * depends on the completeness of the particular implementation of the given context.
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

	
	////////////////////////////////////////////////////////////////////////////////////////////////////
	// FOR TESTING PURPOSES ////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////
	private static long totalConstraintSplittingTime = 0;
	private static boolean alreadyTimingConstraintSplitting = false;
	
	public static long getTotalConstraintSplittingTime() {
		return totalConstraintSplittingTime;
	}
	
	public static void resetTotalConstraintSplittingTime() {
		totalConstraintSplittingTime = 0;
	}
	// // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //
	private static int totalNumberOfConstraintSplittings = 0;
	
	public static void resetCounter() { 
		totalNumberOfConstraintSplittings = 0; 
	}
	////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	/**
	 * Splits given constraint by given literal and stores the result and other information (see methods).
	 * @param literal
	 * @param constraint
	 * @param context
	 */
	public ConstraintSplitting(Expression literal, Constraint constraint, Context context) {
		explanationBlock("Constraint splitting number ", ++totalNumberOfConstraintSplittings, " literal: ", literal, ", constraint ", constraint, code( () -> {
			
			boolean addTheTimeFromThisSplitting;
			long startTime = 0;
			if(alreadyTimingConstraintSplitting) {
				addTheTimeFromThisSplitting = false;
			}
			else {
				addTheTimeFromThisSplitting = true;
				startTime = System.nanoTime();
				alreadyTimingConstraintSplitting = true;
			}

			
			
			this.constraint = constraint;
			this.literal = literal;
			Expression literalNegation   = constraint.getTheory().getLiteralNegation(literal, context);
			constraintAndLiteral         = constraint.conjoin(        literal, context);
			constraintAndLiteralNegation = constraint.conjoin(literalNegation, context);

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
			
			
			
			if(addTheTimeFromThisSplitting) {
				long constraintSplittingTime = System.nanoTime() - startTime;
				totalConstraintSplittingTime += constraintSplittingTime;
				alreadyTimingConstraintSplitting = false;
			}

			return result; }), "Result is ", RESULT);
	}
	
	/** The result of the splitting, which is one of the value of type {@link Result}. */
	public Result getResult() {
		return result;
	}
	
	/**
	 * Equivalent to <code>return getResult().equals(LITERAL_IS_UNDEFINED);</code>.
	 * @return
	 */
	public boolean isUndefined() {
		return getResult().equals(LITERAL_IS_UNDEFINED);
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
	 * (context is not conjoined; it is only used to decide whether
	 * conjunction of constraint and literal is satisfiable).
	 * @return
	 */
	public Constraint getConstraintAndLiteral() {
		return constraintAndLiteral;
	}
	
	/**
	 * Return conjunction of constraint and literal negation
	 * (context is not conjoined; it is only used to decide whether
	 * conjunction of constraint and literal is satisfiable).
	 * @return
	 */
	public Constraint getConstraintAndLiteralNegation() {
		return constraintAndLiteralNegation;
	}
	
	/**
	 * Returns {@link #getConstraintAndLiteral()} if splitter value is true, and {@link #getConstraintAndLiteralNegation()} otherwise.
	 */
	public Constraint getConstraintAndLiteralEqualTo(boolean splitterValue) {
		if (splitterValue) {
			return constraintAndLiteral;
		}
		else {
			return constraintAndLiteralNegation;
		}
	}
}