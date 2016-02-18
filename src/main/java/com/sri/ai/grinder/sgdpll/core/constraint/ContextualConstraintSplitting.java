package com.sri.ai.grinder.sgdpll.core.constraint;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

/**
 * A specialization of {@link ConstraintSplitting} for {@link Context}.
 * 
 * @author braz
 *
 */
@Beta
public class ContextualConstraintSplitting extends ConstraintSplitting {

	/**
	 * Splits given constraint by given literal and stores the result and other information (see methods).
	 * @param literal
	 * @param contextualConstraint
	 * @param context
	 */
	public ContextualConstraintSplitting(Expression literal, Context contextualConstraint, Context context) {
		super(literal, contextualConstraint, context);
	}
	
	@Override
	public Context getConstraint() {
		return (Context) super.getConstraint();
	}

	/**
	 * Same thing as {@link #getConstraint()}, but with a specialized name.
	 * @return
	 */
	public Context getContextualConstraint() {
		return getConstraint();
	}

	@Override
	public Context getConstraintAndLiteral() {
		return (Context) super.getConstraintAndLiteral();
	}
	
	/**
	 * Same thing as {@link #getConstraintAndLiteral()}, but with a specialized name.
	 * @return
	 */
	public Context getContextualConstraintAndLiteral() {
		return getConstraintAndLiteral();
	}

	@Override
	public Context getConstraintAndLiteralNegation() {
		return (Context) super.getConstraintAndLiteralNegation();
	}
	
	/**
	 * Same thing as {@link #getConstraintAndLiteralNegation()}, but with a specialized name.
	 * @return
	 */
	public Context getContextualConstraintAndLiteralNegation() {
		return getConstraintAndLiteralNegation();
	}

	@Override
	public Context getConstraintConjoinedWithDefinedValueOfLiteral() {
		return (Context) super.getConstraintConjoinedWithDefinedValueOfLiteral();
	}

	/**
	 * Same thing as {@link #getConstraintConjoinedWithDefinedValueOfLiteral()}, but with a specialized name.
	 * @return
	 */
	public Context getContextualConstraintConjoinedWithDefinedValueOfLiteral() {
		return getConstraintConjoinedWithDefinedValueOfLiteral();
	}
}