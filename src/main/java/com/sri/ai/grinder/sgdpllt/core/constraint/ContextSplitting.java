package com.sri.ai.grinder.sgdpllt.core.constraint;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;

/**
 * A specialization of {@link ConstraintSplitting} for {@link Context}.
 * 
 * @author braz
 *
 */
@Beta
public class ContextSplitting extends ConstraintSplitting {

	/**
	 * Splits given constraint by given literal and stores the result and other information (see methods).
	 * @param literal
	 * @param context
	 */
	public ContextSplitting(Expression literal, Context context) {
		super(literal, context, context);
		// the above may seem odd, but we are simply splitting the context.
		// Because we need a Context for that to happen (because of type information, etc),
		// we use the context itself.
		// The constraint information inside the second context will be redudant.
	}
	
	@Override
	public Context getConstraint() {
		return (Context) super.getConstraint();
	}

	/**
	 * Same thing as {@link #getConstraint()}, but with a specialized name.
	 * @return
	 */
	public Context getContext() {
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
	public Context getContextAndLiteral() {
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
	public Context getContextAndLiteralNegation() {
		return getConstraintAndLiteralNegation();
	}
}