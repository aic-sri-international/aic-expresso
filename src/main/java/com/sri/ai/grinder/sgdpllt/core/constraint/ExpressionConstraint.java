package com.sri.ai.grinder.sgdpllt.core.constraint;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Constraint;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.boole.And;

/**
 * An {@link Constraint} based on a given expression.
 * Unlike other implementations of {@link Constraint}, this does not keep
 * a special internal efficient representation,
 * but it allows us to use an arbitrary expression as a Constraint
 * (this, however, depends on the other implementations to 
 * 
 * @author braz
 *
 */
public class ExpressionConstraint extends AbstractConstraint {

	private static final long serialVersionUID = 1L;

	private Expression expression;
	
	public ExpressionConstraint(Expression expression, Theory theory) {
		super(theory);
		this.expression = expression;
	}
	
	@Override
	public Constraint conjoinWithLiteral(Expression literal, Context context) {
		Expression conjunction = And.make(expression, literal);
		ExpressionConstraint result = new ExpressionConstraint(conjunction, getTheory());
		return result;
	}

	@Override
	protected Expression computeInnerExpressionIfNotContradiction() {
		return expression;
	}

	@Override
	public Expression binding(Expression variable) {
		return null;
	}
}