package com.sri.ai.grinder.sgdpll.core.constraint;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;

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
public class ExpressionConstraint extends AbstractExpressionWrapper implements Constraint {

	private static final long serialVersionUID = 1L;

	private Expression expression;
	private ConstraintTheory constraintTheory;
	
	public ExpressionConstraint(Expression expression, ConstraintTheory constraintTheory) {
		this.expression = expression;
		this.constraintTheory = constraintTheory;
	}
	
	@Override
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}

	@Override
	public Constraint conjoinWithLiteral(Expression literal, Context context) {
		Expression conjunction = And.make(expression, literal);
		ExpressionConstraint result = new ExpressionConstraint(conjunction, constraintTheory);
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		return expression;
	}

	@Override
	public Expression binding(Expression variable) {
		return null;
	}
}