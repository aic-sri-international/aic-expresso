package com.sri.ai.grinder.sgdpllt.core.solver;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;

abstract public class AbstractMultiQuantifierEliminationProblem implements MultiQuantifierEliminationProblem {

	public final AssociativeCommutativeGroup group;
	public final Expression constraint;
	public final Expression body;

	public AbstractMultiQuantifierEliminationProblem(AssociativeCommutativeGroup group, Expression constraint, Expression body) {
		this.group = group;
		this.constraint = constraint;
		this.body = body;
	}

	public AbstractMultiQuantifierEliminationProblem(AssociativeCommutativeGroup group, Expression body) {
		this.group = group;
		this.constraint = Expressions.TRUE;
		this.body = body;
	}

	@Override
	public AssociativeCommutativeGroup getGroup() {
		return group;
	}

	@Override
	public Expression getConstraint() {
		return constraint;
	}

	@Override
	public Expression getBody() {
		return body;
	}

	@Override
	public String toString() {
		return "Quantifier elimination problem on " + toExpression();
	}

	@Override
	public Expression toExpression() {
		Expression result = getGroup().makeProblemExpression(this);
		return result;
	}
}