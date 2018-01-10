package com.sri.ai.grinder.core.solver;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.ExpressionStepSolver;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;

/**
 * An interface for step solvers for quantified expressions
 * (the quantification being based on an associative commutative group's operation).
 * 
 * @author braz
 *
 */
@Beta
public interface SingleQuantifierEliminationStepSolver extends ExpressionStepSolver, Cloneable {

	SingleQuantifierEliminationProblem getProblem();
	
	AssociativeCommutativeGroup getGroup();
	
	SingleVariableConstraint getIndexConstraint();
	
	Expression getIndex();
	
	Expression getBody();
}