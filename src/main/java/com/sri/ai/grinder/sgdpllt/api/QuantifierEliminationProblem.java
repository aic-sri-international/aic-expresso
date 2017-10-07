package com.sri.ai.grinder.sgdpllt.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;

public interface QuantifierEliminationProblem {

	AssociativeCommutativeGroup getGroup();

	Expression getIndex();

	SingleVariableConstraint getConstraint();

	Expression getBody();
	
	QuantifierEliminationProblem makeWithNewIndexConstraint(SingleVariableConstraint newConstraint);
}