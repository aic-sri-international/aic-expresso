package com.sri.ai.grinder.sgdpll.core.solver;

import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll.group.AssociativeCommutativeGroup;

/**
 * An interface for step solvers for quantified expressions
 * (the quantification being based on an associative commutative group's operation).
 * 
 * @author braz
 *
 */
@Beta
public interface QuantifierEliminationStepSolver extends ContextDependentExpressionProblemStepSolver, Cloneable {

	AssociativeCommutativeGroup getGroup();
	
	SingleVariableConstraint getIndexConstraint();
	
	/**
	 * Convenience method for <code>getIndexConstraint().getConstraintTheory()</code>.
	 * @return
	 */
	ConstraintTheory getConstraintTheory();
	
	Expression getIndex();
	
	/**
	 * Generates a random expression from the class of expressions that can be present in the body of the
	 * quantification once literals have been conditioned away.
	 * <p>
	 * For example, if the quantifier eliminator can deal with conditional polynomials in the body
	 * of quantifier expressions, then this method should return unconditional polynomials.
	 * <p>
	 * This is useful for test problem generators, which will typically generate conditional expressions
	 * of the same type by using constraint theories to generate the literals and put everything together.
	 * @param random TODO
	 * @return
	 */
	Expression makeRandomUnconditionalBody(Random random);
}