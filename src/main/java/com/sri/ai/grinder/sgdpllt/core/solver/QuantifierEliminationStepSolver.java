package com.sri.ai.grinder.sgdpllt.core.solver;

import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.ExpressionFormulaSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;

/**
 * An interface for step solvers for quantified expressions
 * (the quantification being based on an associative commutative group's operation).
 * 
 * @author braz
 *
 */
@Beta
public interface QuantifierEliminationStepSolver extends ExpressionFormulaSplitterStepSolver, Cloneable {

	AssociativeCommutativeGroup getGroup();
	
	SingleVariableConstraint getIndexConstraint();
	
	/**
	 * Convenience method for <code>getIndexConstraint().getTheory()</code>.
	 * @return
	 */
	Theory getTheory();
	
	Expression getIndex();
	
	Expression getBody();
	
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