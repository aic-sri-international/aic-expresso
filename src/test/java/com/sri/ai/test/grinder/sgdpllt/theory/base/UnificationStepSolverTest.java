package com.sri.ai.test.grinder.sgdpllt.theory.base;

import java.util.Random;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.StepSolver;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.base.UnificationStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;

public class UnificationStepSolverTest {

	private Random seededRandom = new Random(1);
	
	@Test
	public void propositionalTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true, new PropositionalTheory());
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
		
		UnificationStepSolver unificationStepSolver = new UnificationStepSolver(Expressions.parse("unary_prop(P)"), Expressions.parse("unary_prop(P)"));
		StepSolver.Step<Boolean> step = unificationStepSolver.step(context);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(Expressions.parse("unary_prop(P)"), Expressions.parse("unary_prop(Q)"));
		step = unificationStepSolver.step(context);
		Assert.assertEquals(true, step.itDepends());		
		Assert.assertEquals(Expressions.parse("P = Q"), step.getSplitter());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsTrue().step(context).itDepends());
		Assert.assertEquals(true, step.getStepSolverForWhenSplitterIsTrue().step(context).getValue());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(context).itDepends());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(context).getValue());
		
		context = context.conjoinWithConjunctiveClause(Expressions.parse("P and not Q"), context);
		step = unificationStepSolver.step(context);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
	}
}
