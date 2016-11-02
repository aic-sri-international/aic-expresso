package com.sri.ai.test.grinder.sgdpllt.theory.base;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.util.Util.map;

import java.util.Random;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
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
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R", BOOLEAN_TYPE,
				"unary_prop/1", new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE), "binary_prop/2", new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE, BOOLEAN_TYPE)));
		Context rootContext = theoryTestingSupport.makeContextWithTestingInformation();
		
		UnificationStepSolver unificationStepSolver = new UnificationStepSolver(parse("unary_prop(P)"), parse("unary_prop(P)"));
		StepSolver.Step<Boolean> step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_prop(P)"), parse("unary_prop(Q)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(true, step.itDepends());		
		Assert.assertEquals(Expressions.parse("P = Q"), step.getSplitter());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).itDepends());
		Assert.assertEquals(true, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).getValue());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).itDepends());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).getValue());
		
		Context localTestContext = rootContext.conjoinWithConjunctiveClause(parse("P and not Q"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_prop(P)"), parse("unary_prop(true)"));
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("P"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("not P"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("binary_prop(P, unary_prop(P))"), parse("binary_prop(unary_prop(Q), Q)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(true, step.itDepends());		
		Assert.assertEquals(Expressions.parse("P = unary_prop(Q)"), step.getSplitter());
		
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("not P and Q and not unary_prop(Q) and unary_prop(P)"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("P and Q and not unary_prop(Q) and unary_prop(P)"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());

		// TODO - Look over with Rodrigo why this doesn't work.
//		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("not P and Q and not unary_prop(true) and unary_prop(false)"), rootContext);
//		step = unificationStepSolver.step(localTestContext);
//		Assert.assertEquals(false,  step.itDepends());
//		Assert.assertEquals(true, step.getValue());
	}
}
