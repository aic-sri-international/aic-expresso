package com.sri.ai.test.grinder.sgdpllt.theory.base;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheoryTestingSupport.TESTING_CATEGORICAL_TYPE;

import java.util.Random;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.StepSolver;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.base.UnificationStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
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
		
		
		// Now test out individual branches
		unificationStepSolver = new UnificationStepSolver(parse("binary_prop(P, unary_prop(P))"), parse("binary_prop(unary_prop(Q), Q)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(true,  step.itDepends());
		Assert.assertEquals(parse("P = unary_prop(Q)"), step.getSplitter());
		
		StepSolver<Boolean> falseItDependsSolver = step.getStepSolverForWhenSplitterIsFalse();
		Assert.assertEquals(false,  falseItDependsSolver.step(rootContext).itDepends());
		Assert.assertEquals(false, falseItDependsSolver.step(rootContext).getValue());
		StepSolver<Boolean> trueItDependsSolver = step.getStepSolverForWhenSplitterIsTrue();
		localTestContext = rootContext.conjoin(parse("P"), rootContext);
		step = trueItDependsSolver.step(localTestContext);
		Assert.assertEquals(true,  step.itDepends());
		Assert.assertEquals(parse("P = unary_prop(Q)"), step.getSplitter());
		
		falseItDependsSolver = step.getStepSolverForWhenSplitterIsFalse();
		Assert.assertEquals(false,  falseItDependsSolver.step(rootContext).itDepends());
		Assert.assertEquals(false, falseItDependsSolver.step(rootContext).getValue());
		localTestContext = localTestContext.conjoin(parse("unary_prop(Q)"), localTestContext);
		step = trueItDependsSolver.step(localTestContext);
		Assert.assertEquals(true,  step.itDepends());
		Assert.assertEquals(parse("unary_prop(P) = Q"), step.getSplitter());
		
		falseItDependsSolver = step.getStepSolverForWhenSplitterIsFalse();
		Assert.assertEquals(false,  falseItDependsSolver.step(rootContext).itDepends());
		Assert.assertEquals(false, falseItDependsSolver.step(rootContext).getValue());
		localTestContext = localTestContext.conjoin(parse("unary_prop(P)"), localTestContext);
		step = trueItDependsSolver.step(localTestContext);
		Assert.assertEquals(true,  step.itDepends());
		Assert.assertEquals(parse("unary_prop(P) = Q"), step.getSplitter());
		
		falseItDependsSolver = step.getStepSolverForWhenSplitterIsFalse();
		Assert.assertEquals(false,  falseItDependsSolver.step(rootContext).itDepends());
		Assert.assertEquals(false, falseItDependsSolver.step(rootContext).getValue());
		localTestContext = localTestContext.conjoin(parse("Q"), localTestContext);
		step = trueItDependsSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
	}
	
	@Ignore("TODO - context implementation currently does not support these more advanced/indirect comparisons")
	@Test
	public void advancedPropositionalTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true, new PropositionalTheory());
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R", BOOLEAN_TYPE,
				"unary_prop/1", new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE), "binary_prop/2", new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE, BOOLEAN_TYPE)));
		Context rootContext = theoryTestingSupport.makeContextWithTestingInformation();
		
		UnificationStepSolver unificationStepSolver = new UnificationStepSolver(parse("binary_prop(P, unary_prop(P))"), parse("binary_prop(unary_prop(Q), Q)"));
		Context localTestContext = rootContext.conjoinWithConjunctiveClause(parse("not P and Q and not unary_prop(true) and unary_prop(false)"), rootContext);
		StepSolver.Step<Boolean> step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
	}
	
	@Test
	public void equalityTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true, new EqualityTheory(false, true));
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("X", TESTING_CATEGORICAL_TYPE, "Y", TESTING_CATEGORICAL_TYPE, "Z", TESTING_CATEGORICAL_TYPE, 
				"unary_eq/1", new FunctionType(TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE), 
				"binary_eq/2", new FunctionType(TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE)));
		Context rootContext = theoryTestingSupport.makeContextWithTestingInformation();
		
		UnificationStepSolver unificationStepSolver = new UnificationStepSolver(parse("unary_eq(X)"), parse("unary_eq(X)"));
		
		StepSolver.Step<Boolean> step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_eq(X)"), parse("unary_eq(Y)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(true, step.itDepends());		
		Assert.assertEquals(Expressions.parse("X = Y"), step.getSplitter());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).itDepends());
		Assert.assertEquals(true, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).getValue());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).itDepends());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).getValue());
		
		Context localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = a and Y = b"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_eq(X)"), parse("unary_eq(a)"));
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = a"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = b"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("binary_eq(X, unary_eq(X))"), parse("binary_eq(unary_eq(Y), Y)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(true, step.itDepends());		
		Assert.assertEquals(Expressions.parse("X = unary_eq(Y)"), step.getSplitter());
	}
	
	@Ignore("TODO - context implementation currently does not support these more advanced/indirect comparisons")
	@Test
	public void advancedEqualityTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true, new EqualityTheory(false, true));
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("X", TESTING_CATEGORICAL_TYPE, "Y", TESTING_CATEGORICAL_TYPE, "Z", TESTING_CATEGORICAL_TYPE, 
				"unary_eq/1", new FunctionType(TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE), 
				"binary_eq/2", new FunctionType(TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE)));
		Context rootContext = theoryTestingSupport.makeContextWithTestingInformation();
		
		UnificationStepSolver unificationStepSolver = new UnificationStepSolver(parse("binary_eq(X, unary_eq(X))"), parse("binary_eq(unary_eq(Y), Y)"));
		Context localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = b and Y = a and unary_eq(Y) = b and unary_eq(X) = a"), rootContext);
		StepSolver.Step<Boolean> step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = a and Y = a and unary_eq(Y) = b and unary_eq(X) = a"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
	}
}
