package com.sri.ai.test.grinder.sgdpllt.theory.base;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheoryTestingSupport.TESTING_INTEGER_INTERVAL_TYPE;
import static com.sri.ai.grinder.theory.equality.EqualityTheoryTestingSupport.TESTING_CATEGORICAL_TYPE;
import static com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheoryTestingSupport.TESTING_REAL_INTERVAL_TYPE;

import java.util.Random;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.StepSolver;
import com.sri.ai.grinder.tester.TheoryTestingSupport;
import com.sri.ai.grinder.theory.base.UnificationStepSolver;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;

public class UnificationStepSolverTest {

	private Random seededRandom = new Random(1);
	
	@Test
	public void propositionalTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, new PropositionalTheory());
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

		// Ignore: PropositionalTheory will only deal with symbol variables for now
//		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("not P and Q and not unary_prop(Q) and unary_prop(P)"), rootContext);
//		step = unificationStepSolver.step(localTestContext);
//		Assert.assertEquals(false,  step.itDepends());
//		Assert.assertEquals(true, step.getValue());
//		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("P and Q and not unary_prop(Q) and unary_prop(P)"), rootContext);
//		step = unificationStepSolver.step(localTestContext);
//		Assert.assertEquals(false,  step.itDepends());
//		Assert.assertEquals(false, step.getValue());
		
		
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
		// Ignore: PropositionalTheory will only deal with symbol variables for now
//		localTestContext = localTestContext.conjoin(parse("unary_prop(Q)"), localTestContext);
//		step = trueItDependsSolver.step(localTestContext);
//		Assert.assertEquals(true,  step.itDepends());
//		Assert.assertEquals(parse("unary_prop(P) = Q"), step.getSplitter());
		
		falseItDependsSolver = step.getStepSolverForWhenSplitterIsFalse();
		Assert.assertEquals(false,  falseItDependsSolver.step(rootContext).itDepends());
		Assert.assertEquals(false, falseItDependsSolver.step(rootContext).getValue());
		// Ignore: PropositionalTheory will only deal with symbol variables for now
//		localTestContext = localTestContext.conjoin(parse("unary_prop(P)"), localTestContext);
//		step = trueItDependsSolver.step(localTestContext);
//		Assert.assertEquals(true,  step.itDepends());
//		Assert.assertEquals(parse("unary_prop(P) = Q"), step.getSplitter());
		
		falseItDependsSolver = step.getStepSolverForWhenSplitterIsFalse();
		Assert.assertEquals(false,  falseItDependsSolver.step(rootContext).itDepends());
		Assert.assertEquals(false, falseItDependsSolver.step(rootContext).getValue());
		// Ignore: PropositionalTheory will only deal with symbol variables for now
//		localTestContext = localTestContext.conjoin(parse("Q"), localTestContext);
//		step = trueItDependsSolver.step(localTestContext);
//		Assert.assertEquals(false,  step.itDepends());
//		Assert.assertEquals(true, step.getValue());
	}
	
	@Ignore("TODO - context implementation currently does not support these more advanced/indirect comparisons")
	@Test
	public void advancedPropositionalTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, new PropositionalTheory());
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
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, new EqualityTheory(true, true));
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("X", TESTING_CATEGORICAL_TYPE, "Y", TESTING_CATEGORICAL_TYPE, "Z", TESTING_CATEGORICAL_TYPE, 
				"unary_eq", new FunctionType(TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE), 
				"binary_eq", new FunctionType(TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE)));
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
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, new EqualityTheory(false, true));
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
		
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = b and Y = a and unary_eq(a) = b and unary_eq(b) = a"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
	}
	
	@Test
	public void differenceArithmeticTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, new DifferenceArithmeticTheory(true, true));
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("I", TESTING_INTEGER_INTERVAL_TYPE, "J", TESTING_INTEGER_INTERVAL_TYPE, "K", TESTING_INTEGER_INTERVAL_TYPE, 
				"unary_dar", new FunctionType(TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE), 
				"binary_dar", new FunctionType(TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE)));
		Context rootContext = theoryTestingSupport.makeContextWithTestingInformation();
		
		UnificationStepSolver unificationStepSolver = new UnificationStepSolver(parse("unary_dar(I)"), parse("unary_dar(I)"));
		StepSolver.Step<Boolean> step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_dar(I)"), parse("unary_dar(J)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(true, step.itDepends());		
		Assert.assertEquals(Expressions.parse("I = J"), step.getSplitter());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).itDepends());
		Assert.assertEquals(true, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).getValue());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).itDepends());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).getValue());
		
		Context localTestContext = rootContext.conjoinWithConjunctiveClause(parse("I = 0 and J = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_dar(I)"), parse("unary_dar(0)"));
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("I = 0"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("I = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
	}
	
	@Ignore("TODO - context implementation currently does not support these more advanced/indirect comparisons")
	@Test
	public void advancedDifferenceArithmeticTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, new DifferenceArithmeticTheory(true, true));
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("I", TESTING_INTEGER_INTERVAL_TYPE, "J", TESTING_INTEGER_INTERVAL_TYPE, "K", TESTING_INTEGER_INTERVAL_TYPE, 
				"unary_dar/1", new FunctionType(TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE), 
				"binary_dar/2", new FunctionType(TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE)));
		Context rootContext = theoryTestingSupport.makeContextWithTestingInformation();
		
		UnificationStepSolver unificationStepSolver =new UnificationStepSolver(parse("binary_dar(I, unary_dar(I))"), parse("binary_dar(unary_dar(J), J)"));
		
		Context localTestContext = rootContext.conjoinWithConjunctiveClause(parse("I = 0 and J = 1 and unary_dar(J) = 0 and unary_dar(I) = 1"), rootContext);
		StepSolver.Step<Boolean> step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("I = 1 and J = 1 and unary_dar(J) = 0 and unary_dar(I) = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("I = 0 and J = 1 and unary_dar(1) = 0 and unary_dar(0) = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
	}
	
	@Test
	public void linearRealArithmeticTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, new LinearRealArithmeticTheory(true, true));
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("X", TESTING_REAL_INTERVAL_TYPE, "Y", TESTING_REAL_INTERVAL_TYPE, "Z", TESTING_REAL_INTERVAL_TYPE, 
				"unary_lra", new FunctionType(TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE), 
				"binary_lra", new FunctionType(TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE)));
		Context rootContext = theoryTestingSupport.makeContextWithTestingInformation();
		
		UnificationStepSolver unificationStepSolver = new UnificationStepSolver(parse("unary_lra(X)"), parse("unary_lra(X)"));
		StepSolver.Step<Boolean> step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_lra(X)"), parse("unary_lra(Y)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(true, step.itDepends());		
		Assert.assertEquals(Expressions.parse("X = Y"), step.getSplitter());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).itDepends());
		Assert.assertEquals(true, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).getValue());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).itDepends());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).getValue());
		
		Context localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = 0 and Y = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_lra(X)"), parse("unary_lra(0)"));
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = 0"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
	}
	
	@Ignore("TODO - context implementation currently does not support these more advanced/indirect comparisons")
	@Test
	public void advancedLinearRealArithmeticTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, new LinearRealArithmeticTheory(true, true));
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("X", TESTING_REAL_INTERVAL_TYPE, "Y", TESTING_REAL_INTERVAL_TYPE, "Z", TESTING_REAL_INTERVAL_TYPE, 
				"unary_lra/1", new FunctionType(TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE), 
				"binary_lra/2", new FunctionType(TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE)));
		Context rootContext = theoryTestingSupport.makeContextWithTestingInformation();
		
		UnificationStepSolver unificationStepSolver =new UnificationStepSolver(parse("binary_lra(X, unary_lra(X))"), parse("binary_lra(unary_lra(Y), Y)"));
		
		Context localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = 0 and Y = 1 and unary_lra(Y) = 0 and unary_lra(X) = 1"), rootContext);
		StepSolver.Step<Boolean> step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = 1 and Y = 1 and unary_lra(Y) = 0 and unary_lra(X) = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = 0 and Y = 1 and unary_lra(1) = 0 and unary_lra(0) = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
	}
	
	@Test
	public void compoundTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, 
														new CompoundTheory(
															new EqualityTheory(false, true),
															new DifferenceArithmeticTheory(false, true),
															new LinearRealArithmeticTheory(false, true),
															new PropositionalTheory()));
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map(
				"P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R", BOOLEAN_TYPE,
				"unary_prop", new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE), 
				"binary_prop", new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE, BOOLEAN_TYPE),
				"S", TESTING_CATEGORICAL_TYPE, "T", TESTING_CATEGORICAL_TYPE, "U", TESTING_CATEGORICAL_TYPE, 
				"unary_eq", new FunctionType(TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE), 
				"binary_eq", new FunctionType(TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE),
				"I", TESTING_INTEGER_INTERVAL_TYPE, "J", TESTING_INTEGER_INTERVAL_TYPE, "K", TESTING_INTEGER_INTERVAL_TYPE, 
				"unary_dar", new FunctionType(TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE), 
				"binary_dar", new FunctionType(TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE),
				"X", TESTING_REAL_INTERVAL_TYPE, "Y", TESTING_REAL_INTERVAL_TYPE, "Z", TESTING_REAL_INTERVAL_TYPE, 
				"unary_lra", new FunctionType(TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE), 
				"binary_lra", new FunctionType(TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE)
				));
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
		
		//
		//
		unificationStepSolver = new UnificationStepSolver(parse("unary_eq(S)"), parse("unary_eq(S)"));
		
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_eq(S)"), parse("unary_eq(T)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(true, step.itDepends());		
		Assert.assertEquals(Expressions.parse("S = T"), step.getSplitter());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).itDepends());
		Assert.assertEquals(true, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).getValue());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).itDepends());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).getValue());
		
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("S = a and T = b"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_eq(S)"), parse("unary_eq(a)"));
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("S = a"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("S = b"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		//
		//
		unificationStepSolver = new UnificationStepSolver(parse("unary_dar(I)"), parse("unary_dar(I)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_dar(I)"), parse("unary_dar(J)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(true, step.itDepends());		
		Assert.assertEquals(Expressions.parse("I = J"), step.getSplitter());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).itDepends());
		Assert.assertEquals(true, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).getValue());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).itDepends());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).getValue());
		
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("I = 0 and J = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_dar(I)"), parse("unary_dar(0)"));
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("I = 0"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("I = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		//
		//
		unificationStepSolver = new UnificationStepSolver(parse("unary_lra(X)"), parse("unary_lra(X)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_lra(X)"), parse("unary_lra(Y)"));
		step = unificationStepSolver.step(rootContext);
		Assert.assertEquals(true, step.itDepends());		
		Assert.assertEquals(Expressions.parse("X = Y"), step.getSplitter());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).itDepends());
		Assert.assertEquals(true, step.getStepSolverForWhenSplitterIsTrue().step(rootContext).getValue());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).itDepends());
		Assert.assertEquals(false, step.getStepSolverForWhenSplitterIsFalse().step(rootContext).getValue());
		
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = 0 and Y = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
		
		unificationStepSolver = new UnificationStepSolver(parse("unary_lra(X)"), parse("unary_lra(0)"));
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = 0"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("X = 1"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
	}
	
	@Ignore("TODO - context implementation currently does not support these more advanced/indirect comparisons")
	@Test
	public void advancedCompositeTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, 
				new CompoundTheory(
					new EqualityTheory(false, true),
					new DifferenceArithmeticTheory(false, true),
					new LinearRealArithmeticTheory(false, true),
					new PropositionalTheory()));
		// NOTE: passing explicit FunctionTypes will prevent the general variables' argument types being randomly changed.
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map(
			"P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R", BOOLEAN_TYPE,
			"unary_prop/1", new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE), 
			"binary_prop/2", new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE, BOOLEAN_TYPE),
			"S", TESTING_CATEGORICAL_TYPE, "T", TESTING_CATEGORICAL_TYPE, "U", TESTING_CATEGORICAL_TYPE, 
			"unary_eq/1", new FunctionType(TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE), 
			"binary_eq/2", new FunctionType(TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE, TESTING_CATEGORICAL_TYPE),
			"I", TESTING_INTEGER_INTERVAL_TYPE, "J", TESTING_INTEGER_INTERVAL_TYPE, "K", TESTING_INTEGER_INTERVAL_TYPE, 
			"unary_dar/1", new FunctionType(TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE), 
			"binary_dar/2", new FunctionType(TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE, TESTING_INTEGER_INTERVAL_TYPE),
			"X", TESTING_REAL_INTERVAL_TYPE, "Y", TESTING_REAL_INTERVAL_TYPE, "Z", TESTING_REAL_INTERVAL_TYPE, 
			"unary_lra/1", new FunctionType(TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE), 
			"binary_lra/2", new FunctionType(TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE, TESTING_REAL_INTERVAL_TYPE)
		));
		Context rootContext = theoryTestingSupport.makeContextWithTestingInformation();
		
		UnificationStepSolver unificationStepSolver = new UnificationStepSolver(parse("binary_prop(P, unary_prop(P))"), parse("binary_prop(unary_prop(Q), Q)"));
		
		Context localTestContext = rootContext.conjoinWithConjunctiveClause(parse("not P and Q and not unary_prop(Q) and unary_prop(P)"), rootContext);
		StepSolver.Step<Boolean> step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(true, step.getValue());
		localTestContext = rootContext.conjoinWithConjunctiveClause(parse("P and Q and not unary_prop(Q) and unary_prop(P)"), rootContext);
		step = unificationStepSolver.step(localTestContext);
		Assert.assertEquals(false,  step.itDepends());
		Assert.assertEquals(false, step.getValue());
	}
}
