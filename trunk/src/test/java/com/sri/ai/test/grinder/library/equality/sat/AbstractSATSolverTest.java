package com.sri.ai.test.grinder.library.equality.sat;

import org.junit.Assert;
import org.junit.Test;

import com.google.common.base.Stopwatch;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.equality.sat.SATSolver;
import com.sri.ai.test.grinder.AbstractGrinderTest;

public abstract class AbstractSATSolverTest extends AbstractGrinderTest {
	
	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}
	
	public abstract SATSolver newSATSolver();
	
	@Test
	public void testBasicSatisfiableCases() {
		testSatisfiable("X = Y or X != Y");
		testSatisfiable("X = a and Y = a and X = Y");
	}
	
	@Test
	public void testBasicUnsatisfiableCases() {
		testUnsatisfiable("X = Y and X != Y");
		testUnsatisfiable("X = a and Y = a and X != Y");
	}

	//
	// PRIVATE
	//
	private void testSatisfiable(String strFormula) {
		test(strFormula, true);
	}
	
	private void testUnsatisfiable(String strFormula) {
		test(strFormula, false);
	}
	
	private void test(String strFormula, boolean expectedSatisfiable) {
		RewritingProcess process = newProcess();
		SATSolver        solver  = newSATSolver();  
		Expression       formula = parse(strFormula);
		
		System.out.println("Is : "+formula);
		Stopwatch stopwatch = new Stopwatch().start();
		boolean satisfiable = solver.isSatisfiable(formula, process);
		long evaluationTime = stopwatch.elapsedMillis();
		System.out.println("- Satisfiable? ->");
		System.out.println(""+satisfiable+", solver time: " + evaluationTime + " ms.");
		Assert.assertEquals(expectedSatisfiable, satisfiable);
	}
	
	private RewritingProcess newProcess() {
		return DirectCardinalityComputationFactory.newCardinalityProcess(Expressions.TRUE);
	}
}
