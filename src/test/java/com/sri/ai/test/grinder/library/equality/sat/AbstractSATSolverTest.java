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
	public void testSatisfiableBasic() {
		testSatisfiable("X = Y or X != Y");
		testSatisfiable("X = a and Y = a and X = Y");
	}
	
	@Test
	public void testSatisfiableLargeDNF() {		
// TODO - expands indefinitely currently
		//testSatisfiable("X = w7 => not(X0 != Y and X0 != Z and Z != Y and (X0 = w7 and X = Y or X0 = w7 and X = Z))");
	}
	
	@Test
	public void testUnsatisfiableBasic() {
		testUnsatisfiable("X = Y and X != Y");
		testUnsatisfiable("X = a and Y = a and X != Y");
		testUnsatisfiable("and(X = a, X = Y, Y != a)");
		testUnsatisfiable("(X = person1) <=> (X != person1)");
		testUnsatisfiable("(X = person1 or X = person2) <=> (X != person1 and X != person2)");
		testUnsatisfiable("(X = person1 or X = person2 or X = person3) <=> (X != person1 and X != person2 and X != person3)");
		
		// TODO - currently not supported.
		//testUnsatisfiable("for all X: (X = person1) => (X != person1)");
		//testUnsatisfiable("for all X: (X = person1 or X = person2) => (X != person1 and X != person2)");
		//testUnsatisfiable("for all X: (X = person1 or X = person2 or X = person3) => (X != person1 and X != person2 and X != person3)");
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
