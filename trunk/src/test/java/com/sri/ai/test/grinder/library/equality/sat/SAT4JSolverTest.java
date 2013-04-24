package com.sri.ai.test.grinder.library.equality.sat;

import com.sri.ai.grinder.library.equality.sat.SAT4JSolver;
import com.sri.ai.grinder.library.equality.sat.SATSolver;

public class SAT4JSolverTest extends AbstractSATSolverTest {

	@Override
	public SATSolver newSATSolver() {
		return new SAT4JSolver();
	}
}
