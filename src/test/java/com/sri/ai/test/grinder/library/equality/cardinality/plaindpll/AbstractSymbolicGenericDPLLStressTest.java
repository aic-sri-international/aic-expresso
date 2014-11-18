package com.sri.ai.test.grinder.library.equality.cardinality.plaindpll;

import java.util.Iterator;
import java.util.List;

import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FirstNIterator;

/**
 * A generic stress test for DPLL algorithms.
 * It is mean for creating stress tests that will not change with time and serve as benchmarks.
 * If the need for new tests arise, they can be extended from this one and its parameter methods
 * can be overridden.
 * 
 * @author braz
 */
abstract public class AbstractSymbolicGenericDPLLStressTest {

	protected abstract Rewriter makeRewriter();

	protected abstract Iterator<Expression> makeProblemsIterator(int size, int minimumNumberOfIndices);

	// These parameters are in method form so variants can be declared as extending classes.
	public boolean useFreeVariables()            { return true;  }
	public int     getRandomSeedForProblems()    { return 1;     }
	public int     getSizeOfDataset()            { return 50;    }
	public int     getMinimumSize()              { return 2;     }
	public int     getMaximumSize()              { return 7;     }
	public int     getNumberOfRunsForAveraging() { return 10;    }
	public boolean isConsoleOutput()             { return true;  }

	@Before
	public void ignoreTest() {
		Assume.assumeFalse("Stress Tests Ignored.", Boolean.getBoolean("ignore.stress.tests"));
	}

	@Test
	public void test() {
		
		GrinderUtil.setMinimumOutputForProfiling();
	
		Rewriter rewriter = makeRewriter();
		
		long totalStart = System.currentTimeMillis();
	
		for (int size = getMinimumSize(); size <= getMaximumSize(); size++) { // "greater or *equal* than" because {@link AbstractRandomDPLLProblemGenerator}'s size is 1-based
			int minimumNumberOfIndices = useFreeVariables()? size/2 : size;
			Iterator<Expression> problemsIterator = makeProblemsIterator(size, minimumNumberOfIndices);
			List<Expression> problems = Util.listFrom(new FirstNIterator<Expression>(getSizeOfDataset(), problemsIterator));
			int problemIndex = 1;
			for (Expression problem : problems) {
				if (isConsoleOutput()) {
					System.out.println(problem);
				}
				Expression solution = null;
				long start = System.currentTimeMillis();
				for (int i = 0; i != getNumberOfRunsForAveraging(); i++) {
					solution = rewriter.rewrite(problem);
				}
				final long time = System.currentTimeMillis() - start;
				if (isConsoleOutput()) {
					System.out.println("->\n" + solution + "\n(Size " + size + ", " + problemIndex + "-th problem, " + ((double)time)/getNumberOfRunsForAveraging() + " ms, " + rewriter.getName() + ")\n");
				}
				problemIndex++;
			}
		}
		
		long total = System.currentTimeMillis() - totalStart;
		
		System.out.println("Total time: " + total + " ms");
	}
}