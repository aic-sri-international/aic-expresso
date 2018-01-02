package com.sri.ai.test.grinder.sgdpllt.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.core.solver.DefaultSingleQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.group.Sum;

public class MultiQuantifierEliminationProblemTest {

	@Test
	public void test() {
		
		DefaultSingleQuantifierEliminationProblem problem;
		Expression problemExpression;
		
		problem = 
				new DefaultSingleQuantifierEliminationProblem(
						new Sum(),
						parse("I"),
						parse("Integer"),
						parse("I > 3 and I < 10"),
						parse("I")
						);
		problemExpression = parse("sum({{ (on I in Integer) I : I > 3 and I < 10 }})");
		assertEquals(problemExpression, problem.toExpression());
	}

}
