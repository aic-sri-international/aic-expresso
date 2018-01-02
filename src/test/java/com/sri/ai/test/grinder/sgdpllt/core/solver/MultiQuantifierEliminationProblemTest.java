package com.sri.ai.test.grinder.sgdpllt.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.core.solver.DefaultMultiQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.core.solver.DefaultSingleQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.group.Conjunction;
import com.sri.ai.grinder.sgdpllt.group.Disjunction;
import com.sri.ai.grinder.sgdpllt.group.Sum;

public class MultiQuantifierEliminationProblemTest {

	@Test
	public void regularCasesTest() {
		
		MultiQuantifierEliminationProblem problem;
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

		
		problem = 
				new DefaultSingleQuantifierEliminationProblem(
						new Disjunction(),
						parse("I"),
						parse("Integer"),
						parse("I > 3 and I < 10"),
						parse("true")
						);
		problemExpression = parse("there exists I in Integer : I > 3 and I < 10");
		assertEquals(problemExpression, problem.toExpression());

		
		problem = 
				new DefaultSingleQuantifierEliminationProblem(
						new Conjunction(),
						parse("I"),
						parse("Integer"),
						parse("I > 3 and I < 10"),
						parse("true")
						);
		problemExpression = parse("for all I in Integer : if I > 3 and I < 10 then true else true");
		assertEquals(problemExpression, problem.toExpression());

		
		problem = 
				new DefaultMultiQuantifierEliminationProblem(
						new Sum(),
						list(parse("I"), parse("J")),
						list(parse("Integer"), parse("Boolean")),
						parse("I > 3 and I < 10 and not J"),
						parse("I")
						);
		problemExpression = parse("sum({{ (on I in Integer, J in Boolean) I : I > 3 and I < 10 and not J }})");
		assertEquals(problemExpression, problem.toExpression());

		
		problem = 
				new DefaultMultiQuantifierEliminationProblem(
						new Disjunction(),
						list(parse("I"), parse("J")),
						list(parse("Integer"), parse("Boolean")),
						parse("I > 3 and I < 10 and not J"),
						parse("true")
						);
		problemExpression = parse("there exists I in Integer : there exists J in Boolean : I > 3 and I < 10 and not J");
		assertEquals(problemExpression, problem.toExpression());

		
		problem = 
				new DefaultMultiQuantifierEliminationProblem(
						new Conjunction(),
						list(parse("I"), parse("J")),
						list(parse("Integer"), parse("Boolean")),
						parse("I > 3 and I < 10 and not J"),
						parse("true")
						);
		problemExpression = parse("for all I in Integer : for all J in Boolean : if I > 3 and I < 10 and not J then true else true");
		assertEquals(problemExpression, problem.toExpression());
	}

	@Test
	public void cornerCasesTest() {
		
		MultiQuantifierEliminationProblem problem;
		Expression problemExpression;
		
		problem = 
				new DefaultMultiQuantifierEliminationProblem(
						new Sum(),
						list(),
						list(),
						parse("false"),
						parse("0")
						);
		problemExpression = parse("sum({{ (on ) 0 : false }})");
		assertEquals(problemExpression, problem.toExpression());

		try {
			problem = 
					new DefaultMultiQuantifierEliminationProblem(
							new Conjunction(),
							list(parse("I")),
							list(parse("Integer"), parse("Boolean")),
							parse("I > 3 and I < 10 and not J"),
							parse("true")
							);
		}
		catch (AssertionError e) {
			if (! e.getMessage().contains("DefaultMultiQuantifierEliminationProblem")) {
				fail("Should have thrown an error containing 'DefaultMultiQuantifierEliminationProblem'");
			}
		}
	}
}
