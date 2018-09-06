package com.sri.ai.grinder.core.solver;

import static com.sri.ai.grinder.core.solver.DefaultMultiQuantifierEliminationProblem.makeProblem;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.api.MultiQuantifierEliminator;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.rewriter.api.Simplifier;

public class SimplifierForAggregateFunctionOnIntensionalSet implements Simplifier {

	private AssociativeCommutativeGroup group;
	private MultiQuantifierEliminator quantifierEliminator;

	public SimplifierForAggregateFunctionOnIntensionalSet(
			AssociativeCommutativeGroup group,
			MultiQuantifierEliminator quantifierEliminator) {
		
		super();
		this.group = group;
		this.quantifierEliminator = quantifierEliminator;
	}

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		Expression result;
		try {
			Expression firstArgument = expression.get(0);
			if (Sets.isIntensionalMultiSet(firstArgument)) {
				IntensionalSet intensionalSet = (IntensionalSet) firstArgument;
				ExtensionalIndexExpressionsSet indexExpressions = 
						(ExtensionalIndexExpressionsSet) intensionalSet.getIndexExpressions();
				Context contextWithIndexExpressions = context.extendWith(indexExpressions);
				MultiQuantifierEliminationProblem problem = makeProblem(intensionalSet, indexExpressions, contextWithIndexExpressions);
				result = quantifierEliminator.solve(problem, contextWithIndexExpressions);
			}
			else {
				result = expression;
			}
		}
		catch (IllegalArgumentException exception) {
			result = expression;
		}
		return result;
	}

	private MultiQuantifierEliminationProblem makeProblem(
			IntensionalSet intensionalSet,
			ExtensionalIndexExpressionsSet indexExpressions, 
			Context contextWithIndexExpressions) {
		
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		Expression body = intensionalSet.getHead();
		Expression condition = intensionalSet.getCondition();
		MultiQuantifierEliminationProblem problem = DefaultMultiQuantifierEliminationProblem.makeProblem(group, indices, condition, body, contextWithIndexExpressions);
		return problem;
	}
	
}