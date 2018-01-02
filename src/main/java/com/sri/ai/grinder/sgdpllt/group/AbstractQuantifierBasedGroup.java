package com.sri.ai.grinder.sgdpllt.group;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.MultiQuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.util.base.Pair;

/**
 * A group whose related problems are represented by a quantified expression<br>
 * (e.g.: <code>for all X in Integer: X > 0 and X < 5</code>).
 * 
 * @author braz
 *
 */
public abstract class AbstractQuantifierBasedGroup extends AbstractAssociativeCommutativeGroup {

	/**
	 * Provides a quantified expression based on index expression and body.
	 * @param indexExpression
	 * @param body
	 * @return
	 */
	public abstract Expression makeQuantifiedExpression(List<Expression> indexExpressions, Expression body);

	@Override
	public Pair<Expression, IndexExpressionsSet> getExpressionAndIndexExpressionsFromProblemExpression(Expression expression, Context context) {
		
		QuantifiedExpressionWithABody quantifiedFormula = (QuantifiedExpressionWithABody) expression;
		Pair<Expression, IndexExpressionsSet> formulaAndIndices = 
				Pair.make(quantifiedFormula.getBody(), quantifiedFormula.getIndexExpressions());
		return formulaAndIndices;
	}

	@Override
	public Expression makeProblemExpression(MultiQuantifierEliminationProblem problem) {
		Expression result = makeProblemExpression(problem.getIndices(), problem.getIndicesTypes(), problem.getConstraint(), problem.getBody());
		return result;
	}

	public Expression makeProblemExpression(List<Expression> indices, List<Expression> indicesTypes, Expression constraint, Expression body) {
		List<Expression> indexExpressions = IndexExpressions.makeIndexExpressions(indices, indicesTypes);
		Expression bodyEncodingConstraint = IfThenElse.make(constraint, body, additiveIdentityElement());
		Expression result = makeQuantifiedExpression(indexExpressions, bodyEncodingConstraint);
		return result;
	}

}