package com.sri.ai.grinder.sgdpll.group;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.util.base.Pair;

/**
 * A group whose related problems are represented by a quantified expression<br>
 * (e.g.: <code>for all X in Integer: X > 0 and X < 5</code>).
 * 
 * @author braz
 *
 */
public abstract class AbstractQuantifierBasedGroup implements AssociativeCommutativeGroup {

	/**
	 * Provides a quantified expression based on index expression and body.
	 * @param indexExpression
	 * @param body
	 * @return
	 */
	public abstract Expression makeQuantifiedExpression(Expression indexExpression, Expression body);

	@Override
	public Pair<Expression, IndexExpressionsSet> getExpressionAndIndexExpressionsFromProblemExpression(Expression expression, Context context) {
		
		QuantifiedExpressionWithABody quantifiedFormula = (QuantifiedExpressionWithABody) expression;
		Pair<Expression, IndexExpressionsSet> formulaAndIndices = 
				Pair.make(quantifiedFormula.getBody(), quantifiedFormula.getIndexExpressions());
		return formulaAndIndices;
	}

	@Override
	public Expression makeProblemExpression(Expression index, Expression indexType, Expression constraint, Expression body) {
		Expression indexExpression = IndexExpressions.makeIndexExpression(index, indexType);
		Expression bodyEncodingConstraint = IfThenElse.make(constraint, body, additiveIdentityElement());
		Expression result = makeQuantifiedExpression(indexExpression, bodyEncodingConstraint);
		return result;
	}

}