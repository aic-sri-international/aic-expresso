package com.sri.ai.grinder.sgdpllt.group;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions.makeIndexExpression;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * A group whose related problems are represented as function applications on sets<br>
 * (e.g.: <code>sum({{ (on X in Integer) X | X > 0 and X < 5 }})</code>).
 * 
 * @author braz
 *
 */
public abstract class AbstractFunctionBasedGroup extends AbstractNumericGroup implements AssociativeCommutativeGroup {

	public AbstractFunctionBasedGroup() {
		super();
	}

	public abstract String getFunctionString();

	@Override
	public Pair<Expression, IndexExpressionsSet> getExpressionAndIndexExpressionsFromProblemExpression(Expression expression, Context context) {
		String functorString = getFunctionString();
		Util.myAssert(() -> expression.hasFunctor(functorString), () -> "Expression expected to be application of " + functorString + " but is " + expression);
		IntensionalSet set = (IntensionalSet) expression.get(0);
		Expression body = IfThenElse.make(set.getCondition(), set.getHead(), additiveIdentityElement());
		Pair<Expression, IndexExpressionsSet> result = Pair.make(body, set.getIndexExpressions());
		return result;
	}

	@Override
	public Expression makeProblemExpression(Expression index, Expression indexType, Expression constraint, Expression body) {
		Expression indexExpression = makeIndexExpression(index, indexType);
		IndexExpressionsSet indexExpressionsSet = new ExtensionalIndexExpressionsSet(indexExpression); 
		DefaultIntensionalMultiSet set = new DefaultIntensionalMultiSet(indexExpressionsSet, body, constraint);
		Expression problem = apply(getFunctionString(), set);
		return problem;
	}
}