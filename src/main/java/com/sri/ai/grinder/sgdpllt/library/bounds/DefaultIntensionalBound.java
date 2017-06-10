package com.sri.ai.grinder.sgdpllt.library.bounds;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;

public class DefaultIntensionalBound extends AbstractIntensionalBound{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public DefaultIntensionalBound(IndexExpressionsSet indexExpressions, Expression head, Expression condition) {
		super(indexExpressions, head, condition);
		}

	public DefaultIntensionalBound(List<Expression> indexExpressionsList, Expression head, Expression condition) {
		this(new ExtensionalIndexExpressionsSet(indexExpressionsList), head, condition);
	}

	@Override
	public Bound simplex(List<Expression> Variables, Model model) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Bound normalize(Expression bound, Theory theory, Context context) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Bound boundProduct(Theory theory, Context context, Expression... listOfBounds) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Bound summingBound(Expression variablesToBeSummedOut, Expression bound, Context context, Theory theory) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Bound summingPhiTimesBound(Expression variablesToBeSummedOut, Expression phi, Expression bound,
			Context context, Theory theory) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Bound applyFunctionToBound(Expression f, Expression variableName, Expression b, Theory theory,
			Context context) {
		// TODO Auto-generated method stub
		return null;
	}
}
