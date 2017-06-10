package com.sri.ai.grinder.sgdpllt.library.bounds;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;
//import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PRODUCT;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
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
	public DefaultIntensionalBound simplex(List<Expression> Variables, Model model) {

		Context context= model.context;
		Expression one = makeSymbol("1");
		Expression zero= makeSymbol("0");
	
		Object[] simplexOnASingleVariable = new Expression[Variables.size()];
		
		int i = 0;
		for(Expression var : Variables){
			Expression c = makeSymbol("c");
			IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(var, context);
			IndexExpressionsSet indicesReplaced = indices.replaceSymbol(var, c, context);
			
			Expression simplexOnVar = IntensionalSet.makeUniSet(
					indicesReplaced,
					apply(IF_THEN_ELSE,apply(EQUAL,var,c),one,zero),
					makeSymbol(true)//No Condition
					);
			
			simplexOnASingleVariable[i] = simplexOnVar;
			i++;
		}
		
//		DefaultIntensionalBound result = apply(PRODUCT, simplexOnASingleVariable);
		//return result;
		return null;
	}


	@Override
	public Bound normalize(Bound bound, Theory theory, Context context) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Bound boundProduct(Theory theory, Context context, Expression... listOfBounds) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Bound summingBound(Expression variablesToBeSummedOut, Bound bound, Context context, Theory theory) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Bound summingPhiTimesBound(Expression variablesToBeSummedOut, Expression phi, Bound bound, Context context,
			Theory theory) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Bound applyFunctionToBound(Expression f, Expression variableName, Bound b, Theory theory, Context context) {
		// TODO Auto-generated method stub
		return null;
	}

	
}
