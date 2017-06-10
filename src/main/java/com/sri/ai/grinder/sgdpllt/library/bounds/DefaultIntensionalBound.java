package com.sri.ai.grinder.sgdpllt.library.bounds;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;
//import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PRODUCT;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;

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
		//TODO Don't know how to represent
		
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
	public DefaultIntensionalBound normalize(Bound bound, Theory theory, Context context) {
		//not tested
		if(!bound.isIntensionalBound()){
			//TODO Launch exception or something
			return null;
		}
		
		IntensionalSet intensionalBound = (IntensionalSet) bound;
		
		IndexExpressionsSet indexExpressions = intensionalBound.getIndexExpressions();
		Expression Head                      = intensionalBound.getHead();
		Expression condition                 = intensionalBound.getCondition();
		
		IndexExpressionsSet freeVariablesOfTheHead = getIndexExpressionsOfFreeVariablesIn(Head, context);
		
		Expression setInstantiationsOfTheHead = IntensionalSet.makeMultiSet(
				freeVariablesOfTheHead,
				Head,//head
				makeSymbol(true)//No Condition
				);
		
		Expression sumOnPhi = apply(SUM, setInstantiationsOfTheHead);
		Expression normalizedHead =  apply("/", Head, sumOnPhi);
		
		Expression evaluation = theory.evaluate(normalizedHead, context);
		
		DefaultIntensionalBound normalizedIntensionalSet = 
				new DefaultIntensionalBound(indexExpressions, evaluation, condition);
		return normalizedIntensionalSet;		
	}

	@Override
	public DefaultIntensionalBound boundProduct(Theory theory, Context context, Expression... listOfBounds) {
		// unite the index expressions and multiply the heads
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public DefaultIntensionalBound summingBound(Expression variablesToBeSummedOut, Bound bound, Context context, Theory theory) {
		//not tested
		if(!bound.isIntensionalBound()){
			//TODO Launch exception or something
			return null;
		}
		Expression x = makeSymbol("variableX");
		
		IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(variablesToBeSummedOut, context);
		
		Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
				indices,
				x,//head
				makeSymbol(true)//No Condition
				);
		Expression f = apply(SUM, setOfFactorInstantiations);
		
		DefaultIntensionalBound result = applyFunctionToBound(f, x, bound, theory, context);
		return result;		
	}

	@Override
	public DefaultIntensionalBound summingPhiTimesBound(Expression variablesToBeSummedOut, Expression phi, Bound bound, Context context,
			Theory theory) {
		//not tested
		if(!bound.isIntensionalBound()){
			//TODO Launch exception or something
			return null;
		}
		Expression x = makeSymbol("l");
		Expression f = apply(TIMES, x,phi);
		
		DefaultIntensionalBound fOfBound = applyFunctionToBound(f, x, bound, theory, context);
		
		DefaultIntensionalBound result = summingBound(variablesToBeSummedOut, fOfBound, context, theory);
		return result;				
	}

	@Override
	public DefaultIntensionalBound applyFunctionToBound(Expression f, Expression variableName, Bound bound, Theory theory, Context context) {
		//not tested
		if(!bound.isIntensionalBound()){
			//TODO Launch exception or something
			return null;
		}
		
		IntensionalSet intensionalBound = (IntensionalSet) bound;
		
		IndexExpressionsSet indexExpressions = intensionalBound.getIndexExpressions();
		Expression Head                      = intensionalBound.getHead();
		Expression condition                 = intensionalBound.getCondition();
		
		Expression fOfHead = f.replaceAllOccurrences(variableName, Head, context);
		Expression evaluation = theory.evaluate(fOfHead, context);
		
		DefaultIntensionalBound result = new DefaultIntensionalBound(indexExpressions, evaluation, condition);
		return result;
	}
}
