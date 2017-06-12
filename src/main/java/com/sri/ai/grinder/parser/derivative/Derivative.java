package com.sri.ai.grinder.parser.derivative;

import static com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets.getElements;

import java.sql.Array;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import static com.sri.ai.util.Util.in;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;


import static com.sri.ai.expresso.helper.Expressions.parse;

public class Derivative {

	public static Expression derivativeSingleExpression(Expression expression, Expression query, Model model){
		Context context = model.context;
		Set<Expression> variableInFactor = Expressions.freeVariables(expression, context);
		variableInFactor.remove(query);
		Set<Expression> ProbabilitiesFactor = new HashSet<Expression>();
		for (Expression variable : variableInFactor){
			String str = "";
			Type type = context.getTypeOfRegisteredSymbol(variable);
			Iterator<Expression> valuesInType = type.iterator();
			List<Expression> probability = new ArrayList<Expression> ();
			int i = 0;
			for(Expression values : in(valuesInType)){
				String s = "prob" + values.toString();
				probability.add(parse(s));
				str = str + "if " + variable + " then " + parse(s) + " else ";
			}
			str = str + " 0 ";
			ProbabilitiesFactor.add(parse(str));
		}
		Expression product = expression;
		for (Expression factor : ProbabilitiesFactor){
			product = apply(TIMES, product, factor);
		}
		Expression evaluation = product;
		for (Expression variable : variableInFactor){
			IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(variable, context);
		
			Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
				indices,
				evaluation,//head
				makeSymbol(true)//No Condition
				);
		
			Expression sumOnPhi = apply(SUM, setOfFactorInstantiations);
			evaluation = model.theory.evaluate(sumOnPhi, context);
		}
		return evaluation;
	}
	
	
}
