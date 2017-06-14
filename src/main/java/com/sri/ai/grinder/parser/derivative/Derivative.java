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
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.AND;
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
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets;

import static com.sri.ai.expresso.helper.Expressions.parse;

public class Derivative {

	public static Expression derivativeSingleExpression(Expression expression, Expression query, Model model){
		Context context = model.context;
		Set<Expression> variableInFactor = Expressions.freeVariables(expression, context);
		Set<Expression> bounds = new HashSet<Expression>();
		variableInFactor.remove(query);
		Set<Expression> ProbabilitiesFactor = new HashSet<Expression>();
		Expression condition = parse("true");
		for (Expression variable : variableInFactor){
			String str = "";
			String strCondition = "";
			Type type = context.getTypeOfRegisteredSymbol(variable);
			Iterator<Expression> valuesInType = type.iterator();
			List<Expression> probability = new ArrayList<Expression> ();
			for(Expression values : in(valuesInType)){
				String s = "prob" + variable.toString() + values.toString();
				probability.add(parse(s));
				model.extendModelWithSymbolsAndTypes(s, "0..1");
				strCondition = strCondition + s + " + ";
				str = str + "if " + variable + " = " + values.toString() +" then " + parse(s) + " else ";
			}
			str = str + " 0";
			strCondition = strCondition + "0 = 1";

			condition = apply(AND, condition, parse(strCondition));
			//Expression withCondition = model.theory.evaluate(parse(strCondition), context);
			ProbabilitiesFactor.add(parse(str));
		}
		
		Expression product = expression;
		for (Expression factor : ProbabilitiesFactor){
			product = apply(TIMES, product, factor);
		}
		Expression evaluation = product;

		context = model.context;
		for (Expression variable : variableInFactor){
			IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(variable, context);

			Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
				indices,
				evaluation,//head
				condition
				);
		
			Expression sumOnPhi = apply(SUM, setOfFactorInstantiations);
			System.out.println(sumOnPhi);
			evaluation = model.theory.evaluate(sumOnPhi, context);
		}
		return evaluation;
	}
	
	
}
