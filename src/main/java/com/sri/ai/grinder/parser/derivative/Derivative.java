package com.sri.ai.grinder.parser.derivative;


import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import static com.sri.ai.util.Util.in;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;

import static com.sri.ai.expresso.helper.Expressions.parse;

public class Derivative {

	public static Expression Derivative(Expression expression, Expression variable, Context context) {
		Expression functor = expression.getFunctor();
		if (functor == parse("*")) {
			return productCase(expression, variable, context);
		}
		if (functor == null) {
			return constantCase(expression, variable, context);
		}
		if (functor == parse("+")) {
			return sumCase(expression, variable, context);
		}
		if (functor == parse("-")) {
			return difCase(expression, variable, context);
		}
		if (functor == parse("/")) {
			return divCase(expression, variable, context);
		}
		if (functor == parse("^")) {
			return puissCase(expression, variable, context);
		}
		if (functor == parse("ln")) {
			return lnCase(expression, variable, context);
		}
		if (functor == parse("'if . then . else .'")) {
			return ifThenElseCase(expression, variable, context);
		}
		return null;
	}
	
	public static Expression constantCase(Expression expression, Expression variable, Context context) {
		if (expression.equals(variable)) {
			return parse("1");
		}
		if (expression.equals(parse("Undefined"))) {
			return parse("Undefined");
		}
		
		return parse("0");
	}
	
	public static Expression productCase(Expression expression, Expression variable, Context context) {
		Theory theory = context.getTheory();
		List<Expression> arguments = expression.getArguments();
		Expression factor = arguments.get(1);
		for (int i = 2; i<arguments.size(); i++) {
			factor = apply(TIMES, factor, arguments.get(i));
		}
		Expression toEvaluate = apply("+",apply("*", Derivative(arguments.get(0), variable, context), factor), apply("*", arguments.get(0), Derivative(factor, variable, context)));
		return theory.simplify(toEvaluate , context);
	}
	
	public static Expression sumCase(Expression expression, Expression variable, Context context) {
		Theory theory = context.getTheory();
		List<Expression> arguments = expression.getArguments();
		Expression sum = arguments.get(1);
		for (int i = 2; i<arguments.size(); i++) {
			sum = apply("+", sum, arguments.get(i));
		}
		Expression toEvaluate = apply("+",Derivative(arguments.get(0), variable, context), Derivative(sum, variable, context));
		return theory.simplify(toEvaluate , context);
	}
	
	public static Expression difCase(Expression expression, Expression variable, Context context) {
		Theory theory = context.getTheory();
		List<Expression> arguments = expression.getArguments();
		Expression sum = arguments.get(1);
		for (int i = 2; i<arguments.size(); i++) {
			sum = apply("*", sum, arguments.get(i));
		}
		Expression toEvaluate = apply("-",Derivative(arguments.get(0), variable, context), Derivative(sum, variable, context));
		return theory.simplify(toEvaluate , context);
	}
	
	public static Expression divCase(Expression expression, Expression variable, Context context) {
		Theory theory = context.getTheory();
		List<Expression> arguments = expression.getArguments();
		Expression factor = arguments.get(1);
		for (int i = 2; i<arguments.size(); i++) {
			factor = apply(TIMES, factor, arguments.get(i));
		}
		Expression toEvaluate = apply("-",apply("*", Derivative(arguments.get(0), variable, context), factor), apply("*", arguments.get(0), Derivative(factor, variable, context)));
		toEvaluate = apply("/", toEvaluate, apply("*", factor, factor));
		return theory.simplify(toEvaluate , context);
	}
	
	public static Expression puissCase(Expression expression, Expression variable, Context context) {
		Theory theory = context.getTheory();
		List<Expression> arguments = expression.getArguments();
		Expression factor = arguments.get(1);
		for (int i = 2; i<arguments.size(); i++) {
			factor = apply("^", factor, arguments.get(i));
		}
		Expression toEvaluate = apply("*", 
										Derivative.Derivative(apply("*",
																	factor,
																	apply("ln", arguments.get(0))
																	), variable, context), 
										expression);
										
		return theory.simplify(toEvaluate , context);
	}
	
	public static Expression lnCase(Expression expression, Expression variable, Context context) {
		Theory theory = context.getTheory();
		Expression insideLn = expression.getArguments().get(0);
		Expression toEvaluate = apply("/", 
										Derivative.Derivative(insideLn, variable, context), 
										insideLn);
										
		return theory.simplify(toEvaluate , context);
	}
	
	public static Expression ifThenElseCase(Expression expression, Expression variable, Context context) {
		Theory theory = context.getTheory();
		Expression condition = expression.getArguments().get(0);
		Expression trueBranch = expression.getArguments().get(1);
		Expression falseBranch = expression.getArguments().get(2);
		
		Set<Expression> conditionVariables = Expressions.freeVariables(condition, context);
	
		if (!conditionVariables.contains(variable)) {
			Expression toEvaluate = IfThenElse.make(condition, Derivative.Derivative(trueBranch, variable, context), Derivative.Derivative(falseBranch, variable, context));		
			return theory.simplify(toEvaluate , context);
		}
		else{
			Expression functor = condition.getFunctor();
			if (functor == parse("=")) {
				Expression toEvaluate = IfThenElse.make(condition, parse("Undefined"), Derivative.Derivative(falseBranch, variable, context));		
				return theory.simplify(toEvaluate , context);
			}
			if (functor == parse("!=")) {
				Expression toEvaluate = IfThenElse.make(condition, Derivative.Derivative(trueBranch, variable, context), parse("Undefined"));		
				return theory.simplify(toEvaluate , context);
			}
			if (functor == parse("<=") || functor == parse(">=")) {
				Expression toEvaluate = IfThenElse.make(condition, 
														IfThenElse.make(apply("=",condition.getArguments().get(0), condition.getArguments().get(1)), 
																		parse("Undefined"), 
																		Derivative.Derivative(trueBranch, variable, context)), 
														Derivative.Derivative(falseBranch, variable, context));	
				return theory.simplify(toEvaluate , context);
			}
			if (functor == parse("<") || functor == parse(">")) {
				Expression toEvaluate = IfThenElse.make(condition, 
														Derivative.Derivative(trueBranch, variable, context),
														IfThenElse.make(apply("=",condition.getArguments().get(0), condition.getArguments().get(1)), 
																		parse("Undefined"), 
																		Derivative.Derivative(falseBranch, variable, context)));	
				return theory.simplify(toEvaluate , context);
			}
		}						
		return parse("UndefinedIfThenElseFunctor");
	}
	
	public static Set<Expression> derivativesOfFactor(Expression expression, Expression query, Context context) {
		Theory theory = context.getTheory();
		Set<Expression> variableInFactor = Expressions.freeVariables(expression, context);
		variableInFactor.remove(query);
		Set<Expression> ProbabilitiesFactor = new HashSet<Expression>();
		for (Expression variable : variableInFactor) {
			String str = "";
			Type type = context.getTypeOfRegisteredSymbol(variable);
			Iterator<Expression> valuesInType = type.iterator();
			List<Expression> probability = new ArrayList<Expression> ();
			for (Expression values : in(valuesInType)) {
				String s = "prob" + variable.toString() + values.toString();
				probability.add(parse(s));
				context.extendWithSymbolsAndTypes(s, "0..1");
				str = str + "if " + variable + " = " + values.toString() +" then " + parse(s) + " else ";
			}
			str = str + " 0";
			ProbabilitiesFactor.add(parse(str));
		}
		
		Expression product = expression;
		for (Expression factor : ProbabilitiesFactor) {
			product = apply(TIMES, product, factor);
		}
		Expression evaluation = product;

		for (Expression variable : variableInFactor) {
			IndexExpressionsSet indices = getIndexExpressionsOfFreeVariablesIn(variable, context);

			Expression setOfFactorInstantiations = IntensionalSet.makeMultiSet(
				indices,
				evaluation,// head
				parse("true")
				);
		
			Expression sumOnPhi = apply(SUM, setOfFactorInstantiations);
			System.out.println(sumOnPhi);
			evaluation = theory.evaluate(sumOnPhi, context);
		}
		int i = 0;
		Set<Expression> result = new HashSet<Expression>();
		System.out.println(result);
		for (Expression variable : variableInFactor) {
			Type type = context.getTypeOfRegisteredSymbol(variable);
			Iterator<Expression> valuesInType = type.iterator();
			for (Expression values : in(valuesInType)) {
				String s = "prob" + variable.toString() + values.toString();
				result.add(Derivative.Derivative(evaluation, parse(s), context));
			}
			i++;
		}
		
		
		return result;
	}
	
}
