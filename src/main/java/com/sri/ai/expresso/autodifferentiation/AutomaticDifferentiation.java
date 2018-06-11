package com.sri.ai.expresso.autodifferentiation;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.isSubExpressionOf;
import static com.sri.ai.grinder.library.FunctorConstants.DERIV;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIAL;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIATION;
import static com.sri.ai.grinder.library.FunctorConstants.LOG;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.grinder.library.FunctorConstants.TIMES;
import static com.sri.ai.util.Util.thereExists;

import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.FunctorConstants;

/**
 * Class for automatic symbolic differentiation of {@link Expression}
 * 
 * @author Sarah Perrin
 */

public class AutomaticDifferentiation {
	
	private Function<Expression, Expression> simplifier;
	
	public AutomaticDifferentiation(Function<Expression, Expression> simplifier){
		this.simplifier = simplifier;
	}
	
	/**
	 * Method which compute the partial derivative of an {@link Expression}, regarding the type of the argument passed.
	 * If the argument is a {@link DefaultFunctionApplication} or a {@link Symbol}, it computes as much as possible the differentiation.
	 * Otherwise, returns "deriv(expression, argument)"
	 * 
	 */
	public Expression differentiateExpression(Expression f, Expression argument) {
		Expression result;
		if (argument.getSyntacticFormType().equals("Symbol") || argument.getSyntacticFormType().equals("Function application")) {
			result = differentiateExpressionIfArgumentIsSymbolOrFunctionApplication(f, argument);
		}
		else { 
			result = apply(DERIV, f, argument);
		}
		return result;
	}
	
	/**
	 * Main method. Recursive algorithm which compute the partial derivative of an {@link Expression} 
	 * (either a {@link DefaultFunctionApplication} or a {@link Symbol}
	 * 
	 */
	private Expression differentiateExpressionIfArgumentIsSymbolOrFunctionApplication(Expression f, Expression argument) {
		
		Expression result;
		if (f.getSyntacticFormType().equals("Symbol")) {
			result = differentiateSymbol(f, argument);
		}
		else if (f.hasFunctor(PLUS) || f.hasFunctor(MINUS)) {
			result = differentiateSumOrSubstraction(f, argument);
		}
		else if (f.hasFunctor(TIMES)) {
			result = differentiateMultiplication(f, argument);
		}
		else if (f.hasFunctor(DIVISION)) {
			result = differentiateDivision(f, argument);
		}
		else if (f.hasFunctor(EXPONENTIATION)) {
			result = differentiateExponentiation(f, argument);
		}
		else if (f.hasFunctor(EXPONENTIAL)) {
			result = differentiateExponential(f, argument);
		}
		else if (f.hasFunctor(LOG)) {
			result = differentiateLog(f, argument);
		}
		else if (f.getSyntacticFormType().equals("Function application")) {
			result = differentiateUndefinedFunctionApplication(f, argument);
		}
		else {
			result = apply(DERIV, f, argument);
		}
		result = simplify(result);
		
		return result;
	}

	private Expression differentiateLog(Expression f, Expression argument) {
		Expression result;
		if (f.numberOfArguments() == 1) {
			result = apply(FunctorConstants.DIVISION, differentiateExpression(f.get(0), argument), f.get(0));
		}
		else {
			result = apply(DERIV, f, argument);
		}
		return result;
	}

	private Expression differentiateExponential(Expression f, Expression argument) {
		Expression result;
		if (f.numberOfArguments() > 0) {
			result = apply(FunctorConstants.TIMES, differentiateExpression(f.get(0), argument), f);
		}
		else {
			result = apply(DERIV, f, argument);
		}
		return result;
	}

	/**
	 * Differentiate the {@link Expression} f when f is a {@link Symbol}
	 * 
	 */
	private Expression differentiateSymbol(Expression f, Expression argument) {
		Expression result;
		if (f.getValue() instanceof String) {
			result = differentiateString(f, argument);
		}
		else {
			result = ZERO;
		}
		return result;
	}

	/**
	 * Differentiate the {@link Expression} f when f is a {@link String}
	 * 
	 */
	private Expression differentiateString(Expression f, Expression argument) {
		Expression result;
		if (f.equals(argument)) {
			result = ONE;
		 }
		else if (argument.getSyntacticFormType().equals("Function application")) {
			result = differentiateStringWhenArgumentIsFunctionApplication(f, argument);
		}
		else {
			result = ZERO;
		}
		return result;
	}

	private Expression differentiateStringWhenArgumentIsFunctionApplication(Expression f, Expression argument) {
		Expression result;
		if (symbolIsAnArgumentOfFunction((Symbol)f, (DefaultFunctionApplication)argument)) {
			result = apply(DERIV, f, argument);
		}
		else {
			result = ZERO;
		}
		return result;
	}

	/**
	 * Simplify the expression: removes multiplication and additions with 0, multiplication with 1...
	 * 
	 */
	public Expression simplify(Expression expression) {
		Expression result = simplifier.apply(expression);
		return result;
	}
	
	/**
	 * Returns a boolean which indicates if the {@link Expression} has one of its arguments which is a {@link DefaultFunctionApplication}
	 * 
	 */
	private boolean isAComposed(Expression f) {
		boolean result = thereExists(f.getArguments(), a -> a.getSyntacticFormType().equals("Function application"));
		return result;
	}

	/**
	 * Returns a boolean which indicates if the {@link Expression} has one of its direct arguments 
	 * which is the {@link DefaultFunctionApplication} f passed in the arguments.
	 * 
	 */
	private boolean isADirectComposedOfArgument(Expression f, Expression argument) {
		boolean result = thereExists(f.getArguments(), a -> a.equals(argument));
		return result;
	}
	
	/**
	 * Returns a boolean which indicates if the {@link DefaultFunctionApplication} f has one of its arguments 
	 * which is the {@link Symbol} argument passed in the arguments.
	 * 
	 */
	private boolean symbolIsAnArgumentOfFunction(Symbol argument, FunctionApplication f) {
		boolean result = isSubExpressionOf(argument, f);
		return result;
	}

	/**
	 * Differentiate a sum or substraction of n terms : a1+a2+...+an or a1-a2-...-an or -a1
	 * 
	 */
	public Expression differentiateSumOrSubstraction(Expression f, Expression argument) {
		Expression result;
		if (f.numberOfArguments() > 0) {
			result = f;
			result = setArgumentsWhenDifferentiatingSumOrSubstraction(f, argument, result);
		}
		else { 
			result = apply(DERIV, f, argument);
		}
		return result;
	}

	private Expression setArgumentsWhenDifferentiatingSumOrSubstraction(Expression f, Expression argument,
			Expression result) {
		for (int i = 0; i < f.numberOfArguments(); i++) {
			Expression ithTermOfF = f.get(i);
			Expression newIthArgument = differentiateExpression(ithTermOfF, argument);
			result = result.set(i, newIthArgument);
		}
		return result;
	}
	
	/**
	 * Differentiate a product of n terms: a1*a2*...*an
	 * 
	 */
	public Expression differentiateMultiplication(Expression f, Expression argument) {
		Expression result;
		if (f.numberOfArguments() > 1) {
			result = apply(PLUS,f.getArguments());
			result = setArgumentsWhenDifferentiatingMultiplication(f, argument, result);
		}
		else { 
			result = apply(DERIV, f, argument);
		}
		return result;
	}

	/**
	 * Set the arguments during the differentiation of a product. The ith term of result in the loop is a1*a2*...*ai'*...*an
	 * 
	 */
	private Expression setArgumentsWhenDifferentiatingMultiplication(Expression f, Expression argument,
			Expression result) {
		for (int i = 0; i < f.numberOfArguments(); i++) {
			Expression ithTermOfResult = apply(TIMES,f.getArguments());
			ithTermOfResult = ithTermOfResult.set(i, differentiateExpression(ithTermOfResult.get(i), argument));
			result = result.set(i, ithTermOfResult);
		}
		return result;
	}
	
	/**
	 * Differentiate u/v
	 * 
	 */
	public Expression differentiateDivision(Expression f, Expression argument) {
		Expression result;
		if (f.numberOfArguments() == 2) {
			
			Expression u = f.get(0);
			Expression v = f.get(1);
			
			Expression uPrime = differentiateExpression(u, argument);
			Expression vPrime = differentiateExpression(v, argument);
			
			Expression uVPrime = apply(TIMES, u, vPrime);
			Expression uPrimeV = apply(TIMES, uPrime, v);
			
			Expression uPrimeVMinusuVPrime = apply(MINUS, uPrimeV, uVPrime);
			
			Expression vSquare = apply(TIMES,v, v);
			
			result = apply(DIVISION, uPrimeVMinusuVPrime, vSquare);
			
		}
		else { 
			result = apply(DERIV, f, argument);
		}
		return result;
	}
	
	/**
	 * Differentiate u^v
	 * 
	 */
	public Expression differentiateExponentiation(Expression f, Expression argument) {
		Expression result;
		if (f.numberOfArguments() == 2) {
			
			Expression uExpV = apply(EXPONENTIATION, f.getArguments());
			
			Expression logUDerivV = apply(TIMES,Expressions.apply(LOG,f.get(0)),differentiateExpression(f.get(1), argument));
			Expression vDividedByUDerivU = apply(TIMES, apply(DIVISION, f.get(1), f.get(0)), differentiateExpression(f.get(0), argument));
			Expression logUDerivVPlusVDividedByUDerivU = apply(PLUS, logUDerivV, vDividedByUDerivU);
			
			result = apply(TIMES, uExpV, logUDerivVPlusVDividedByUDerivU );
			
		}
		else {
			result = apply(DERIV, f, argument);
		}
		return result;
	}
	

	/**
	 * Differentiate the {@link Expression} f when f has an undefined functor (different from PLUS, MINUS, TIMES, DIVISION, EXPONENTIATION)
	 * 
	 */
	private Expression differentiateUndefinedFunctionApplication(Expression f, Expression argument) {
		Expression result;
		if(isAComposed(f) && !isADirectComposedOfArgument(f, argument)) {
			result = differentiateAnyFunction(f, argument);
		}
		else if(!isAComposed(f) && !isADirectComposedOfArgument(f, argument)) {
			result = ZERO;
		}
		else {
			result = apply(DERIV, f, argument);
		}
		return result;
	}
	
	/**
	 * Is called only when the {@link Expression} argument is not one of the direct arguments of {@link Expression} f. Used to differentiate composed functions.
	 * 
	 */
	public Expression differentiateAnyFunction(Expression f, Expression argument) {
		Expression result;
		if (f.numberOfArguments() > 0) {
			result = apply(PLUS,f.getArguments());
			result = setArgumentsWhenDifferentiatingComposedFunction(f, argument, result);
		}
		else {
			result = apply(DERIV, f, argument);
		}
		return result;
	}

	private Expression setArgumentsWhenDifferentiatingComposedFunction(Expression f, Expression argument, Expression result) {
		for (int i = 0; i < f.numberOfArguments(); i++) {
			Expression ithTermOfF = f.get(i);
			Expression argument_i = apply(TIMES,differentiateExpression(f,ithTermOfF),differentiateExpression(f.get(i), argument));
			result = result.set(i, argument_i);
		}
		return result;
	}
	
}
