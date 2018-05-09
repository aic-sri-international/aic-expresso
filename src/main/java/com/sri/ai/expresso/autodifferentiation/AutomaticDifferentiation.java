package com.sri.ai.expresso.autodifferentiation;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.DERIV;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIATION;
import static com.sri.ai.grinder.library.FunctorConstants.LOG;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.grinder.library.FunctorConstants.TIMES;

import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
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
	public Expression differentiateExpression(Expression f, Expression arg) {
		if (arg instanceof Symbol || arg instanceof DefaultFunctionApplication) {
			return differentiateExpressionIfArgIsSymbolOrFunctionApplication(f, arg);
		}
		else { 
			return apply(DERIV, f, arg);
		}
	}
	
	/**
	 * Main method. Recursive algorithm which compute the partial derivative of an {@link Expression} 
	 * (either a {@link DefaultFunctionApplication} or a {@link Symbol}
	 * 
	 */
	private Expression differentiateExpressionIfArgIsSymbolOrFunctionApplication(Expression f, Expression arg) {
		
		Expression result = apply(DERIV, f, arg);
		if (f.getSyntacticFormType().equals("Symbol")) {
			result = differentiateSymbol(f, arg);
		}
		else if (f.hasFunctor("+") || f.hasFunctor("-")) {
			result = differentiateSumOrSubstraction(f, arg);
		}
		else if (f.hasFunctor("*")) {
			result = differentiateMultiplication(f, arg);
		}
		else if (f.hasFunctor("/")) {
			result = differentiateDivision(f,arg);
		}
		else if (f.hasFunctor("^")) {
			result = differentiateExponentiation(f,arg);
		}
		else if (f instanceof DefaultFunctionApplication) {
			if (isAComposed(f)) {
				if (isADirectComposedOfArg(f,arg)) {
					result = apply(DERIV, f, arg);
				}
				else {
					result = differentiateAnyFunction(f,arg);
				}
			}
			else {
				if (isADirectComposedOfArg(f,arg)) {
					result = apply(DERIV, f, arg);
				}
				else {
					result = ZERO;
				}
			}
		}
		
		result = simplify(result);
		
		return result;
	}

	/**
	 * Differentiate the {@link Expression} f when f is a {@link Symbol}
	 * 
	 */
	private Expression differentiateSymbol(Expression f, Expression arg) {
		Expression result;
		if (f.getValue() instanceof String) {
			if (f.equals(arg)) {
				result = ONE;
		     }
			else if (arg instanceof DefaultFunctionApplication) {
				if (symbolIsAnArgumentOfFunction((Symbol)f, (DefaultFunctionApplication)arg)) {
					result = apply(DERIV, f, arg);
				}
				else {
					result = ZERO;
				}
			}
			else {
				result = ZERO;
			}
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
	private Expression simplify(Expression expression) {
		Expression result = simplifier.apply(expression);
		return result;
	}
	
	/**
	 * Returns a boolean which indicates if the {@link Expression} has one of its arguments which is a {@link DefaultFunctionApplication}
	 * 
	 */
	private boolean isAComposed(Expression f) {
		for (int i = 0; i<f.numberOfArguments(); i++ ) {
			if (f.get(i) instanceof DefaultFunctionApplication) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns a boolean which indicates if the {@link Expression} has one of its direct arguments 
	 * which is the {@link DefaultFunctionApplication} f passed in the arguments.
	 * 
	 */
	private boolean isADirectComposedOfArg(Expression f, Expression arg) {
		for (int i = 0; i<f.numberOfArguments(); i++ ) {
			if (f.get(i).equals(arg)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Returns a boolean which indicates if the {@link DefaultFunctionApplication} f has one of its arguments 
	 * which is the {@link Symbol} arg passed in the arguments.
	 * 
	 */
	private boolean symbolIsAnArgumentOfFunction(Symbol arg, DefaultFunctionApplication f) {
		for (int i = 0; i<f.numberOfArguments(); i++ ) {
			if (f.get(i).equals(arg)) {
				return true;
			}
		}
		for (int i = 0; i<f.numberOfArguments(); i++ ) {
			if (f.get(i) instanceof DefaultFunctionApplication) {
				return symbolIsAnArgumentOfFunction(arg, (DefaultFunctionApplication)f.get(i));
			}
		}
		return false;
	}

	/**
	 * Differentiate a sum or substraction of n terms : a1+a2+...+an or a1-a2-...-an or -a1
	 * 
	 */
	public Expression differentiateSumOrSubstraction(Expression f, Expression arg) {
		if (f.numberOfArguments() > 0) {
			Expression result = f;
			for (int i = 0; i < f.numberOfArguments(); i++) {
				Expression newIthArgument = differentiateExpression(result.get(i), arg);
				result = result.set(i, newIthArgument);
			}
			return result;
		}
		else { 
			return apply(DERIV, f, arg);
		}
	}
	
	/**
	 * Differentiate a product of n terms: a1*a2*...*an
	 * 
	 */
	public Expression differentiateMultiplication(Expression f, Expression arg) {
		if (f.numberOfArguments() > 1) {
			Expression result = apply(PLUS,f.getArguments());
			for (int i = 0; i < f.numberOfArguments(); i++) {
				Expression argument_i = apply(TIMES,f.getArguments());
				argument_i = argument_i.set(i, differentiateExpression(argument_i.get(i), arg));
				result = result.set(i, argument_i);
			}
			return result;
		}
		else { 
			return apply(DERIV, f, arg);
		}
	}
	
	/**
	 * Differentiate u/v
	 * 
	 */
	public Expression differentiateDivision(Expression f, Expression arg) {
		if (f.numberOfArguments() == 2) {
			
			Expression u = f.get(0);
			Expression v = f.get(1);
			
			Expression uPrime = differentiateExpression(u,arg);
			Expression vPrime = differentiateExpression(v,arg);
			
			Expression uVPrime = apply(TIMES, u, vPrime);
			Expression uPrimeV = apply(TIMES, uPrime, v);
			
			Expression uPrimeVMinusuVPrime = apply(MINUS, uPrimeV, uVPrime);
			
			Expression vSquare = apply(TIMES,v, v);
			
			Expression result = apply(DIVISION, uPrimeVMinusuVPrime, vSquare);
			
			return result;
		}
		else { 
			return Expressions.apply(DERIV, f, arg);
		}
	}
	
	/**
	 * Differentiate u^v
	 * 
	 */
	public Expression differentiateExponentiation(Expression f, Expression arg) {
		if (f.numberOfArguments() == 2) {
			
			Expression uExpV = apply(EXPONENTIATION, f.getArguments());
			
			Expression logUDerivV = apply(TIMES,Expressions.apply(LOG,f.get(0)),differentiateExpression(f.get(1),arg));
			Expression vDividedByUDerivU = apply(TIMES, apply(DIVISION, f.get(1), f.get(0)), differentiateExpression(f.get(0),arg));
			Expression logUDerivVPlusVDividedByUDerivU = apply(PLUS, logUDerivV, vDividedByUDerivU);
			
			Expression result = apply(TIMES, uExpV, logUDerivVPlusVDividedByUDerivU );
			
			return result;
		}
		else {
			return apply(DERIV, f, arg);
		}
	}
	
	/**
	 * Is called only when the {@link Expression} arg is not one of the direct arguments of {@link Expression} f. Used to differentiate composed functions.
	 * 
	 */
	public Expression differentiateAnyFunction(Expression f, Expression arg) {
		if (f.numberOfArguments() > 0) {
			Expression result = apply(PLUS,f.getArguments());
			for (int i = 0; i < f.numberOfArguments(); i++) {
				Expression argument_i = apply(TIMES,differentiateExpression(f,f.get(i)),differentiateExpression(f.get(i),arg));
				result = result.set(i, argument_i);
			}
			return result;
		}
		else { 
			return apply(DERIV, f, arg);
		}
	}
	
}
