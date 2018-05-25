package com.sri.ai.grinder.library.number;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.rewriter.api.Simplifier;
import com.sri.ai.util.math.Rational;

/**
 * @author Sarah Perrin
 *
 */
@Beta
public class Exponential implements Simplifier {
	public static final Expression EXPONENTIAL_FUNCTOR = Expressions.makeSymbol(FunctorConstants.EXPONENTIAL);
	
	/**
	 * Makes an exponential function application.
	 * @param expression the expression to exponentiate
	 * @return the exponential function application
	 */
	public static Expression make(Expression expression) {
		Expression plainForm = Expressions.apply(EXPONENTIAL_FUNCTOR, expression);
		Expression result = simplify(plainForm);
		return result;
	}

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression);
	}
	
	
	
	public static Expression simplify(Expression expression) {
		
		Expression base     = Expressions.makeSymbol(Math.E);
		Expression exponent = expression.get(0);

		Rational exponentValue = null;
		Rational baseValue     = null;
		
		if (Expressions.isNumber(exponent)) {
			exponentValue = exponent.rationalValue();
			if (exponentValue.isZero()) {
				return Expressions.ONE;
			}
			if (exponentValue.isOne()) {
				return base;
			}
		}
		
		if (Expressions.isNumber(base)) {
			baseValue = base.rationalValue();
			if (baseValue.isOne()) {
				return Expressions.ONE;
			}
			// we refrain from simplifying 0^x to 0, because x could be 0 itself.
		}
		
		if (baseValue != null && exponentValue != null) {
			Rational   pow    = baseValue.pow(exponentValue);
			Expression result = Expressions.makeSymbol(pow);
			return result;
		}

		return expression;
	}
}