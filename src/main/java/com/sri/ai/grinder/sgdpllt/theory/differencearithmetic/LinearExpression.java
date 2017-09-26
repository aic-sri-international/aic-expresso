package com.sri.ai.grinder.sgdpllt.theory.differencearithmetic;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PLUS;
import static com.sri.ai.util.Util.accumulateMapValues;
import static com.sri.ai.util.Util.applyFunctionToValuesOfMap;
import static com.sri.ai.util.Util.getValueOrDefault;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.numberInJustNeededType;
import static com.sri.ai.util.Util.sum;

import java.util.LinkedList;
import java.util.Map;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.number.Minus;
import com.sri.ai.grinder.sgdpllt.library.number.Plus;
import com.sri.ai.grinder.sgdpllt.library.number.Times;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

/** 
 * A specialized {@link Expression} representation of a difference arithmetic literal side
 * (valid of the other side is zero)
 * that provides the positive, negative and constant terms in it.
 * @author braz
 *
 */
public class LinearExpression extends AbstractExpressionWrapper {
	
	private static final long serialVersionUID = 1L;

	private Map<Expression, Number> fromVariablesAndConstantOneToCoefficients;
	
	public LinearExpression(Map<Expression, Number> fromVariablesAndConstantOneToCoefficients) {
		this.fromVariablesAndConstantOneToCoefficients = fromVariablesAndConstantOneToCoefficients;
	}
	
	/**
	 * Given a sum, or an expression to be interpreted as a single-term sum,
	 * returns a triple containing a multiset of positive terms, a multiset of negative terms,
	 * and the sum of all numerical constants in it.
	 * If <code>makeDuplicateError</code> is non-null and a duplicate term is found,
	 * the duplicate is passed to it as a parameter and the resulting Error is thrown.
	 * @param expression
	 * @return
	 */
	public static LinearExpression make(Expression expression) throws IllegalLinearExpression {
		LinearExpression result;
		if (expression.getSyntacticFormType().equals("Symbol")) {
			Map<Expression, Number> fromVariablesAndConstantOneToCoefficients = map();
			if (Expressions.isNumber(expression)) {
				Number expressionNumber = (Number)expression.getValue();
				double expressionDouble = expressionNumber.doubleValue();
				Number coefficient = numberInJustNeededType(expressionDouble);
				fromVariablesAndConstantOneToCoefficients.put(ONE, coefficient);
			}
			else {
				fromVariablesAndConstantOneToCoefficients.put(expression, new Integer(1));
			}
			result = new LinearExpression(fromVariablesAndConstantOneToCoefficients);
		}
		else if (expression.hasFunctor(PLUS)) {
			result = null;
			for (Expression argument : expression.getArguments()) {
				LinearExpression argumentLinearExpression = make(argument);
				if (result == null) {
					result = argumentLinearExpression;
				}
				else {
					result = result.add(argumentLinearExpression);
				}
			}
		}
		else if (expression.hasFunctor(FunctorConstants.MINUS) && expression.numberOfArguments() == 1) {
			LinearExpression argumentLinearExpression = make(expression.get(0));
			result = argumentLinearExpression.negate();
		}
		else if (expression.hasFunctor(FunctorConstants.MINUS) && expression.numberOfArguments() == 2) {
			LinearExpression first = make(expression.get(0));
			LinearExpression second = make(expression.get(1));
			result = first.subtract(second);
		}
		else {
			throw new IllegalLinearExpression(expression);
		}
		
		return result;
	}
	
	/** 
	 * Adds this {@link LinearExpression} to another.
	 */
	public LinearExpression add(LinearExpression another) {
		
		LinkedList<Map<Expression, Number>> maps = 
				list(fromVariablesAndConstantOneToCoefficients, 
					another.fromVariablesAndConstantOneToCoefficients);
		Map<Expression, Number> fromVariablesAndConstantOneToCoefficients = accumulateMapValues(maps, sum);
		LinearExpression result = new LinearExpression(fromVariablesAndConstantOneToCoefficients);
		
		return result;
	}
	
	private static final BinaryFunction<Number, Number, Number> sum = 
			(coefficient1, coefficient2) -> sum(list(coefficient1, coefficient2));
	
	/**
	 * Returns the negative of this {@link LinearExpression}.
	 * @return
	 */
	public LinearExpression negate() {
		Map<Expression, Number> fromVariablesAndConstantOneToCoefficients = 
				applyFunctionToValuesOfMap(this.fromVariablesAndConstantOneToCoefficients, negate);
		LinearExpression result = new LinearExpression(fromVariablesAndConstantOneToCoefficients);
		return result;
	}
	
	private static final Integer MINUS_ONE = new Integer(-1);
	
	private static final Function<Number, Number> negate = 
			(coefficient) -> Util.product(list(coefficient, MINUS_ONE));
	
	/** 
	 * Subtract another {@link LinearExpression} from this one,
	 * or throws an exception if invalid.
	 */
	public LinearExpression subtract(LinearExpression another) {
		
		LinearExpression result = this.add(another.negate());
		
		return result;
	}

	public Number getCoefficient(Expression variableOrOne) {
		return fromVariablesAndConstantOneToCoefficients.get(variableOrOne);
	}

	@Override
	protected Expression computeInnerExpression() {
		Expression current = ZERO;
		for (Map.Entry<Expression, Number> entry : fromVariablesAndConstantOneToCoefficients.entrySet()) {
			Expression variableOrOne = entry.getKey();
			Number coefficient = entry.getValue();
			current = addTermAsAdditionOrSubtraction(current, variableOrOne, coefficient);
		}
		Number constantCoefficient = getValueOrDefault(fromVariablesAndConstantOneToCoefficients, ONE, new Integer(0));
		current = addTermAsAdditionOrSubtraction(current, ONE, constantCoefficient);
		return current;
	}

	/**
	 * Adds a new term to an expression as an addition or subtraction depending on the coefficient's sign.
	 * This is done to avoid expressions like X + -Y, which are formed instead as X - Y.
	 * @param current
	 * @param variableOrOne
	 * @param coefficient
	 * @return
	 */
	private static Expression addTermAsAdditionOrSubtraction(Expression current, Expression variableOrOne, Number coefficient) {
		if (coefficient.doubleValue() >= 0) {
			Expression term = Times.make(makeSymbol(coefficient), variableOrOne);
			current = Plus.make(current, term);
		}
		else {
			Number positiveCoefficientNumber = numberInJustNeededType(-coefficient.doubleValue());
			Symbol positiveCoefficientSymbol = makeSymbol(positiveCoefficientNumber);
			Expression term = Times.make(positiveCoefficientSymbol, variableOrOne);
			current = Minus.make(current, term);
		}
		return current;
	}
	
	public static class IllegalLinearExpression extends IllegalArgumentException {
		
		private static final long serialVersionUID = 1L;

		public IllegalLinearExpression(String message) {
			super(message);
		}
		
		public IllegalLinearExpression(Expression illegalLinearExpression) {
			super(illegalLinearExpression + " is not a linear expression");
		}
	}
}