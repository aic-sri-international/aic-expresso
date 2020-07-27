package com.sri.ai.grinder.interpreter;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Map;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.helper.UniquelyNamedConstantIncludingBooleansAndNumbersPredicate;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

/**
 * A hard-coded interpreter faster than {@link BruteForceCommonInterpreter}.
 * Currently supports only a sub-set of operations.
 * 
 * @author braz
 *
 */
public class HardCodedInterpreter implements BinaryFunction<Expression, Context, Expression> {

	/**
	 * Gives a chance to extending classes to obtain result of expression without
	 * evaluating it (typically through some kind of caching).
	 * If it returns null, the result will be evaluated as usual.
	 *
	 * @param functorOperation
	 * @param expression
	 * @param context
	 * @return
	 */
	protected Expression beforeEvaluate(
			Function<ArrayList<Expression>, Expression> functorOperation,
			Expression expression,
			Context context) {

		return null;
	}

	protected
	void
	afterEvaluate(
			Function<ArrayList<Expression>, Expression> functorOperation,
			Expression expression,
			Context context,
			Expression resultingValue) {
	}

	private Map<String, Function<ArrayList<Expression>, Expression>> functorOperations;

	public HardCodedInterpreter() {
		functorOperations = map();
		functorOperations.put("+", this::evaluateAddition);
		functorOperations.put("and", this::evaluateAnd);
		functorOperations.put("or", this::evaluateOr);
		functorOperations.put(IF_THEN_ELSE, this::evaluateIfThenElse);
		functorOperations.put("<", this::evaluateLessThan);
		functorOperations.put(">", this::evaluateGreaterThan);
		functorOperations.put("<=", this::evaluateLessThanOrEqualTo);
		functorOperations.put(">=", this::evaluateGreaterThanOrEqualTo);
		functorOperations.put("=", this::evaluateEqualTo);
		functorOperations.put("!=", this::evaluateNotEqualTo);
	}
	
	@Override
	public Expression apply(Expression expression, Context context) {
		if (expression.getSyntacticFormType().equals("Symbol")) {
			return evaluateSymbol(expression, context);
		}
		else {
			return evaluateFunctionApplication(expression, context);
		}
	}

	private Expression evaluateSymbol(Expression expression, Context context) {
		if (context.isVariable(expression)) {
			return evaluateVariable(expression, context);
		}
		else {
			return expression;
		}
	}

	protected Expression evaluateVariable(Expression expression, Context context) {
		var value = ContextAssignmentLookup.getAssignedValue(expression, context);
		myAssert(value != null, () -> expression + " has no assigned value");
		return value;
	}

	private Expression evaluateFunctionApplication(Expression expression, Context context) {
		var functorOperation = obtainFunctorOperation(expression);
		var result = evaluate(functorOperation, expression, context);
		return result;
	}

	private Function<ArrayList<Expression>, Expression> obtainFunctorOperation(Expression expression) {
		var functorString = expression.getFunctor().getValue();
		var functorOperation = functorOperations.get(functorString);
		myAssert(functorOperation != null, () -> "Undefined operator: " + functorString + " from " + expression);
		return functorOperation;
	}

	private
	Expression
	evaluate(
			Function<ArrayList<Expression>, Expression> functorOperation,
			Expression expression,
			Context context) {

		var resultingValue = beforeEvaluate(functorOperation, expression, context);
		if (resultingValue == null) {
			var evaluatedArguments = mapIntoArrayList(expression.getArguments(), a -> apply(a, context));
			resultingValue = functorOperation.apply(evaluatedArguments);
		}
		afterEvaluate(functorOperation, expression, context, resultingValue);
		return resultingValue;
	}

	private Symbol evaluateAddition(ArrayList<Expression> evaluatedArguments) {
		var numericValues = mapIntoList(evaluatedArguments, a -> a.doubleValue());
		var resultingValue = Util.sum(numericValues);
		var result = makeSymbol(resultingValue);
		return result;
	}
	
	private Symbol evaluateAnd(ArrayList<Expression> evaluatedArguments) {
		var booleanValues = mapIntoList(evaluatedArguments, a -> a.booleanValue());
		var resultingValue = Util.and(booleanValues);
		var result = makeSymbol(resultingValue);
		return result;
	}
	
	private Symbol evaluateOr(ArrayList<Expression> evaluatedArguments) {
		var booleanValues = mapIntoList(evaluatedArguments, a -> a.booleanValue());
		var resultingValue = Util.or(booleanValues);
		var result = makeSymbol(resultingValue);
		return result;
	}
	
	private Expression evaluateIfThenElse(ArrayList<Expression> evaluatedArguments) {
		myAssert(evaluatedArguments.size() == 3, () -> "if then else requires exactly three arguments but got: " + evaluatedArguments);
		var branchIndex = evaluatedArguments.get(0).booleanValue()? 1 : 2;
		var result = evaluatedArguments.get(branchIndex);
		return result;
	}
	
	private Symbol evaluateLessThan(ArrayList<Expression> evaluatedArguments) {
		return evaluateRelationalOperatorOnNumbers((d1, d2) -> d1.doubleValue() < d2.doubleValue(), "<", evaluatedArguments);
	}

	private Symbol evaluateGreaterThan(ArrayList<Expression> evaluatedArguments) {
		return evaluateRelationalOperatorOnNumbers((d1, d2) -> d1.doubleValue() > d2.doubleValue(), ">", evaluatedArguments);
	}

	private Symbol evaluateLessThanOrEqualTo(ArrayList<Expression> evaluatedArguments) {
		return evaluateRelationalOperatorOnNumbers((d1, d2) -> d1.doubleValue() <= d2.doubleValue(), "<=", evaluatedArguments);
	}

	private Symbol evaluateGreaterThanOrEqualTo(ArrayList<Expression> evaluatedArguments) {
		return evaluateRelationalOperatorOnNumbers((d1, d2) -> d1.doubleValue() >= d2.doubleValue(), ">=", evaluatedArguments);
	}

	private Symbol evaluateEqualTo(ArrayList<Expression> evaluatedArguments) {
		return evaluateRelationalOperatorOnObjects((d1, d2) -> d1 == d2, "=", evaluatedArguments);
	}

	private Symbol evaluateNotEqualTo(ArrayList<Expression> evaluatedArguments) {
		return evaluateRelationalOperatorOnObjects((d1, d2) -> d1 != d2, "!=", evaluatedArguments);
	}

	private Symbol evaluateRelationalOperatorOnNumbers(
			BinaryFunction<Number, Number, Boolean> binaryOperator,
			String operatorName,
			ArrayList<Expression> evaluatedArguments) {

		myAssert(evaluatedArguments.size() == 2, () -> operatorName + " applied to more than two arguments: " + evaluatedArguments);
		ArrayList<Number> numericValues = mapIntoArrayList(evaluatedArguments, a -> (Number) a.getValue());
		var resultingValue = binaryOperator.apply(numericValues.get(0), numericValues.get(1));
		var result = makeSymbol(resultingValue);
		return result;
	}

	private Symbol evaluateRelationalOperatorOnObjects(
			BinaryFunction<Object, Object, Boolean> binaryOperator,
			String operatorName,
			ArrayList<Expression> evaluatedArguments) {

		myAssert(evaluatedArguments.size() == 2, () -> operatorName + " applied to more than two arguments: " + evaluatedArguments);
		ArrayList<Object> objectValues = mapIntoArrayList(evaluatedArguments, a -> a.getValue());
		var resultingValue = binaryOperator.apply(objectValues.get(0), objectValues.get(1));
		var result = makeSymbol(resultingValue);
		return result;
	}

	public static void main(String[] args) {
		Map<Expression, Expression> assignments = map(parse("y"), makeSymbol(10), parse("x"), makeSymbol(2));
		var expression = Expressions.parse(
				"x + (2 + y) + "
				+ "(if x = y then 0 else 100) + "
				+ "(if x < 4 and y > 8 and x <= 2 and y >= 10 and x != y then 1000 else 0)");
		Context context = new TrueContext();
		context = context.setIsUniquelyNamedConstantPredicate(new UniquelyNamedConstantIncludingBooleansAndNumbersPredicate());
		context = ContextAssignmentLookup.setAssignments(context, assignments);
		var fastInterpreter = new HardCodedInterpreter();
		var result = fastInterpreter.apply(expression, context);
		println(result);
	}
}
