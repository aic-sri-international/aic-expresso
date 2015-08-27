package com.sri.ai.grinder.api;

import static com.sri.ai.util.Util.putAll;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * A Simplifier knows just enough about the symbols in a language to simplify it in a shallow way,
 * that is, to replace function applications by a simpler equivalent expression, if that expression is determined by their immediate arguments.
 * Shallow simplifications are required to take polynomial time in the size of expressions (preferably linear time).
 * <p>
 * Examples of shallow simplifications are <code>x + 0</code> to <code>x</code>, <code>x or true</code> to <code>true</code>, and <code>x + 1 + 3</code> to <code>x + 4</code>.
 * Simplifications that are <i>not</i> shallow include those requiring case analysis (inference), such as <code>(p and q) or (p and not q)</code>leading to <code>p</code>.
 * @author braz
 *
 */
public interface Simplifier {
	/**
	 * Simplifies expression.
	 * @param expression
	 * @param process
	 * @return
	 */
	Expression simplify(Expression expression, RewritingProcess process);

	/**
	 * Simplifies the top expression of an equality-logic-with-quantifiers formula until it cannot be simplified anymore.
	 * Always returns either a symbol or a function application (quantified formulas have their top quantifiers eliminated).
	 * @param expression
	 * @param functionApplicationSimplifiers
	 * @param syntacticFormTypeSimplifiers
	 * @param process
	 * @return
	 */
	static Expression topSimplifyExhaustively(Expression expression, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers, RewritingProcess process) {
		
		Expression previous;
		do {
			expression = topSimplifyOnce(previous = expression, functionApplicationSimplifiers, syntacticFormTypeSimplifiers, process);
		} while (expression != previous);
		
		return expression;
	}

	static Expression topSimplifyOnce(Expression expression, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers, RewritingProcess process) {
		BinaryFunction<Expression, RewritingProcess, Expression> simplifier;
		if (expression.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
			simplifier = functionApplicationSimplifiers.get(expression.getFunctor().getValue());
		}
		else {
			simplifier = syntacticFormTypeSimplifiers.get(expression.getSyntacticFormType());
		}
		
		if (simplifier != null) {
			expression = simplifier.apply(expression, process);
		}
		
		return expression;
	}

	static BinaryFunction<Expression, RewritingProcess, Expression> makeTopExhaustiveSimplifier(Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormSimplifiers) {
		BinaryFunction<Expression, RewritingProcess, Expression>
			topExhaustivelySimplifier =
			(e, p) -> topSimplifyExhaustively(e, functionApplicationSimplifiers, syntacticFormSimplifiers, p);
		return topExhaustivelySimplifier;
	}

	/**
	 * Simplifies an expression based on two maps of simplifiers.
	 * The first map of simplifiers is a map from functor values (Strings) to a binary function taking a function application of that functor and a rewriting process,
	 * and performing a simplification on it (or returning the same instance).
	 * The second map of simplifiers is a map from syntactic type forms (Strings) to a binary function taking an expression of that type and a rewriting process,
	 * and performing a simplification on it (or returning the same instance).
	 * These two maps are then used to create a top exhaustive simplifier
	 * (made with {@link #makeTopExhaustiveSimplifier(Map, Map)}) for use with {@link DPLLUtil#simplify(Expression, BinaryFunction, RewritingProcess).
	 * @param expression
	 * @param functionApplicationSimplifiers
	 * @param syntacticFormSimplifiers
	 * @param process
	 * @return
	 */
	static Expression simplify(Expression expression, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormSimplifiers, RewritingProcess process) {
		BinaryFunction<Expression, RewritingProcess, Expression> topExhaustiveSimplifier = makeTopExhaustiveSimplifier(functionApplicationSimplifiers, syntacticFormSimplifiers);
		Expression result = simplify(expression, topExhaustiveSimplifier, process);
		return result;
	}

	/**
	 * Simplifies an expression by exhaustively simplifying its top expression with given top simplifier, then simplifying its sub-expressions,
	 * and again exhaustively simplifying its top expression.
	 * @param expression
	 * @param topSimplifier
	 * @param process
	 * @return
	 */
	static Expression simplify(
			Expression expression,
			BinaryFunction<Expression, RewritingProcess, Expression> topSimplifier,
			RewritingProcess process) {
		
		Expression result = expression;
		result = topSimplifier.apply(result, process);
		if (result.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
			List<Expression> originalArguments = result.getArguments();
			ArrayList<Expression> simplifiedArguments =
					Util.mapIntoArrayList(originalArguments, e -> simplify(e, topSimplifier, process));
			if ( ! Util.sameInstancesInSameIterableOrder(originalArguments, simplifiedArguments)) { // this check speeds cardinality algorithm by about 25%; it is also required for correctness wrt not returning a new instance that is equal to the input.
				result = Expressions.apply(result.getFunctor(), simplifiedArguments);
			}
			result = topSimplifier.apply(result, process);
		}
	
		return result;
	}

	/**
	 * Simplify an expression given maps of function application and syntactic form type simplifiers,
	 * and an extra simplifier for a given syntactic form type.
	 * @param expression
	 * @param functionApplicationSimplifiers
	 * @param syntacticFormTypeSimplifiers
	 * @param process
	 * @param syntacticTypeForm
	 * @param simplifier
	 * @return
	 */
	static Expression simplifyWithExtraSyntacticFormTypeSimplifiers(
			Expression expression,
			Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers, Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers,
			RewritingProcess process,
			Object... syntacticFormTypesAndBinaryFunctionsFromExpressionRewritingProcessToExpression) {
		
		Map<String, BinaryFunction<Expression, RewritingProcess, Expression>>
		mySyntacticFormTypeSimplifiers = new StackedHashMap<String, BinaryFunction<Expression, RewritingProcess, Expression>>(syntacticFormTypeSimplifiers);
		
		putAll(mySyntacticFormTypeSimplifiers, syntacticFormTypesAndBinaryFunctionsFromExpressionRewritingProcessToExpression);
		
		Expression result = simplify(expression, functionApplicationSimplifiers, mySyntacticFormTypeSimplifiers, process);
		return result;
	}
}