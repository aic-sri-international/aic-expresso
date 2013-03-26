package com.sri.ai.grinder.library;

import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.controlflow.IfThenElse;

/**
 * A class providing a static method for substituting symbols or function applications in an expression
 * by another expression (not subject itself to the same substitution).
 * 
 * Replacing a symbol works as one would expect:
 * 
 * Replacing <code>X</code> by 99 in <code>foo</code> returns <code>foo</code>
 * Replacing <code>X</code> by 99 in <code>X</code> returns <code>99</code>
 * 
 * However, quantifications need to be taken into account:
 * replacing <code>X</code> by 99 in <code>X + if there exists X : X = 9 then X else 0</code> returns <code>99 + if there exists X : X = 9 then 99 else 0</code>
 * because the quantified X works as a distinct variable. Its scope is limited to the if's condition,
 * so the X in the then branch corresponds to the original X.
 * 
 * When replacing a function substitution, the function's corresponding "cell" has its value replaced.
 * Here are some examples:
 * 
 * Replacing <code>f(10)</code> by 99 in <code>f(9)</code> returns <code>f(9)</code>
 * Replacing <code>f(10)</code> by 99 in <code>f(X)</code> returns <code>if X = 10 then 99 else f(X)</code>
 * Replacing <code>f(Y)</code> by 99 in <code>f(X)</code> returns <code>if X = Y then 99 else f(X)</code>
 * Replacing <code>f(Y)</code> by 99 in <code>f(Y)</code> returns <code>99</code>
 * 
 * Quantification for function applications extends the symbol case, but may seem more confusing:
 * 
 * Replacing <code>age(bob)</code> by 99 in <code>age(bob) + if there exists age(Y) : age(Y) = age(bob) then 1 else 0</code>
 * returns <code>99 + if Y = bob then (if there exists age(Y) : age(Y) = age(bob) then 1 else 0) else (if there exists age(Y) : age(Y) = 99 then 1 else 0)</code>
 * 
 * The above is justified in the following way: we know that the age of bob is 99, so we replace the first <code>age(bob)</code> by 99.
 * The there exists quantification is asking the question of whether there is a possible age for a person Y (which may or may not be bob) satisfying a condition.
 * If Y = bob, then the quantification is essentially defining a "local", distinct age(bob) (which is also age(Y)), so we do <i>not</i> substitute age(bob) by anything in that context
 * (because age(bob) inside that context is a distinct, new, local age(bob), much in the same way we have locally quantified symbols).
 * If Y != bob, however, "local" age(Y) is not the same as age(bob) and we do proceed with the substitution of age(bob) under that context.
 *
 * @author braz
 *
 */
public class NewSubstitute {

	private static Expression substitute(Expression replaced, Expression replacement, Expression expression, int nextScopedVariableIndex, RewritingProcess process) {
		System.out.println("Substitute: " + replaced + ", " + replacement + " in " + expression + ", with nextScopedVariableIndex " + nextScopedVariableIndex);
		return expression.replaceAllOccurrences(new Replace(replaced, replacement, nextScopedVariableIndex), process);
	}
	
	public static Expression substitute(Expression replaced, Expression replacement, Expression expression, RewritingProcess process) {
		return substitute(replaced, replacement, expression, 0, process);
	}
	
	public static Expression replaceAll(Expression expression,
			Map<? extends Expression, ? extends Expression> replacements,
			RewritingProcess process) {
		for (Map.Entry<? extends Expression, ? extends Expression> entry : replacements.entrySet()) {
			expression = substitute(entry.getKey(), entry.getValue(), expression, process);
		}
		return expression;
	}

	private static class Replace implements ReplacementFunctionWithContextuallyUpdatedProcess {

		private Expression replaced;
		private Expression replacement;
		private int nextScopedVariableIndex;
		
		public Replace(Expression replaced, Expression replacement, int nextScopedVariableIndex) {
			this.replaced = replaced;
			this.replacement = replacement;
			this.nextScopedVariableIndex = nextScopedVariableIndex;
		}
		
		public Replace(Expression replaced, Expression replacement) {
			this(replaced, replacement, 0);
		}
		
		@Override
		public Expression apply(Expression arg0) {
			throw new Error(Replace.class + ".apply(Expression) must not be invoked");
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			Expression result = null;
			List<Expression> scopedVariables = ScopedVariables.get(expression, process);
			if (scopedVariables != null && ! scopedVariables.isEmpty()) {
				result = replaceQuantifiedExpression(expression, scopedVariables, process);
			}
			else if (expression.getFunctorOrSymbol().equals(replaced.getFunctorOrSymbol())) {
				result = replaceNonQuantifiedExpression(expression, process);
			}
			return result;
		}

		private Expression replaceQuantifiedExpression(Expression expression, List<Expression> scopedVariables, RewritingProcess process) {
			Expression result = expression;
			Expression nextScopedVariable = scopedVariables.get(nextScopedVariableIndex);
			if (nextScopedVariable.getFunctorOrSymbol().equals(replaced.getFunctorOrSymbol())) {
				Expression conditionForScopedVariableToRedefineReplaced = Equality.makePairwiseEquality(nextScopedVariable.getArguments(), replaced.getArguments());
				result = replaceIfConditionHolds(conditionForScopedVariableToRedefineReplaced, expression, nextScopedVariableIndex + 1, process);
			}
			return result;
		}

		private Expression replaceNonQuantifiedExpression(Expression expression, RewritingProcess process) {
			Expression conditionForExpressionToMatchReplaced = Equality.makePairwiseEquality(expression.getArguments(), replaced.getArguments());
			Expression result = replaceIfConditionHolds(conditionForExpressionToMatchReplaced, expression, 1, process);
			return result;
		}

		private Expression replaceIfConditionHolds(Expression condition, Expression expression, int indexOfWhichScopedVariableToConsiderIfAnyAreFound, RewritingProcess process) {
			Expression result = null;
			if (condition.equals(Expressions.FALSE)) {
				result = expression;
			}
			else {
				RewritingProcess newProcess = GrinderUtil.extendContextualConstraint(condition, process);
				Expression replacementIfConditionHolds = substitute(replaced, replacement, replacement, indexOfWhichScopedVariableToConsiderIfAnyAreFound, newProcess);
				result = IfThenElse.make(condition, replacementIfConditionHolds, expression);
			}
			return result;
		}
	}
}
