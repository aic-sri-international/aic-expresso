package com.sri.ai.grinder.helper;

import static com.sri.ai.util.Util.mapIntoList;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.IfRewriter;

/**
 * A {@link TopRewriter} that
 * rewrites an expression to its value if it is
 * a {@link LazySampledFunction} application,
 * or delegates its rewriting to a given base {@link TopRewriter} otherwise.
 *
 * @author braz
 *
 */
public class LazySampledFunctionApplicationTopRewriter extends IfRewriter {
	
	/**
	 * Constructor receives
	 * a {@link Rewriter} that evaluates the arguments of the {@link LazySampledFunction} applications,
	 * and a base {@link TopRewriter}, which processes all expressions
	 * that are not an application of a {@link LazySampledFunction}.
	 * @param argumentsEvaluator
	 * @param baseTopRewriter
	 */
	public LazySampledFunctionApplicationTopRewriter(Rewriter argumentsEvaluator, TopRewriter baseTopRewriter) {
		super(
				(Expression e) -> e.getFunctor() instanceof LazySampledFunction,

				(Simplifier) (e, c) -> evaluateArgumentsAndGetValueOfFunctionApplication(e, argumentsEvaluator, c),
				
				baseTopRewriter
		);
	}

	private static Expression evaluateArgumentsAndGetValueOfFunctionApplication(Expression lazySampledFunctionApplication, Rewriter evaluator, Context c) {
		LazySampledFunction lazySampledFunctor = (LazySampledFunction) lazySampledFunctionApplication.getFunctor();
		List<Expression> argumentValues = mapIntoList(lazySampledFunctionApplication.getArguments(), a -> evaluator.apply(a, c));
		Expression result = lazySampledFunctor.sampleApplication(argumentValues);
		return result;
	}
}