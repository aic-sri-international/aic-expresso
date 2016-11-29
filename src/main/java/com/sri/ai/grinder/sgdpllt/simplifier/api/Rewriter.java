package com.sri.ai.grinder.sgdpllt.simplifier.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver.ItDependsOn;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver.Solution;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver.Step;

/**
 * A rewriter either rewrites a given expression into a simpler form (wrapped in a {@link Solution} object),
 * or indicates a formula it depends on before it can proceed (wrapped in a {@link ItDependsOn} step),
 * taking a context into account,
 * @author braz
 *
 */
public interface Rewriter {
	public Step rewrite(Expression expression, Context context);
}