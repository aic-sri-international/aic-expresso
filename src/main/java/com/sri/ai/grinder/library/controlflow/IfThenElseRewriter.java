package com.sri.ai.grinder.library.controlflow;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.rewriter.api.Rewriter;

public class IfThenElseRewriter implements Rewriter {

	@Override
	public ExpressionLiteralSplitterStepSolver makeStepSolver(Expression expression) {
		
		return new IfThenElseStepSolver(expression);
	}

}
