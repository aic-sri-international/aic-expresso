package com.sri.ai.grinder.helper;

import static com.sri.ai.util.Util.println;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.rewriter.api.Rewriter;

public class RewriterWithPrinting implements Rewriter {

	private Rewriter baseRewriter;

	public RewriterWithPrinting(Rewriter base) {
		this.baseRewriter = base;
	}
	
	@Override
	public Expression apply(Expression input1, Context input2) {
		println("Going to apply " + baseRewriter + " to " + input1);
		Expression result = baseRewriter.apply(input1, input2);
		println("Finished applying " + baseRewriter + " to " + input1 + " ---> " + result);
		return result;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver makeStepSolver(Expression expression) {
		return baseRewriter.makeStepSolver(expression);
	}
	
}