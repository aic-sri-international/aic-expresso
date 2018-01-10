package com.sri.ai.grinder.rewriter.core;

import static com.sri.ai.util.Util.map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.rewriter.api.TopRewriter;

/**
 * A {@link TopRewriter} that executes one of two base top rewriters
 * based on a predicate.
 * 
 * @author braz
 *
 */
public class IfRewriter extends Switch<Boolean> {
	
	public IfRewriter(Predicate<Expression> test, TopRewriter thenRewriter, TopRewriter elseRewriter) {
		super(
				e -> test.apply(e),
				map(true, thenRewriter, false, elseRewriter));
	}
}