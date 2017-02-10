package com.sri.ai.grinder.sgdpllt.interpreter;

import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;

/**
 * A convenience variant of {@link TopRewriterUsingContextAssignments} that takes its base top rewriter at construction time.
 * 
 * @author braz
 *
 */
public class TopRewriterUsingContextAssignmentsReceivingBaseTopRewriterAtConstruction extends TopRewriterUsingContextAssignments {

	public TopRewriterUsingContextAssignmentsReceivingBaseTopRewriterAtConstruction(TopRewriter baseTopRewriter) {
		super();
		setBaseRewriter(baseTopRewriter);
	}
}