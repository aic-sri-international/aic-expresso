package com.sri.ai.grinder.sgdpllt.interpreter;

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;

/**
 * 
 * 
 * @author braz
 *
 */
public class DefaultTopRewriterWithAssignment extends TopRewriterWithAssignment {

	public DefaultTopRewriterWithAssignment() {
		this(null, map());
	}
	
	public DefaultTopRewriterWithAssignment(Map<Expression, Expression> assignment) {
		this(null, assignment);
	}
	
	public DefaultTopRewriterWithAssignment(TopRewriter baseTopRewriter) {
		this(baseTopRewriter, map());
	}

	public DefaultTopRewriterWithAssignment(TopRewriter baseTopRewriter, Map<Expression, Expression> assignment) {
		super(assignment);
		setBaseTopRewriter(baseTopRewriter);
	}

	@Override
	public DefaultTopRewriterWithAssignment makeCopyWith(Map<Expression, Expression> newAssignment) {
		DefaultTopRewriterWithAssignment result = new DefaultTopRewriterWithAssignment(getBaseTopRewriter(), newAssignment);
		return result;
	}
}