package com.sri.ai.grinder.sgdpllt.interpreter;

import static com.sri.ai.util.Util.map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.FirstOf;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Switch;
import com.sri.ai.grinder.sgdpllt.rewriter.help.RedirectingRewriter;

/**
 * A {@link TopRewriter} based on another {@link TopRewriter},
 * and adding functionality for replacing expressions by their assignments stored on the context,
 * stored there by {@link AbstractIterativeMultiIndexQuantifierEliminator#extendAssignments(Map<Expression, Expression> newAssignments, Context context)}.
 * 
 * @author braz
 *
 */
public class TopRewriterUsingContextAssignments extends RedirectingRewriter {

	private static final Switch<Object> valueReplacer = new Switch<Object>(
			Switch.SYNTACTIC_FORM_TYPE,
			map(
					Symbol.SYNTACTIC_FORM_TYPE,
					(Simplifier) (e, c) -> {
						Expression result = ContextAssignmentLookup.getAssignedValue(e, c);
						if (result == null) {
							result = e;
						}
						return result;
					}));

	@Override
	public void setBaseRewriter(Rewriter baseTopRewriter) {
		super.setBaseRewriter(new FirstOf("Replacer of assigned value or " + baseTopRewriter, valueReplacer, baseTopRewriter));
	}
}