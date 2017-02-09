package com.sri.ai.grinder.sgdpllt.interpreter;

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.FirstOf;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Switch;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * {@link BruteForceMultiIndexQuantifierEliminator} needs a rewriter to evaluate bodies of expressions.
 * It needs this rewriter to consider the current assignment
 * and replace symbols by their values according to it.
 * It also takes a base top rewriter that includes any other desired operations,
 * and to be extensible with a new assignment;
 * {@link BruteForceMultiIndexQuantifierEliminator} will do this at every iteration with a new assignment..
 * {@link TopRewriterWithAssignment} serves as a super class for rewriters of this type.
 * 
 * @author braz
 *
 */
public abstract class TopRewriterWithAssignment implements TopRewriter {

	/** The assignment to use to replace values for symbols. */
	private Map<Expression, Expression> assignment;
	
	private TopRewriter baseTopRewriter;
	
	private Switch<Object> valueReplacer;

	protected TopRewriterWithAssignment(Map<Expression, Expression> assignment) {
		this.baseTopRewriter = null; // delayed setting of baseTopRewriter by extending class
		this.assignment = assignment;
		this.valueReplacer = new Switch<Object>(
				Switch.SYNTACTIC_FORM_TYPE,
				map(
						Symbol.SYNTACTIC_FORM_TYPE,
						(Simplifier) (e, c) -> {
							Expression result = this.assignment.get(e);
							result = AbstractIterativeMultiIndexQuantifierElimination.getAssignedValue(e, c);
							if (result == null) {
								result = e;
							}
							return result;
						}));
	}
	
	public TopRewriter getBaseTopRewriter() {
		return baseTopRewriter;
	}
	
	/** Updates the base top rewriters used. */
	public void setBaseTopRewriter(TopRewriter baseTopRewriter) {
		this.baseTopRewriter = baseTopRewriter;
	}
	
	@Override
	public ExpressionLiteralSplitterStepSolver makeStepSolver(Expression expression) {
		return new FirstOf(valueReplacer, baseTopRewriter).makeStepSolver(expression);
		// we use {@link FirstOf} because it is much cheaper than merging all rewriters every time with {@link DefaultTopRewriter}
		// This is crucial because we extend this rewriter with new assignments at inner loops, so it needs to be fast.
	}
	
	public TopRewriterWithAssignment extendWith(Map<Expression, Expression> moreAssignments)  {
		StackedHashMap<Expression, Expression> extendedAssignment = new StackedHashMap<>(moreAssignments, assignment);
		TopRewriterWithAssignment result = makeCopyWith(extendedAssignment);
		return result;
	}

	/**
	 * Creates a copy of this class with the given assignment.
	 * @param newAssignment
	 * @return
	 */
	public abstract TopRewriterWithAssignment makeCopyWith(Map<Expression, Expression> newAssignment);
}