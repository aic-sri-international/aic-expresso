package com.sri.ai.grinder.sgdpllt.interpreter;

import static com.sri.ai.grinder.helper.GrinderUtil.extendRegistryWithIndexExpressions;
import static com.sri.ai.util.Util.in;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractMultiIndexQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;

public abstract class AbstractIterativeMultiIndexQuantifierElimination extends AbstractMultiIndexQuantifierEliminator {

	protected TopRewriterWithAssignment topRewriterWithBaseAssignment;

	/**
	 * Make the term to be summed for all assignments provided by assignments iterator.
	 * @param group
	 * @param indices 
	 * @param indicesCondition
	 * @param body
	 * @param context 
	 * @return
	 */
	public abstract Expression makeSummand(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesCondition, Expression body, Context context);

	/**
	 * Makes an iterator (ranging assignments from indices to their values)
	 * that will be used to generate all the terms to be added.
	 * @param indices
	 * @param indicesCondition
	 * @param context
	 * @return
	 */
	public abstract Iterator<Map<Expression, Expression>> makeAssignmentsIterator(List<Expression> indices, Expression indicesCondition, Context context);

	public AbstractIterativeMultiIndexQuantifierElimination(TopRewriter topRewriter) {
		this(new DefaultTopRewriterWithAssignment(topRewriter));
	}

	public AbstractIterativeMultiIndexQuantifierElimination(TopRewriterWithAssignment topRewriterWithBaseAssignment) {
		super();
		this.topRewriterWithBaseAssignment = topRewriterWithBaseAssignment;
	}

	@Override
	public Expression solve(AssociativeCommutativeGroup group, ExtensionalIndexExpressionsSet indexExpressions, Expression indicesCondition, Expression body, Context context) throws Error {
		
		context = (Context) extendRegistryWithIndexExpressions(indexExpressions, context);
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		return solve(group, indices, indicesCondition, body, context);
	}

	public Expression solve(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesCondition, Expression body, Context context) {
		Expression result = group.additiveIdentityElement();
		Expression summand = makeSummand(group, indices, indicesCondition, body, context);
		Iterator<Map<Expression, Expression>> assignmentsIterator = makeAssignmentsIterator(indices, indicesCondition, context);
		for (Map<Expression, Expression> indicesValues : in(assignmentsIterator)) {
			TopRewriterWithAssignment extended = topRewriterWithBaseAssignment.extendWith(indicesValues);
			Rewriter rewriter = new Recursive(new Exhaustive(extended));
			Expression bodyEvaluation = rewriter.apply(summand, context);
			if (group.isAdditiveAbsorbingElement(bodyEvaluation)) {
				return bodyEvaluation;
			}
			result = group.add(result, bodyEvaluation, context);
		}
		return result;
	}
}