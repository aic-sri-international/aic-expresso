package com.sri.ai.grinder.sgdpllt.interpreter;

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
import com.sri.ai.util.collect.StackedHashMap;

/**
 * An abstract class for quantifier eliminators using a simple (total or sampled)
 * iteration over the domain of the eliminated variables.
 * <p>
 * The current assignment to the eliminated variables is kept in the {@link Context}'s
 * global object with key {@link #ASSIGNMENTS_GLOBAL_OBJECTS_KEY},
 * which can be extended with {@link #extendAssignments(Map, Context)}.
 * This same assignment is also used by top rewriters in implementations of
 * {@link AbstractInterpreter} to simplify variables.
 * <p>
 * 
 * @author braz
 *
 */
public abstract class AbstractIterativeMultiIndexQuantifierEliminator extends AbstractMultiIndexQuantifierEliminator {

	protected TopRewriterUsingContextAssignments topRewriterUsingContextAssignments;

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

	public AbstractIterativeMultiIndexQuantifierEliminator(TopRewriter topRewriter) {
		this(new TopRewriterUsingContextAssignmentsReceivingBaseTopRewriterAtConstruction(topRewriter));
	}

	public AbstractIterativeMultiIndexQuantifierEliminator(TopRewriterUsingContextAssignments topRewriterUsingContextAssignments) {
		super();
		this.topRewriterUsingContextAssignments = topRewriterUsingContextAssignments;
	}

	public TopRewriterUsingContextAssignments getTopRewriterUsingContextAssignments() {
		return topRewriterUsingContextAssignments;
	}
	
	@Override
	public Expression solve(AssociativeCommutativeGroup group, ExtensionalIndexExpressionsSet indexExpressions, Expression indicesCondition, Expression body, Context context) throws Error {
		context = context.extendWith(indexExpressions);
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		return solve(group, indices, indicesCondition, body, context);
	}

	public Expression solve(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesCondition, Expression body, Context context) {
		Expression result   = group.additiveIdentityElement();		
		Expression summand  = makeSummand(group, indices, indicesCondition, body, context);
		Rewriter   rewriter = new Recursive(new Exhaustive(getTopRewriterUsingContextAssignments()));
		Iterator<Map<Expression, Expression>> assignmentsIterator = makeAssignmentsIterator(indices, indicesCondition, context);
		for (Map<Expression, Expression> indicesValues : in(assignmentsIterator)) {
			Context extendedContext = extendAssignments(indicesValues, context);			
			Expression bodyEvaluation = rewriter.apply(summand, extendedContext);
			if (group.isAdditiveAbsorbingElement(bodyEvaluation)) {
				return bodyEvaluation;
			}
			result = group.add(result, bodyEvaluation, extendedContext);
		}
		return result;
	}
	
	private static final String ASSIGNMENTS_GLOBAL_OBJECTS_KEY = "ASSIGNMENTS_GLOBAL_OBJECTS_KEY";
	
	/**
	 * Obtains the value assignment to a given expression in the binding mechanism stored in the context.
	 * @param expression
	 * @param context
	 * @return
	 */
	public static Expression getAssignedValue(Expression expression, Context context) {
		@SuppressWarnings("unchecked")
		Map<Expression, Expression> assignments = (Map<Expression, Expression>) context.getGlobalObject(ASSIGNMENTS_GLOBAL_OBJECTS_KEY);
		Expression result = assignments == null? null : assignments.get(expression);
		return result;
	}
	
	/**
	 * Sets the value assignment to a given expression in the binding mechanism stored in the context.
	 * @param newAssignment
	 * @param context
	 * @return
	 */
	public static Context extendAssignments(Map<Expression, Expression> newAssignments, Context context) {
		@SuppressWarnings("unchecked")
		Map<Expression, Expression> assignments = (Map<Expression, Expression>) context.getGlobalObject(ASSIGNMENTS_GLOBAL_OBJECTS_KEY);
		Map<Expression, Expression> extendedAssignments;
		if (assignments == null) {
			extendedAssignments = newAssignments;
		}
		else {
			extendedAssignments = new StackedHashMap<>(newAssignments, assignments);
		}
		Context result = context.putGlobalObject(ASSIGNMENTS_GLOBAL_OBJECTS_KEY, extendedAssignments);
		return result;
	}
}