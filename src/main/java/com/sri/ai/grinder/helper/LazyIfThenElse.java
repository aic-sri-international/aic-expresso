package com.sri.ai.grinder.helper;

import static com.sri.ai.expresso.helper.Expressions.ifThenElse;
import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.interpreter.Assignment;

/** 
 * A lazy expression based on:
 * <ul>
 * <li> a set of variables <code>keys</code> 
 * <li> a {@link Function} <code>maker</code> from {@link Assignment}s
 * (to <code>Keys</code>) to a corresponding sub-{@link Expression}.
 * <li> a {@link Registry} (or more commonly its extension {@link Context}) <code>context</code>
 * </ul>
 * <p>
 * that represents an if-then-else expression
 * testing <code>keys</code> over all possibly assignments <code>assignment</code>
 * according to the <code>context</code>,
 * with then branches formed by the corresponding sub-expression <code>maker.apply(assignment)</code>. 
 * <p>
 * Sub-expression are cached, and therefore made only once.
 * <p>
 * @author braz
 *
 */
public class LazyIfThenElse extends AbstractExpressionWrapper {

	private static final long serialVersionUID = 1L;
	
	//////////////////
	
	private Collection<? extends Expression> keys;
	private Function<Assignment, Expression> maker;
	private Registry context;
	
	//////////////////
	
	public LazyIfThenElse(Collection<? extends Expression> keys, Function<Assignment, Expression> maker, Registry context) {
		this.keys = keys;
		this.maker = maker;
		this.context = context;
	}

	//////////////////
	
	public Collection<? extends Expression> getKeys() {
		return keys;
	}

	public Function<Assignment, Expression> getMaker() {
		return maker;
	}

	public Registry getContext() {
		return context;
	}

	//////////////////
	
	private Map<Assignment, Expression> fromAssignmentToSubExpression = map();
	
	public Expression getSubExpression(Assignment assignment) {
		return getValuePossiblyCreatingIt(fromAssignmentToSubExpression, assignment, a -> maker.apply(a));
	}
	
	//////////////////
	
	@Override
	protected Expression computeInnerExpression() {
		Iterator<Expression> conditionsIterator = functionIterator(makeAssignmentsIterator(), Assignment::makeCondition);
		Iterator<Expression> subExpressionIterator = functionIterator(makeAssignmentsIterator(), this::getSubExpression);
		Expression result = ifThenElse(conditionsIterator, subExpressionIterator);
		return result;
	}
	
	//////////////////
	
	private AssignmentsIterator makeAssignmentsIterator() {
		return new AssignmentsIterator(keys, context);
	}

}
