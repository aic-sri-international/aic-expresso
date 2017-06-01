package com.sri.ai.grinder.sgdpllt.api;

import static com.sri.ai.expresso.api.Tuple.tuple;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.core.SGDPLLTUtil;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.util.base.Triple;

/**
 * Interface to classes able to eliminate quantification (for a fixed group) over given indices, constraint and body.
 * 
 * @author braz
 *
 */
public interface MultiIndexQuantifierEliminator {

	default Expression solve(
			AssociativeCommutativeGroup group,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression indicesCondition,
			Expression body,
			Context context) {

		Triple<Context, ExtensionalIndexExpressionsSet, Expression> extension
		= context.extendWith(indexExpressions, tuple(indicesCondition, body));
		context = extension.first;
		indexExpressions = extension.second;
		indicesCondition = extension.third.get(0);
		body = extension.third.get(1);
		
//		context = context.extendWith(indexExpressions);
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		Expression quantifierFreeExpression = solve(group, indices, indicesCondition, body, context);
		return quantifierFreeExpression;
	}
	
	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices and a constraint on them
	 */
	Expression solve(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesConstraint, Expression body, Context context);
	
	/**
	 * Convenience substitute for {@link #solve(AssociativeCommutativeGroup, Expression, Expression, Collection, Context)}
	 * assuming a true constraint.
	 */
	default Expression solve(AssociativeCommutativeGroup group, List<Expression> indices, Expression body, Context context) {
		Constraint trueConstraint = context.getTheory().makeTrueConstraint();
		Expression result = solve(group, indices, trueConstraint, body, context);
		return result;
	}

	void interrupt();
	
	boolean getDebug();

	void setDebug(boolean newValue);
	
	// Convenience:
	
	/**
	 * Convenience substitute for {@link #solve(AssociativeCommutativeGroup, Collection, Expression, Context)} that takes care of constructing the Context
	 * given the data required to build it.
	 */
	default Expression solve(
			AssociativeCommutativeGroup group, 
			Expression expression,
			List<Expression> indices, 
			Map<String, String> mapFromSymbolNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString, 
			Collection<Type> additionalTypes, 
			Predicate<Expression> isUniquelyNamedConstantPredicate, Theory theory) {
		
		Context context =
				SGDPLLTUtil.makeContext(
				mapFromSymbolNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes,
				isUniquelyNamedConstantPredicate,
				theory);
		
		Expression result = solve(group, indices, expression, context);
		return result;
	}
}