package com.sri.ai.grinder.sgdpllt.api;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.core.SGDPLLTUtil;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.util.base.Pair;

/**
 * Interface to classes able to eliminate quantification (for a fixed group) over given indices, constraint and body.
 * 
 * @author braz
 *
 */
public interface QuantifierEliminator {

	default Expression solve(
			AssociativeCommutativeGroup group,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression indicesCondition,
			Expression body,
			Context context) {

		context = (Context) GrinderUtil.extendRegistryWithIndexExpressions(indexExpressions, context);
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		Expression quantifierFreeExpression = solve(group, indices, indicesCondition, body, context);
		return quantifierFreeExpression;
	}
	
	public AssociativeCommutativeGroup getGroup();
	
	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices and a constraint on them
	 */
	Expression solve(AssociativeCommutativeGroup group, Collection<Expression> indices, Expression indicesConstraint, Expression body, Context context);
	
	/**
	 * Convenience substitute for {@link #solve(AssociativeCommutativeGroup, Expression, Expression, Collection, Context)}
	 * assuming a true constraint.
	 * @param group TODO
	 */
	default Expression solve(AssociativeCommutativeGroup group, Collection<Expression> indices, Expression body, Context context) {
		Constraint trueConstraint = context.getTheory().makeTrueConstraint();
		Expression result = solve(group, indices, trueConstraint, body, context);
		return result;
	}

	/**
	 * Solves a problem encoded in an expression according to
	 * the way this quantifier eliminator's group encodes it.
	 * @param problem
	 * @param context
	 * @return
	 */
	default Expression solve(Expression problem, Context context) {
		Pair<Expression, IndexExpressionsSet> bodyAndIndexExpressionSet
		= getGroup().getExpressionAndIndexExpressionsFromProblemExpression(problem, context);

		Expression body = bodyAndIndexExpressionSet.first;
		IndexExpressionsSet indexExpressions = bodyAndIndexExpressionSet.second;

		context = (Context) GrinderUtil.extendRegistryWithIndexExpressions(indexExpressions, context);

		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		Expression quantifierFreeExpression = solve(getGroup(), indices, body, context);
		return quantifierFreeExpression;
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
			Expression expression, 
			Collection<Expression> indices,
			Map<String, String> mapFromSymbolNameToTypeName, 
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes, 
			Predicate<Expression> isUniquelyNamedConstantPredicate, 
			Theory theory) {
		
		Context context =
				SGDPLLTUtil.makeContext(
				mapFromSymbolNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes,
				isUniquelyNamedConstantPredicate,
				theory);
		
		Expression result = solve(getGroup(), indices, expression, context);
		return result;
	}
}