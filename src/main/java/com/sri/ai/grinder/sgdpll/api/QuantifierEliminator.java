package com.sri.ai.grinder.sgdpll.api;

import java.util.Collection;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.GrinderUtil;

/**
 * Interface to classes able to eliminate quantification (for a fixed group) over given indices, constraint and body.
 * 
 * @author braz
 *
 */
public interface QuantifierEliminator {

	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices and a constraint on them
	 */
	Expression solve(Collection<Expression> indices, Constraint constraint, Expression body, Context context);
	
	/**
	 * Convenience substitute for {@link #solve(Expression, Constraint, Collection, Context)}
	 * assuming a true constraint.
	 */
	default Expression solve(Expression input, Collection<Expression> indices, Context context) {
		Constraint trueConstraint = context.getTheory().makeTrueConstraint();
		Expression result = solve(indices, trueConstraint, input, context);
		return result;
	}

	void interrupt();
	
	boolean getDebug();

	void setDebug(boolean newValue);
	
	// Convenience:
	
	/**
	 * Convenience substitute for {@link #solve(Expression, Collection, Context)} that takes care of constructing the Context
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
				GrinderUtil.makeContext(
				mapFromSymbolNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes,
				isUniquelyNamedConstantPredicate,
				theory);
		
		Expression result = solve(expression, indices, context);
		return result;
	}

	default Context makeProcess(Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate, Theory theory) {
		return GrinderUtil.makeContext(
				mapFromSymbolNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes,
				isUniquelyNamedConstantPredicate, 
				theory) ;
	}
}