package com.sri.ai.grinder.sgdpll.api;

import java.util.Collection;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;

/**
 * Interface to classes able to eliminate quantification (for a fixed group) over given indices, constraint and body.
 * 
 * @author braz
 *
 */
public interface OldStyleQuantifierEliminator {

	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices and a constraint on them
	 */
	Expression solve(Collection<Expression> indices, Constraint constraint, Expression body, Context context);
	
	/**
	 * Convenience substitute for {@link #solve(Expression, Constraint, Collection, Context)}
	 * assuming a true context.
	 */
	default Expression solve(Expression input, Collection<Expression> indices, Context context) {
		Expression result = solve(indices, context, input, context);
		return result;
	}

	void interrupt();
	
	boolean getDebug();

	void setDebug(boolean newValue);
	
	// Convenience:
	
	/**
	 * Makes an appropriate context with the given data for SGDPLL2, which does not require a context.
	 * @param mapFromSymbolNameToTypeName
	 * @param mapFromCategoricalTypeNameToSizeString
	 * @param additionalTypes
	 * @param isUniquelyNamedConstantPredicate
	 * @param constraintTheory TODO
	 * @return
	 */
	Context makeProcess(
			Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate, ConstraintTheory constraintTheory);
	
	/**
	 * Convenience substitute for {@link #solve(Expression, Collection, Context)} that takes care of constructing the Context
	 * given the data required to build it.
	 * @param constraintTheory TODO
	 */
	default Expression solve(
			Expression expression, 
			Collection<Expression> indices,
			Map<String, String> mapFromSymbolNameToTypeName, 
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes, 
			Predicate<Expression> isUniquelyNamedConstantPredicate, 
			ConstraintTheory constraintTheory) {
		
		Context context =
				makeProcess(
						mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString,
						additionalTypes, isUniquelyNamedConstantPredicate, constraintTheory);
		
		Expression result = solve(expression, indices, context);
		return result;
	}
}