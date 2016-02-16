package com.sri.ai.grinder.sgdpll.api;

import java.util.Collection;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.PrologConstantPredicate;

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
	Expression solve(Collection<Expression> indices, Constraint constraint, Expression body, Context process);
	
	/**
	 * Returns a true constraint for a problem with given indices.
	 * This is used by the default implementation of {@link #solve(Expression, Collection, Context).
	 * @param indices
	 * @return
	 */
	Constraint makeTrueConstraint(Collection<Expression> indices);
	
	/**
	 * Convenience substitute for {@link #solve(Expression, Constraint, Collection, Context)}
	 * assuming a true contextual constraint.
	 */
	default Expression solve(Expression input, Collection<Expression> indices, Context process) {
		Constraint constraint = makeTrueConstraint(indices);
		Expression result = solve(indices, constraint, input, process);
		return result;
	}

	void interrupt();
	
	boolean getDebug();

	void setDebug(boolean newValue);
	
	// Convenience:
	
	/**
	 * Makes an appropriate rewriting process with the given data for SGDPLL2, which does not require a contextual constraint.
	 * @param mapFromSymbolNameToTypeName
	 * @param mapFromCategoricalTypeNameToSizeString
	 * @param additionalTypes
	 * @param isUniquelyNamedConstantPredicate
	 * @return
	 */
	Context makeProcess(
			Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate);
	
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
			Predicate<Expression> isUniquelyNamedConstantPredicate) {
		
		Context process =
				makeProcess(
						mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString,
						additionalTypes, isUniquelyNamedConstantPredicate);
		
		Expression result = solve(expression, indices, process);
		return result;
	}

	/**
	 * Convenience substitute for {@link #solve(Expression, Collection, Context)} that takes care of constructing the Context
	 * given the data required to build it.
	 */
	default Expression solve(
			Expression expression,
			Collection<Expression> indices,
			Map<String, String> mapFromVariableNameToTypeName, 
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes) {
		
		return solve(expression, indices, mapFromVariableNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes, new PrologConstantPredicate());
	}
}