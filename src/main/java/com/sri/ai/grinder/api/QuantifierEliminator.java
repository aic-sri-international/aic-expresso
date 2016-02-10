package com.sri.ai.grinder.api;

import java.util.Collection;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.core.PrologConstantPredicate;

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
	Expression solve(Collection<Expression> indices, Constraint constraint, Expression body, RewritingProcess process);
	
	/**
	 * Convenience substitute for {@link #solve(Expression, Constraint, Collection, RewritingProcess)}
	 * assuming a true contextual constraint.
	 */
	Expression solve(Expression input, Collection<Expression> indices, RewritingProcess process);
	// TODO: make it a default method

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
	RewritingProcess makeProcess(
			Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate);
	
	/**
	 * Convenience substitute for {@link #solve(Expression, Collection, RewritingProcess)} that takes care of constructing the RewritingProcess
	 * given the data required to build it.
	 */
	default Expression solve(
			Expression expression, 
			Collection<Expression> indices,
			Map<String, String> mapFromSymbolNameToTypeName, 
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes, 
			Predicate<Expression> isUniquelyNamedConstantPredicate) {
		
		RewritingProcess process =
				makeProcess(
						mapFromSymbolNameToTypeName, mapFromCategoricalTypeNameToSizeString,
						additionalTypes, isUniquelyNamedConstantPredicate);
		
		Expression result = solve(expression, indices, process);
		return result;
	}

	/**
	 * Convenience substitute for {@link #solve(Expression, Collection, RewritingProcess)} that takes care of constructing the RewritingProcess
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