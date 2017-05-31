package com.sri.ai.grinder.helper;

import java.io.Serializable;
import java.util.Set;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Registry;

/**
 * A predicate indicating that uniquely named constants are all symbols not in a given collection.
 * 
 * This predicate is useful for providing a predicate to {@link Registry#setIsUniquelyNamedConstantPredicate(Predicate)}
 * for registries and contexts if we want to select a specific set of symbols as variables.
 * 
 * @author braz
 *
 */
public class UniquelyNamedConstantAreAllSymbolsNotIn implements Predicate<Expression>, Serializable {
	private static final long serialVersionUID = 1L;
	
	private Set<Expression> notUniquelyNamedConstants;
	
	public UniquelyNamedConstantAreAllSymbolsNotIn(Set<Expression> notUniquelyNamedConstants) {
		this.notUniquelyNamedConstants = notUniquelyNamedConstants;
	}
	
	@Override
	public boolean apply(Expression e) {
		boolean result = e.getSyntacticFormType().equals("Symbol") && !notUniquelyNamedConstants.contains(e);
		return result;
	}
}