package com.sri.ai.grinder.helper;

import java.io.Serializable;
import java.util.Set;

import com.google.common.base.Predicate;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;

/**
 * A predicate based on a set of given uniquely named constants (symbols) that also
 * implicitly include numbers and boolean constants as uniquely named constants.
 * 
 * This predicate is useful for providing a predicate to {@link Registry#setIsUniquelyNamedConstantPredicate(Predicate)}
 * for registries and contexts if we want to select a specific set of symbols as uniquely named constants.
 * 
 * @author braz
 *
 */
public class UniquelyNamedConstantIncludingBooleansAndNumbersPredicate implements Predicate<Expression>, Serializable {
	private static final long serialVersionUID = 1L;
	
	private Set<Expression> uniquelyNamedConstants;
	
	public UniquelyNamedConstantIncludingBooleansAndNumbersPredicate(Set<Expression> uniquelyNamedConstants) {
		this.uniquelyNamedConstants = uniquelyNamedConstants;
	}
	
	@Override
	public boolean apply(Expression e) {
		boolean result = uniquelyNamedConstants.contains(e) || Expressions.isNumber(e) || Expressions.isBooleanSymbol(e);
		return result;
	}
	
}