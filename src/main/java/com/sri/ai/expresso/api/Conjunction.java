package com.sri.ai.expresso.api;

import com.sri.ai.grinder.api.Context;

/**
 * A conjunction of literals onto a specific context with the ability to assert
 * whether it is satisfiable or not.
 * 
 * @author Bobak
 *
 */
public interface Conjunction {
	
	/**
	 * Returns the satisfiability of the stored conjunction given its enclosed context.
	 * 
	 * @return true if the conjunction is satisfiable, false otherwise
	 */
	public boolean isSatisfiable();
	
	/**
	 * Conjoins an {@link Expression} literal to the current conjunction and adds the
	 * appropriate constraint to the enclosed context.
	 * @param context is an external {@link Context} object from which literal symbols and types can be determined.
	 * @param expressionLiteral is the {@link Expression} literal to be conjoined to the current conjunction
	 * 
	 * @return newly formed conjunction with updated internal context.  
	 */
	public Conjunction and(Expression literal, Context context);
	
}
