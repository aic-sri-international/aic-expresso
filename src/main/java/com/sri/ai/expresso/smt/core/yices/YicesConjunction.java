package com.sri.ai.expresso.smt.core.yices;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.core.AbstractSMTConjunction;

/**
 * TODO currently mutable via {@link #and(Expression,Context)} method
 * as this method mutates the enclosed Yices context object
 * 
 * @author Bobak
 *
 */
public class YicesConjunction extends AbstractSMTConjunction {
	
	
	// CONSTRUCTORS
	//////////////////////////////////////////////////////
	public YicesConjunction() {
		super(new YicesContext(), Yices.EXPRESSO_TO_YICES_UTIL);
	}
	
	public YicesConjunction(Expression literal, Context context) {
		super(literal, context, new YicesContext(), Yices.EXPRESSO_TO_YICES_UTIL);
	}

}
