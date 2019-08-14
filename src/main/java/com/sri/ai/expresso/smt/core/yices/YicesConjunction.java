package com.sri.ai.expresso.smt.core.yices;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.expresso.api.Conjunction;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.api.SMTFormula;

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
		super(new YicesContext());
	}
	
	public YicesConjunction(Expression literal, Context context) {
		super(new YicesContext());
		SMTFormula smtLiteral = new YicesFormula(literal, context, smtContext);
		conjoinSMTFormulaToContextOnANewStackFrame(smtLiteral);
	}


	

	// OVERRIDING INHERITED METHODS : Conjunction 
	//////////////////////////////////////////////////////
	@Override
	public Conjunction and(Expression literal, Context context) {
		SMTFormula smtLiteral = new YicesFormula(literal, context, smtContext);
		conjoinSMTFormulaToContextOnANewStackFrame(smtLiteral);
		return this;
	}
	
	
	
	

	
}
