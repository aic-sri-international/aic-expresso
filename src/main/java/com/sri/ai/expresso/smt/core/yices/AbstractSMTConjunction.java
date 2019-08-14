package com.sri.ai.expresso.smt.core.yices;

import com.sri.ai.expresso.smt.api.SMTConjunction;
import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.expresso.smt.api.SMTFormula;

public abstract class AbstractSMTConjunction implements SMTConjunction {
	
	
	// DATA MEMBERS
	//////////////////////////////////////////////////////	
	protected final SMTContext smtContext;
	

	
	
	// CONSTRUCTORS 
	//////////////////////////////////////////////////////
	public AbstractSMTConjunction(SMTContext smtContext) {
		this.smtContext = smtContext;
	}

	
	
	    
	// OVERRIDING INHERITED METHODS : Conjunction 
	//////////////////////////////////////////////////////
	@Override
	public boolean isSatisfiable() {
		return smtContextIsSatisfiable();
	}
	
	
	
	
	// OVERRIDING INHERITED METHODS : SMTConjunction 
	//////////////////////////////////////////////////////
	@Override
	public SMTConjunction backtrack() {
		smtContext.popStackFrame();
		return this;
	}
	
	@Override
	public String getModel() {
		String model = smtContext.getModel();
		return model;
	}
	
	
	
	
	// INHERITABLE HELPER METHODS 
	//////////////////////////////////////////////////////
	protected void conjoinSMTFormulaToContextOnANewStackFrame(SMTFormula smtFormula) {
		smtContext.pushStackFrame();
		smtContext.assertOntoContext(smtFormula);
	}

	
	
	
	// PRIVATE HELPER METHODS
	//////////////////////////////////////////////////////	
	private boolean smtContextIsSatisfiable() {
		boolean contextIsSatisfiable = smtContext.contextIsSatisfiable();
		return contextIsSatisfiable;
	}

}
