package com.sri.ai.expresso.smt.core;

import com.sri.ai.expresso.smt.api.SMTSolver;
import com.sri.ai.expresso.smt.api.SMTType;

public abstract class AbstractSMTType implements SMTType {
	
	// DATA MEMBERS
	//////////////////////////////////////////////////////	
	protected final Object smtType;
	protected final SMTSolver smtSolver;
	
	
	
	// CONSTRUCTORS 
	//////////////////////////////////////////////////////
	public AbstractSMTType(Object smtType, SMTSolver expressoToSMTUtil) {
		this.smtSolver = expressoToSMTUtil;
		this.smtType = smtType;
	}


	
	// OVERRIDING INHERITED METHODS : SMTType 
	//////////////////////////////////////////////////////
	@Override
	public Object getEmbeddedSMTSolverObject() {
		return smtType;
	}
	

}
