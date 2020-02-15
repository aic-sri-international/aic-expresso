package com.sri.ai.expresso.smt.core;

import com.sri.ai.expresso.smt.api.SMTModel;
import com.sri.ai.expresso.smt.api.SMTSolver;

public abstract class AbstractSMTModel implements SMTModel {
	
	// DATA MEMBERS
	//////////////////////////////////////////////////////	
	protected final Object smtModel;
	protected final SMTSolver smtSolver;
	
	
	
	// CONSTRUCTORS 
	//////////////////////////////////////////////////////
	public AbstractSMTModel(Object smtModel, SMTSolver expressoToSMTUtil) {
		this.smtSolver = expressoToSMTUtil;
		this.smtModel = smtModel;
	}


	@Override
	public Object getEmbeddedSMTSolverModelObject() {
		return smtModel;
	}

}
