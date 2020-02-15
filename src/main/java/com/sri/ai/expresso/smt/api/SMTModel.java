package com.sri.ai.expresso.smt.api;

public interface SMTModel {
	
	/**
	 * Returns the integrated SMT solver's native model object
	 * that is wrapped.
	 * 
	 * @return the wrapped SMT solver's model object
	 */
	Object getEmbeddedSMTSolverModelObject();

}
