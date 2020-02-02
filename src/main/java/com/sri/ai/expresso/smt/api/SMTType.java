package com.sri.ai.expresso.smt.api;

public interface SMTType {

	
	/**
	 * Returns the integrated SMT solver's native type object
	 * that is wrapped.
	 * 
	 * @return the wrapped SMT solver's type object
	 */
	Object getEmbeddedSMTSolverObject();

}
