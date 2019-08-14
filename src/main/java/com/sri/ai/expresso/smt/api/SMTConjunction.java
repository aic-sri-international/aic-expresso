package com.sri.ai.expresso.smt.api;

import com.sri.ai.expresso.api.Conjunction;

public interface SMTConjunction extends Conjunction {
	
	public SMTConjunction backtrack();
	public String getModel();

}
