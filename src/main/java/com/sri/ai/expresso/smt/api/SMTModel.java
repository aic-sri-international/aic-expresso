package com.sri.ai.expresso.smt.api;

import com.sri.ai.expresso.api.Expression;

public interface SMTModel {
	
	
	Object getEmbeddedSMTSolverObject();
	Expression getValueOfVariable(Expression expression, SMTBasedContext smtContext);

}
