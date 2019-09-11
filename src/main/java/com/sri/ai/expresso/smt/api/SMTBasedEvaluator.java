package com.sri.ai.expresso.smt.api;

import com.sri.ai.expresso.api.Expression;

public interface SMTBasedEvaluator {
	
	Expression eval(Expression expression, SMTBasedContext smtBasedContext);

}
