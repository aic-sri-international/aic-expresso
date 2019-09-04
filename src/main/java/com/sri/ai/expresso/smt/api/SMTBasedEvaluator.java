package com.sri.ai.expresso.smt.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

public interface SMTBasedEvaluator {

	Expression eval(Expression expression, Context context);

}
