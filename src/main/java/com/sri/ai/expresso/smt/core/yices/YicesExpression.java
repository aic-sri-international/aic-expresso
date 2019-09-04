package com.sri.ai.expresso.smt.core.yices;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.api.SMTBasedContext;
import com.sri.ai.expresso.smt.core.AbstractSMTExpression;

public class YicesExpression extends AbstractSMTExpression{

	public YicesExpression(Object yicesTerm) {
		super(yicesTerm, Yices.YICES_SMT_SOLVER);
		}

	public YicesExpression(Expression expression, SMTBasedContext smtContext) {
		super(expression, smtContext, Yices.YICES_SMT_SOLVER);
	}

}
