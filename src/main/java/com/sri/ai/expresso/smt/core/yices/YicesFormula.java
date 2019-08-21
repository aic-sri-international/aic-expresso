package com.sri.ai.expresso.smt.core.yices;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.expresso.smt.core.AbstractSMTFormula;
import com.sri.ai.grinder.api.Context;

public class YicesFormula extends AbstractSMTFormula {
	
	public YicesFormula(Object yicesFormula) {
		super(yicesFormula, Yices.EXPRESSO_TO_YICES_UTIL);
	}

	public YicesFormula(Expression expression, Context context, SMTContext smtContext) {
		super(expression, context, smtContext, Yices.EXPRESSO_TO_YICES_UTIL);
	}

}
