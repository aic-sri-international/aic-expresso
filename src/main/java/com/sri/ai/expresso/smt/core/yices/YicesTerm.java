package com.sri.ai.expresso.smt.core.yices;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.core.AbstractSMTTerm;
import com.sri.ai.expresso.smt.core.yices.Yices;
import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.grinder.api.Context;

public class YicesTerm extends AbstractSMTTerm {
	
	public YicesTerm(Object yicesTerm) {
		super(yicesTerm, Yices.EXPRESSO_TO_YICES_UTIL);
	}

	public YicesTerm(Expression expression, Context context, SMTContext smtContext) {
		super(expression, context, smtContext, Yices.EXPRESSO_TO_YICES_UTIL);
	}

}
