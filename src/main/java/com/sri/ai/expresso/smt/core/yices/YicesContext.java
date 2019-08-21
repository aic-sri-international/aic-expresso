package com.sri.ai.expresso.smt.core.yices;

import com.sri.ai.expresso.smt.core.AbstractSMTContext;

public class YicesContext extends AbstractSMTContext {
	
	public YicesContext() {
		super(Yices.EXPRESSO_TO_YICES_UTIL);
	}
	
	public YicesContext(Object yicesContext) {
		super(yicesContext, Yices.EXPRESSO_TO_YICES_UTIL);
	}
}
