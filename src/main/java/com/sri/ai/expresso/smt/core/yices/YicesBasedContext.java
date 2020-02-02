package com.sri.ai.expresso.smt.core.yices;

import com.sri.ai.expresso.smt.core.AbstractSMTBasedContext;
import com.sri.ai.grinder.api.Theory;

public class YicesBasedContext extends AbstractSMTBasedContext {

	private static final long serialVersionUID = 1L;

	public YicesBasedContext(Theory theory) {
		super(Yices.YICES_SMT_SOLVER, theory);
	}
	
}
