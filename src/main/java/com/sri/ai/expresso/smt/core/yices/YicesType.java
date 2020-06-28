package com.sri.ai.expresso.smt.core.yices;

import com.sri.ai.expresso.smt.core.AbstractSMTType;

public class YicesType extends AbstractSMTType {

	public YicesType(Object smtType) {
		super(smtType, Yices.YICES_SMT_SOLVER);
	}

}
