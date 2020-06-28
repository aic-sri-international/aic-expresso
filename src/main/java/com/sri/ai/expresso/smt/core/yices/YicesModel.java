package com.sri.ai.expresso.smt.core.yices;

import static com.sri.ai.util.Util.myAssert;

import com.sri.ai.expresso.smt.core.AbstractSMTModel;
import com.sri.yices.Model;

public class YicesModel extends AbstractSMTModel {

	public YicesModel(Object smtModel) {
		super(smtModel, Yices.YICES_SMT_SOLVER);
		myAssert(smtModel instanceof Model, ()->"ERROR: Attempting to construct a YicesFormula using a " + smtModel.getClass().getSimpleName() + " object instead of an " + Model.class.getSimpleName() + " object!");
	}
	
	
	
}
