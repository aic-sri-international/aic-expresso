package com.sri.ai.expresso.smt.core;

import static com.sri.ai.util.Util.myAssert;

import com.sri.ai.expresso.smt.api.SMTSolver;
import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.expresso.smt.api.SMTFormula;

public abstract class AbstractSMTContext implements SMTContext {
	
	// DATA MEMBERS 
	//////////////////////////////////////////////////////
	protected final Object smtContext;
	protected final SMTSolver expressoToSMTUtil;
	
	
	
	// CONSTRUCTORS 
	//////////////////////////////////////////////////////
	public AbstractSMTContext(SMTSolver expressoToSMTUtil) {
		this.expressoToSMTUtil = expressoToSMTUtil;
		this.smtContext = expressoToSMTUtil.makeSMTSolverContextObject();
	}
	
	public AbstractSMTContext(Object smtContext, SMTSolver expressoToSMTUtil) {
		myAssert(expressoToSMTUtil.getCorrespondingExpressoSMTContextClassBranch().isAssignableFrom(smtContext.getClass()), ()->"ERROR: Attempting to construct SMT conjunction with a " + smtContext.getClass().getSimpleName() + " objet as the SMT context, but using " + expressoToSMTUtil.getClass().getSimpleName() + "!");
		this.expressoToSMTUtil = expressoToSMTUtil;
		this.smtContext = smtContext;
	}
	
	
	
	// OVERRIDING INHERITED METHODS : SMTContext 
	//////////////////////////////////////////////////////
	@Override
	public SMTContext assertOntoContext(SMTFormula smtFormula) {
		expressoToSMTUtil.assertOntoContex(smtContext, smtFormula);
		return this;
	}

	@Override
	public SMTContext assertOntoContext(SMTFormula... smtFormulas) {
		expressoToSMTUtil.assertOntoContex(smtContext, smtFormulas);
		return this;	
	}

	@Override
	public SMTContext pushStackFrame() {
		expressoToSMTUtil.pushStackFrame(smtContext);
		return this;	
	}

	@Override
	public SMTContext popStackFrame() {
		expressoToSMTUtil.popStackFrame(smtContext);
		return this;	
	}

	@Override
	public boolean contextIsSatisfiable() {
		boolean isSAT = expressoToSMTUtil.contextIsSatisfiable(smtContext);
		return isSAT;
	}

	@Override
	public String getModel() {
		String model = expressoToSMTUtil.getModel(smtContext);
		return model;
	}
	
	
}
