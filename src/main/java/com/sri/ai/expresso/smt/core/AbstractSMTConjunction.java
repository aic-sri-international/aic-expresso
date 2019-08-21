package com.sri.ai.expresso.smt.core;

import static com.sri.ai.util.Util.myAssert;

import com.sri.ai.expresso.api.Conjunction;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.api.SMTSolver;
import com.sri.ai.expresso.smt.api.SMTConjunction;
import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.expresso.smt.api.SMTFormula;
import com.sri.ai.grinder.api.Context;

public abstract class AbstractSMTConjunction implements SMTConjunction {
	
	// DATA MEMBERS
	//////////////////////////////////////////////////////	
	private final SMTSolver expressoToSMTUtil;
	protected final SMTContext smtContext;

	
	
	// CONSTRUCTORS 
	//////////////////////////////////////////////////////
	public AbstractSMTConjunction(SMTContext smtContext, SMTSolver expressoToSMTUtil) {
		myAssert(expressoToSMTUtil.getCorrespondingExpressoSMTContextClassBranch().isAssignableFrom(smtContext.getClass()), ()->"ERROR: Attempting to construct SMT conjunction with a " + smtContext.getClass().getSimpleName() + " objet as the SMT context, but using " + expressoToSMTUtil.getClass().getSimpleName() + "!");
		this.smtContext = smtContext;
		this.expressoToSMTUtil = expressoToSMTUtil;
	}
	
	public AbstractSMTConjunction(Expression literal, Context context, SMTContext smtContext, SMTSolver expressoToSMTUtil) {
		myAssert(expressoToSMTUtil.getCorrespondingExpressoSMTContextClassBranch().isAssignableFrom(smtContext.getClass()), ()->"ERROR: Attempting to construct SMT conjunction with " + smtContext.getClass().getSimpleName() + " as the SMT context, but using " + expressoToSMTUtil.getClass().getSimpleName() + "!");
		this.smtContext = smtContext;
		this.expressoToSMTUtil = expressoToSMTUtil;
		SMTFormula smtLiteral = expressoToSMTUtil.makeExpressoSMTFormulaFromExpressionLiteral(literal, context, smtContext);
		conjoinSMTFormulaToContextOnANewStackFrame(smtLiteral);
	}

	
	    
	// OVERRIDING INHERITED METHODS : Conjunction 
	//////////////////////////////////////////////////////
	@Override
	public boolean isSatisfiable() {
		return smtContextIsSatisfiable();
	}
	
	@Override
	public Conjunction and(Expression literal, Context context) {
		SMTFormula smtLiteral = expressoToSMTUtil.makeExpressoSMTFormulaFromExpressionLiteral(literal, context, smtContext);
		conjoinSMTFormulaToContextOnANewStackFrame(smtLiteral);
		return this;
	}
	
	
	
	// OVERRIDING INHERITED METHODS : SMTConjunction 
	//////////////////////////////////////////////////////
	@Override
	public SMTConjunction backtrack() {
		smtContext.popStackFrame();
		return this;
	}
	
	@Override
	public String getModel() {
		String model = smtContext.getModel();
		return model;
	}
	
	
	
	// INHERITABLE HELPER METHODS 
	//////////////////////////////////////////////////////
	protected void conjoinSMTFormulaToContextOnANewStackFrame(SMTFormula smtFormula) {
		smtContext.pushStackFrame();
		smtContext.assertOntoContext(smtFormula);
	}

	
	
	// PRIVATE HELPER METHODS
	//////////////////////////////////////////////////////	
	private boolean smtContextIsSatisfiable() {
		boolean contextIsSatisfiable = smtContext.contextIsSatisfiable();
		return contextIsSatisfiable;
	}

}
