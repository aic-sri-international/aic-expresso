package com.sri.ai.expresso.smt.core;

import static com.sri.ai.util.Util.myAssert;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.api.SMTSolver;
import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.expresso.smt.api.SMTFormula;
import com.sri.ai.grinder.api.Context;

public abstract class AbstractSMTFormula implements SMTFormula {
	
	// DATA MEMBERS
	//////////////////////////////////////////////////////	
	protected final Object smtFormula;
	protected final SMTSolver expressoToSMTUtil;
	
	
	
	// CONSTRUCTORS 
	//////////////////////////////////////////////////////
	public AbstractSMTFormula(Object smtFormula, SMTSolver expressoToSMTUtil) {
		myAssert(smtFormula.getClass() == expressoToSMTUtil.getSMTSolverFormulaClass(), ()->"ERROR: Attempting to construct SMT formula with a " + smtFormula.getClass().getSimpleName() + " object as the embedded formula, but expected " + expressoToSMTUtil.getSMTSolverFormulaClass() + "!");
		this.expressoToSMTUtil = expressoToSMTUtil;
		this.smtFormula = smtFormula;
	}

	public AbstractSMTFormula(Expression expression, Context context, SMTContext smtContext, SMTSolver expressoToSMTUtil) {
		myAssert(expressoToSMTUtil.getCorrespondingExpressoSMTContextClassBranch().isAssignableFrom(smtContext.getClass()), ()->"ERROR: Attempting to construct SMT term with " + smtContext.getClass().getSimpleName() + " as the SMT context, but using " + expressoToSMTUtil.getClass().getSimpleName() + "!");
		this.expressoToSMTUtil = expressoToSMTUtil;
		this.smtFormula = expressoToSMTUtil.makeSMTSolverFormulaObjectFromExpressionLiteral(expression, context, smtContext);
	}
	
	

	// OVERRIDING INHERITED METHODS : SMTFormula 
	//////////////////////////////////////////////////////
	@Override
	public Object getEmbeddedFormulaObject() {
		return smtFormula;
	}

	@Override
	public String getFormulaString() {
		// TODO Auto-generated method stub
		return null;
	}

}
