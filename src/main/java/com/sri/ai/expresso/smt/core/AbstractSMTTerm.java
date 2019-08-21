package com.sri.ai.expresso.smt.core;

import static com.sri.ai.util.Util.myAssert;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.api.SMTSolver;
import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.expresso.smt.api.SMTTerm;
import com.sri.ai.grinder.api.Context;

public abstract class AbstractSMTTerm implements SMTTerm {
	
	// DATA MEMBERS
	//////////////////////////////////////////////////////	
	protected final Object smtTerm;
	protected final SMTSolver expressoToSMTUtil;
	
	
	
	// CONSTRUCTORS 
	//////////////////////////////////////////////////////
	public AbstractSMTTerm(Object smtTerm, SMTSolver expressoToSMTUtil) {
		myAssert(smtTerm.getClass() == expressoToSMTUtil.getSMTSolverTermClass(), ()->"ERROR: Attempting to construct SMT term with a " + smtTerm.getClass().getSimpleName() + " object as the embedded term, but expected " + expressoToSMTUtil.getSMTSolverTermClass() + "!");
		this.expressoToSMTUtil = expressoToSMTUtil;
		this.smtTerm = smtTerm;
	}

	public AbstractSMTTerm(Expression expression, Context context, SMTContext smtContext, SMTSolver expressoToSMTUtil) {
		myAssert(expressoToSMTUtil.getCorrespondingExpressoSMTContextClassBranch().isAssignableFrom(smtContext.getClass()), ()->"ERROR: Attempting to construct SMT term with " + smtContext.getClass().getSimpleName() + " as the SMT context, but using " + expressoToSMTUtil.getClass().getSimpleName() + "!");
		this.expressoToSMTUtil = expressoToSMTUtil;
		this.smtTerm = expressoToSMTUtil.makeSMTSolverTermObjectFromExpressoExpression(expression, context, smtContext);
	}
	
	
	
	// OVERRIDING INHERITED METHODS : SMTTerm 
	//////////////////////////////////////////////////////
	@Override
	public Object getEmeddedReference() {
		return smtTerm;
	}

	@Override
	public Object getCorrespondingTypeReference() {
		Object termTypeReference = expressoToSMTUtil.getTypeReference(smtTerm);
		return termTypeReference;
	}

	@Override
	public String getCorrespondingTypeName() {
		String typeNmae = expressoToSMTUtil.getTypeName(smtTerm);
		return typeNmae;
	}

}
