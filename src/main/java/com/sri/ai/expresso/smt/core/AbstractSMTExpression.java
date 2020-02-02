package com.sri.ai.expresso.smt.core;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.api.SMTBasedContext;
import com.sri.ai.expresso.smt.api.SMTExpression;
import com.sri.ai.expresso.smt.api.SMTSolver;

public abstract class AbstractSMTExpression implements SMTExpression {
	
	// DATA MEMBERS
	//////////////////////////////////////////////////////	
	protected final Object smtExpression;
	protected final SMTSolver smtSolver;
	
	
	
	// CONSTRUCTORS 
	//////////////////////////////////////////////////////
	public AbstractSMTExpression(Object smtFormula, SMTSolver expressoToSMTUtil) {
		this.smtSolver = expressoToSMTUtil;
		this.smtExpression = smtFormula;
	}

	public AbstractSMTExpression(Expression expression, SMTBasedContext smtContext, SMTSolver expressoToSMTUtil) {
		this.smtSolver = expressoToSMTUtil;
		this.smtExpression = expressoToSMTUtil.makeSMTSolverExpressionObjectFromExpressionLiteral(expression, smtContext);
	}
	
	

	// OVERRIDING INHERITED METHODS : SMTFormula 
	//////////////////////////////////////////////////////
	@Override
	public Object getEmeddedSMTSolverExpressionObject() {
		return smtExpression;
	}

}
