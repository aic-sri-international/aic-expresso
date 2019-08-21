package com.sri.ai.expresso.smt.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

public interface SMTSolver {
	
	public Class<? extends Object> getSMTSolverContextClass();
	public Class<? extends Object> getSMTSolverTermClass();
	public  Class<? extends Object> getSMTSolverFormulaClass();
	
	public Class<? extends SMTContext> getCorrespondingExpressoSMTContextClassBranch();
	public Class<? extends SMTTerm> getCorrespondingExpressoSMTTermClassBranch();
	public  Class<? extends SMTFormula> getCorrespondingExpressoSMTFormulaClassBranch();


	public SMTFormula makeExpressoSMTFormulaFromExpressionLiteral(Expression literal, Context context, SMTContext smtContext);
	public SMTTerm makeExpressoSMTTermFromExpressoExpression(Expression expression, Context context, SMTContext smtContext);
	public SMTContext makeExpressoSMTContext();

	public Object makeSMTSolverFormulaObjectFromExpressionLiteral(Expression literal, Context context, SMTContext smtContext);
	public String getFormulaString(Object smtFormula);
	
	public Object makeSMTSolverTermObjectFromExpressoExpression(Expression expression, Context context, SMTContext smtContext);
	public Object getTypeReference(Object smtTerm);
	public String getTypeName(Object smtTerm);
	public boolean isRegistered(Object smtTerm);
	public String getSymbolName(Object smtTerm);
	
	public Object makeSMTSolverContextObject();
	public Object assertOntoContex(Object smtContext, SMTFormula smtFormula);
	public Object assertOntoContex(Object smtContext, SMTFormula... smtFormula);
	public Object pushStackFrame(Object smtContext);
	public Object popStackFrame(Object smtContext);
	public boolean contextIsSatisfiable(Object smtContext);
	public String getModel(Object smtContext);

}
