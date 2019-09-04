package com.sri.ai.expresso.smt.api;

import com.sri.ai.expresso.api.Expression;

/**
 * An Expresso interface for representing an integrated SMT Solver (such as
 * Yices or Z3).  Expresso SMTSolver-derived classes will represent specific
 * SMT solvers and allow Expresso to utilize their functionality through
 * generalized methods.
 * 
 * @author Bobak
 *
 */
public interface SMTSolver {

	public Class<? extends SMTBasedContext> getExpressoSMTContextClass();
	public Class<? extends SMTExpression> 	getExpressoSMTExpressionClass();
	Class<? extends SMTModel> 				getExpressoSMTModelClass();
	Class<? extends SMTType> 				getExpressoSMTTypeClass();
	
	
	Object 			makeSMTSolverExpressionObjectFromExpressionLiteral(Expression literal, SMTBasedContext smtContext);
	String 			getExpressionString(SMTExpression smtFormula);
	SMTType 		getExpressionType(SMTExpression smtFormula);
	String 			getExpressionTypeSimpleName(SMTExpression smtFormula);
	String 			getExpressionTypeNativeName(SMTExpression smtFormula);
	public boolean	isBooleanType(SMTExpression smtExpression);
	public boolean	isIntegerType(SMTExpression smtExpression);
	public boolean	isRealType(SMTExpression smtExpression);
	
	
	//SMTContext Utilities
	public Object 	makeSMTSolverContextObject();
	public Object 	assertOntoContext(SMTBasedContext smtContext, SMTExpression smtFormula);
	public Object 	assertOntoContext(SMTBasedContext smtContext, SMTExpression... smtFormula);
	public Object 	pushStackFrame(SMTBasedContext smtContext);
	public Object 	popStackFrame(SMTBasedContext smtContext);
	public boolean 	contextIsSatisfiable(SMTBasedContext smtContext);
	public boolean 	contextIsSatisfiable(SMTBasedContext smtContext, SMTExpression smtFormula);
	public Object 	assertOntoContext(SMTBasedContext smtContext, Expression formula);
	public boolean 	contextIsSatisfiable(SMTBasedContext smtContext, Expression formula);
	public Object 	assertOntoContext(SMTBasedContext smtContext, Expression[] formulas);
	public SMTModel getModel(SMTBasedContext smtContext);
	String 			getModelAsString(SMTBasedContext smtContext);


	//SMTModel Utilities
	Expression getValueOfVariable(Expression variable, SMTModel smtModel, SMTBasedContext smtContext);

	boolean expressionIsAssertible(SMTExpression smtFormula);













}
