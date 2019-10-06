package com.sri.ai.expresso.smt.core;

import com.sri.ai.expresso.smt.api.SMTSolver;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionEvaluator;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.TrueContext;

import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;

import java.text.DecimalFormat;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.api.SMTBasedContext;
import com.sri.ai.expresso.smt.api.SMTExpression;
import com.sri.ai.expresso.smt.api.SMTModel;

public abstract class AbstractSMTBasedContext extends TrueContext implements SMTBasedContext {
	
	private static final long serialVersionUID = 1L;
	
	// STATIC FIELDS 
	//////////////////////////////////////////////////////
	private static final ExpressionEvaluator smtBasedEvaluator = new DefaultSMTBasedExpressionEvaluator();
	private static long totalEvaluateTime = 0;
	
	
	
	// DATA MEMBERS 
	//////////////////////////////////////////////////////
	protected final Object smtContext;
	protected final SMTSolver smtSolver;

	
	
	
	// CONSTRUCTORS 
	//////////////////////////////////////////////////////
	public AbstractSMTBasedContext(SMTSolver expressoToSMTUtil, Theory theory) {
		super(theory);
		this.smtSolver = expressoToSMTUtil;
		this.smtContext = expressoToSMTUtil.makeSMTSolverContextObject();
	}
	
	
	//TODO: register symbols with smt context when registering with Expresso
//	// OVERRIDING INHERITED METHODS : Registry 
//	//////////////////////////////////////////////////////
//	@Override
//	public Context extendWithSymbolsAndTypes(Expression... symbolsAndTypes) {
//		
//		return null;
//	}
//	
//	@Override
//	public Context extendWithSymbolsAndTypes(String... symbolsAndTypes) {
//		
//		return null;
//	}
	
	
	
	// OVERRIDING INHERITED METHODS : Constraint 
	//////////////////////////////////////////////////////
	@Override
	public boolean isContradiction() {
		boolean result = !isSatisfiable();
		return result;
	}
	
	@Override
	public boolean isContradiction(Expression literal) {
		boolean result = !isSatisfiable(literal);
		return result;
	}
	
	
	
	// OVERRIDING INHERITED METHODS : Context 
	//////////////////////////////////////////////////////
	@Override
	public Context conjoin(Expression formula, Context context) {
		Context result = assertOnNewStackFrame(formula);
		return result;
	}
	
	@Override
	public Context conjoinWithLiteral(Expression literal, Context context) {
		Context result = assertOnNewStackFrame(literal);
		return result;
	}
	
	@Override
	public Expression evaluate(Expression expression) {
		long initialTime = System.currentTimeMillis();
		Expression result = smtBasedEvaluator.eval(expression, this);
		long finalTime = System.currentTimeMillis();
		totalEvaluateTime += (finalTime - initialTime);
		return result;
	}
	public void printEvaluateTimeBreakdown() {
		DecimalFormat df = new DecimalFormat();
		df.setMaximumFractionDigits(2);
		long topRewriteTime = ((DefaultSMTBasedExpressionEvaluator) smtBasedEvaluator).getTopRewriteTime();
		long literalNegationTime = ((DefaultSMTBasedExpressionEvaluator) smtBasedEvaluator).getLiteralNegationTime();
		long expressionEqualsTime = ((DefaultSMTBasedExpressionEvaluator) smtBasedEvaluator).getExpressionEqualsTime();
		long isVariableOrConstantTime = ((DefaultSMTBasedExpressionEvaluator) smtBasedEvaluator).getIsVariableOrConstantTime();
		long isContradictionTime = ((DefaultSMTBasedExpressionEvaluator) smtBasedEvaluator).getIsContradictionTime();
		long isLiteralTime = ((DefaultSMTBasedExpressionEvaluator) smtBasedEvaluator).getIsLiteralTime();
		long makeExpressionUnderLiteralValueTime = ((DefaultSMTBasedExpressionEvaluator) smtBasedEvaluator).getMakeExpressionUnderLiteralValueTime();
		println("                                  Total smtContext.evaluate() time:  " + totalEvaluateTime);
		println("                             Total theory.topRewriter.apply() time:  " + topRewriteTime);
		println("                                   Percentage of topRewrite() time:  " + df.format(  100.0*topRewriteTime/totalEvaluateTime ) + "%");
		println("                            Total theory.getLiteralNegation() time:  " + literalNegationTime);
		println("                           Percentage of getLiteralNegation() time:  " + df.format(  100.0*literalNegationTime/totalEvaluateTime ) + "%");
		println("                                    Total expression.equals() time:  " + expressionEqualsTime);
		println("                                       Percentage of equals() time:  " + df.format(  100.0*expressionEqualsTime/totalEvaluateTime ) + "%");
		println(" Total context.isVariable()+context.isUniquelyNamedConstant() time:  " + isVariableOrConstantTime);
		println("         Percentage of isVariable()+isUniquelyNamedConstant() time:  " + df.format(  100.0*isVariableOrConstantTime/totalEvaluateTime ) + "%");
		println("                              Total context.isContradiction() time:  " + isContradictionTime);
		println("                              Percentage of isContradiction() time:  " + df.format(  100.0*isContradictionTime/totalEvaluateTime ) + "%");
		println("                                    Total context.isLiteral() time:  " + isLiteralTime);
		println("                                    Percentage of isLiteral() time:  " + df.format(  100.0*isLiteralTime/totalEvaluateTime ) + "%");
		println("                      Total makeExpressionUnderLiteralValue() time:  " + makeExpressionUnderLiteralValueTime);
		println("              Percentage of makeExpressionUnderLiteralValue() time:  " + df.format(  100.0*makeExpressionUnderLiteralValueTime/totalEvaluateTime ) + "%");
	}
	
	
	
	// OVERRIDING INHERITED METHODS : SMTContext 
	//////////////////////////////////////////////////////
	@Override
	public Object getEmbeddedSMTSolverContextObject() {
		return smtContext;
	}
	
	@Override
	public SMTBasedContext pushStackFrame() {
		smtSolver.pushStackFrame(this);
		return this;
	}
	
	@Override
	public SMTBasedContext popStackFrame() {
		smtSolver.popStackFrame(this);
		return this;
	}
	
	@Override
	public AbstractSMTBasedContext assertOnExistingStackFrame(SMTExpression SMTExpression) {
		smtSolver.assertOntoContext(this, SMTExpression);
		return this;
	}
	
	@Override
	public AbstractSMTBasedContext assertOnExistingStackFrame(Expression formula) {
//		SMTExpression SMTExpression = smtSolver.makeExpressoSMTExpressionFromExpressionLiteral(formula, this);
		smtSolver.assertOntoContext(this, formula);
		return this;
	}

	@Override
	public AbstractSMTBasedContext assertOnExistingStackFrame(SMTExpression... smtFormulas) {
		smtSolver.assertOntoContext(this, smtFormulas);
		return this;	
	}
	
	@Override
	public AbstractSMTBasedContext assertOnExistingStackFrame(Expression... formulas) {
//		SMTExpression[] smtFormulas = convertExpressionFormulasToExpressoSMTFormuas(formulas);
		smtSolver.assertOntoContext(this, formulas);
		return this;
	}
	
	@Override
	public AbstractSMTBasedContext assertOnNewStackFrame(SMTExpression SMTExpression) {
		smtSolver.pushStackFrame(this);
		smtSolver.assertOntoContext(this, SMTExpression);
		return this;
	}
	
	@Override
	public AbstractSMTBasedContext assertOnNewStackFrame(Expression formula) {
		smtSolver.pushStackFrame(this);
		smtSolver.assertOntoContext(this, formula);
		return this;
	}
	
	@Override
	public AbstractSMTBasedContext assertOnNewStackFrame(SMTExpression... smtFormulas) {
		smtSolver.pushStackFrame(this);
		smtSolver.assertOntoContext(this, smtFormulas);
		return this;	
	}
	
	@Override
	public AbstractSMTBasedContext assertOnNewStackFrame(Expression... formulas) {
		smtSolver.pushStackFrame(this);
		smtSolver.assertOntoContext(this, formulas);
		return this;
	}

	@Override
	public boolean isSatisfiable() {
		boolean isSAT = smtSolver.contextIsSatisfiable(this);
		return isSAT;
	}

	@Override
	public boolean isSatisfiable(Expression formula) {
		boolean isSAT = smtSolver.contextIsSatisfiable(this, formula);
		return isSAT;
	}

	@Override
	public String getModelAsString() {
		String model = smtSolver.getModelAsString(this);
		return model;
	}
	
	@Override
	public SMTSolver getSMTSolver() {
		return smtSolver;
	}
	
	@Override
	public SMTModel getModel() {
		SMTModel model = smtSolver.getModel(this);
		return model;
	}
	
	@Override
	public Expression getValueOfVariable(Expression var) {
		myAssert(this.isVariable(var), ()->"ERROR: attempting to obtain value of " + var + " which is not a variable expression!");
		Expression result;
		result = smtSolver.getValueOfVariable(var, this);
		return result;
	}
	
}
