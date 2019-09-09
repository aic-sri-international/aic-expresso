package com.sri.ai.expresso.smt.core;

import com.sri.ai.expresso.smt.api.SMTSolver;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.AbstractTrivialContext;
import com.sri.ai.grinder.core.TrueContext;

import static com.sri.ai.util.Util.myAssert;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.api.SMTBasedContext;
import com.sri.ai.expresso.smt.api.SMTExpression;
import com.sri.ai.expresso.smt.api.SMTModel;

public abstract class AbstractSMTBasedContext extends TrueContext implements SMTBasedContext {
	
	private static final long serialVersionUID = 1L;
	
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
	
	
	
	// OVERRIDING INHERITED METHODS : Cloneable 
	//////////////////////////////////////////////////////
//	@Override
//	public AbstractSMTBasedContext clone() {
//		AbstractSMTBasedContext result = (AbstractSMTBasedContext) super.clone();
//		return result;
//	}
//	
//	
//	
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
	
	
	
	// OVERRIDING INHERITED METHODS : SMTContext 
	//////////////////////////////////////////////////////
	@Override
	public Object getEmbeddedSMTContext() {
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
//		SMTExpression SMTExpression = smtSolver.makeExpressoSMTExpressionFromExpressionLiteral(formula, this);
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
//		SMTExpression[] smtFormulas = convertExpressionFormulasToExpressoSMTFormuas(formulas);
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
//		SMTExpression SMTExpression = smtSolver.makeExpressoSMTExpressionFromExpressionLiteral(formula, this);
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
		SMTModel model = getModel();
		Expression result = model.getValueOfVariable(var, this);
		return result;
	}
	

	
	
//	private SMTExpression[] convertExpressionFormulasToExpressoSMTFormuas(Expression... formulas) {
//		SMTExpression[] smtFormulas = new SMTExpression[formulas.length];
//		int i = 0;
//		for(Expression formula : formulas) {
//			smtFormulas[i] = smtSolver.makeExpressoSMTExpressionFromExpressionLiteral(formula, this);
//		}		
//		return smtFormulas;
//	}
	
}
