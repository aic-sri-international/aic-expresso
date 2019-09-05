package com.sri.ai.expresso.smt.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

/**
 * A wrapper class acting as the context of an integrated SMT solver 
 * (such as Yices or Z3).
 * 
 * TODO: currently, any assertions onto the context are placed
 * 	     on a separate stack frame
 * NOTE: operations of this class may modify the class object
 * 
 * @author Bobak
 *
 */
public interface SMTBasedContext extends Context {
	
	/**
	 * Enforces a constraint onto the SMT solver's context.
	 * 
	 * @param smtFormula is the constraint to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnExistingStackFrame(SMTExpression smtFormula);
	
	/**
	 * Enforces a constraint onto the SMT solver's context.
	 * 
	 * @param formula is the constraint to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnExistingStackFrame(Expression formula);
	
	/**
	 * Enforces multiple constrains onto the SMT solver's context.
	 * 
	 * @param smtFormulas are the constraints to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnExistingStackFrame(SMTExpression... smtFormulas);
	
	/**
	 * Enforces multiple constrains onto the SMT solver's context.
	 * 
	 * @param smtFormulas are the constraints to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnExistingStackFrame(Expression... formulas);
	
	/**
	 * Enforces a constraint onto the SMT solver's context.
	 * 
	 * @param smtFormula is the constraint to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnNewStackFrame(SMTExpression smtFormula);
	
	/**
	 * Enforces a constraint onto the SMT solver's context.
	 * 
	 * @param formula is the constraint to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnNewStackFrame(Expression formula);
	
	/**
	 * Enforces multiple constrains onto the SMT solver's context.
	 * 
	 * @param smtFormulas are the constraints to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnNewStackFrame(SMTExpression... smtFormulas);
	
	/**
	 * Enforces multiple constrains onto the SMT solver's context.
	 * 
	 * @param smtFormulas are the constraints to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnNewStackFrame(Expression... formulas);
	
	/**
	 * Checks whether the context [with asserted constraints] is
	 * satisfiable.
	 * 
	 * @return true if the context is 
	 */
	public boolean isSatisfiable();
	
	/**
	 * Checks whether the context would be satisfiable if conjoined
	 * with the given formula.
	 * 
	 * @return true if the context is 
	 */
	public boolean isSatisfiable(Expression formula);
	
	/**
	 * If the context is satisfiable, returns one possible set of assignments to the
	 * registered variables that satisfies the context.
	 * 
	 * @return A string representation of assignments to the registered variables
	 *         that satisfies the context if context is satisfiable; null otherwise
	 */
	public String getModelAsString();
	
	/**
	 * Gets an SMTModel of the context if satisfiable
	 * 
	 * @return SMTModel if context is satisfiable; null otherwise
	 */
	public SMTModel getModel();
	
	/**
	 * Gets the SMTSolver being used by this context.
	 * 
	 * @return SMTSolver object being used by this context.
	 */
	public SMTSolver getSMTSolver();
	
	public Object getEmbeddedSMTContext();
	
	public SMTBasedContext pushStackFrame();
	public SMTBasedContext popStackFrame();

	Expression getValueOfVariable(Expression var);

}
