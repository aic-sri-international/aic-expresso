package com.sri.ai.expresso.smt.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

/**
 * An API for wrapping the context object of an integrated SMT solver 
 * (such as Yices or Z3).  The wrapper interface allows interaction
 * of the integrated context with Expresso types.
 * 
 * SMT solver contexts often have a notion of stack-frames where
 * assertions onto the context are collected.  Backtracking assertions
 * is not done individually, but rather via popping stack-frames that
 * hold assertions.  Thus, methods here make a distinction between asserting
 * a constraints onto an existing stack frame and asserting constraints
 * onto a newly added stack frame.
 * 
 * NOTE: the integrated SMT solver context may be mutable and unclonable
 *       leaving the wrapper object similarly mutable
 * 
 * @author Bobak
 *
 */
public interface SMTBasedContext extends Context {
	
	/**
	 * Enforces a constraint onto the current stack frame of the SMT solver's context.
	 * 
	 * @param smtFormula is the constraint to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnExistingStackFrame(SMTExpression smtFormula);
	
	/**
	 * Enforces a constraint onto the current stack frame of the SMT solver's context.
	 * 
	 * @param formula is the constraint to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnExistingStackFrame(Expression formula);
	
	/**
	 * Enforces multiple constrains onto the current stack frame of the SMT solver's context.
	 * 
	 * @param smtFormulas are the constraints to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnExistingStackFrame(SMTExpression... smtFormulas);
	
	/**
	 * Enforces multiple constrains onto current stack frame of the SMT solver's context.
	 * 
	 * @param smtFormulas are the constraints to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnExistingStackFrame(Expression... formulas);
	
	/**
	 * Enforces a constraint onto a newly added stack frame of the SMT solver's context.
	 * 
	 * @param smtFormula is the constraint to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnNewStackFrame(SMTExpression smtFormula);
	
	/**
	 * Enforces a constraint onto a newly added stack frame of the SMT solver's context.
	 * 
	 * @param formula is the constraint to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnNewStackFrame(Expression formula);
	
	/**
	 * Enforces multiple constrains onto a newly added stack frame of the SMT solver's context.
	 * Note that all constraints are added to the same stack frame.
	 * 
	 * @param smtFormulas are the constraints to be asserted onto the context
	 * @return an {@link SMTBasedContext} object with updated context
	 */
	public SMTBasedContext assertOnNewStackFrame(SMTExpression... smtFormulas);
	
	/**
	 * Enforces multiple constrains onto a newly added stack frame of the SMT solver's context.
	 * Note that all constraints are added to the same stack frame.
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
	 * Returns an {@link SMTModel} object containing a solution of single assignments to each 
	 * variables such that the set of assignments is valid under the context's constraints).
	 * 
	 * Gets an {@link SMTModel} of the context if the context is satisfiable
	 * 
	 * @return {@link SMTModel} if context is satisfiable; null otherwise
	 */
	public SMTModel getModel();
	
	/**
	 * Gets the {@link SMTSolver} object being used by this context.
	 * 
	 * @return SMTSolver object being used by this context.
	 */
	public SMTSolver getSMTSolver();
	
	/**
	 * Returns the integrated SMT solver's native context object
	 * that is wrapped.
	 * 
	 * @return the wrapped SMT solver's context object
	 */
	public Object getEmbeddedSMTSolverContextObject();
	
	/**
	 * Adds a new stack frame to collect newly added constraints on.
	 * 
	 * @return itself
	 */
	public SMTBasedContext pushStackFrame();
	
	/**
	 * Pops a stack frame and all constraints that were stored on it.
	 * 
	 * @return itself
	 */
	public SMTBasedContext popStackFrame();

	/**
	 * If constraints limit the passed in variable to a unique value,
	 * an {@link Expression} object of this value is returned.
	 * 
	 * @return if the passed in variable has a unique satisfiable value,
	 *         returns an {@link Expression} of this value; otherwise
	 *         returns null.
	 */
	public Expression getValueOfVariable(Expression var);

}
