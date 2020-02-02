package com.sri.ai.expresso.smt.api;

/**
 * An API for wrapping the expression object of an integrated SMT solver 
 * (such as Yices or Z3).  The wrapper interface allows interaction
 * of the integrated expression with Expresso types.
 * 
 * SMT solver expressions can be constants, variables, terms, or formulas.
 * 
 * TODO: currently there is only one method to extract the embedded SMT
 *       solver object because [thus far] any other operation on these
 *       constructed expressions require the native embedded object.
 *       However, as these {@link SMTExpressions} are used more independently,
 *       additional methods should be added.
 * 
 * @author Bobak
 *
 */
public interface SMTExpression {
	
	/**
	 * Returns the integrated SMT solver's native expression object
	 * that is wrapped.
	 * 
	 * @return the wrapped SMT solver's expression object
	 */
	public Object getEmeddedSMTSolverExpressionObject();
	
}
