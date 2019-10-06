package com.sri.ai.expresso.smt.api;

import com.sri.ai.expresso.api.Expression;

/**
 * An Expresso interface for representing an integrated SMT Solver engine (such as
 * Yices or Z3).  Expresso SMTSolver-derived classes will represent specific
 * SMT solvers and allow Expresso to utilize their functionality through
 * generalized methods.
 * 
 * @author Bobak
 *
 */
public interface SMTSolver {

	/**
	 * A function to get the specific Expresso wrapper class being used to wrap the integrated SMT solver's context object
	 * 
	 * @return the specific Expresso wrapper class being used to wrap the integrated SMT solver's context object
	 */
	public Class<? extends SMTBasedContext> getExpressoWrapperClassForSMTContextObject();
	
	/**
	 * A function to get the specific Expresso wrapper class being used to wrap the integrated SMT solver's expression object
	 * 
	 * @return the specific Expresso wrapper class being used to wrap the integrated SMT solver's expression object
	 */
	public Class<? extends SMTExpression> 	getExpressoWrapperClassForSMTExpressionObject();
	
	/**
	 * A function to get the specific Expresso wrapper class being used to wrap the integrated SMT solver's model object
	 * 
	 * @return the specific Expresso wrapper class being used to wrap the integrated SMT solver's model object
	 */
	public Class<? extends SMTModel> 		getExpressoWrapperClassForSMTModelObject();
	
	/**
	 * A function to get the specific Expresso wrapper class being used to wrap the integrated SMT solver's type object
	 * 
	 * @return the specific Expresso wrapper class being used to wrap the integrated SMT solver's type object
	 */
	public Class<? extends SMTType> 		getExpressoWrapperClassForSMTTypeObject();
	
	
	
	//SMTContext Utilities
	
	/**
	 * Using a provided {@link SMTBasedContext}, converts an Expresso {@link Expression} to the analogous expression
	 * object of the integrated SMT solver.
	 * 
	 * @param literal is the literal to convert
	 * @param smtContext is the context used to determine variables and types
	 * @return a reference to the integrated SMT solver expression object created
	 */
	Object 			makeSMTSolverExpressionObjectFromExpressionLiteral(Expression literal, SMTBasedContext smtContext);
	
	/**
	 * Get the String representation of an {@link SMTExpression}
	 * 
	 * @param smtExpression is the expression to get the String representation of
	 * @return a String representation of the passed in {@link SMTExpression}
	 */
	String 			getExpressionString(SMTExpression smtExpression);
	
	/**
	 * Get an {@link SMTType} object corresponding to the type of the passed in {@link SMTExpression} object
	 * 
	 * @param smtExpression is the {@link SMTExpression} object to get the type of
	 * @return an {@link SMTType} object corresponding to the type of the passed in {@link SMTExpression}
	 */
	SMTType 		getExpressionType(SMTExpression smtExpression);
	
	/**
	 * Some SMT solvers allow custom names to be set for common types or custom created types.
	 * This method gets the set name for the type of an {@link SMTExpression} object (if one exists).
	 * 
	 * @param smtExpression is the expression object to get the set type name of.
	 * @return the set type name for the type of the passed in {@link SMTExpression};
	 *    <br> null if there is no custom set type name.
	 */
	String 			getExpressionTypeSetName(SMTExpression smtExpression);
	
	/**
	 * Gets the SMTSolver native type name for the type of an {@link SMTExpression} object.
	 * 
	 * @param smtExpression is the expression object to get the native type name of.
	 * @return the native type name for the type of the passed in {@link SMTExpression}
	 */
	String 			getExpressionTypeNativeName(SMTExpression smtExpression);
	
	/**
	 * Checks whether the passed in {@link SMTExpression} is a boolean typed object with respect
	 * to the SMT solver it corresponds to.
	 * <p>
	 * Note that for some SMT solvers, this is a sufficient criteria for an expression;
	 * to be assertable to a context.
	 * 
	 * @param smtExpression is the {@link SMTExpression} object to check
	 * @return true if the passed in {@link SMTExpression} is a corresponding boolean type;
	 * 	  <br> false otherwise
	 */
	public boolean	isBooleanType(SMTExpression smtExpression);
	
	/**
	 * Checks whether the passed in {@link SMTExpression} is a integer typed object with respect
	 * to the SMT solver it corresponds to.
	 * 
	 * @param smtExpression is the {@link SMTExpression} object to check
	 * @return true if the passed in {@link SMTExpression} is a corresponding integer type;
	 * 	  <br> false otherwise
	 */
	public boolean	isIntegerType(SMTExpression smtExpression);
	
	/**
	 * Checks whether the passed in {@link SMTExpression} is a real number typed object with respect
	 * to the SMT solver it corresponds to.
	 * 
	 * @param smtExpression is the {@link SMTExpression} object to check
	 * @return true if the passed in {@link SMTExpression} is a corresponding real number type;
	 * 	  <br> false otherwise
	 */
	public boolean	isRealType(SMTExpression smtExpression);
	
	/**
	 * Checks whether the passed in {@link SMTEXpression} has a type corresponding to the SMT solver's
	 * boolean type (which for Yices and Z3 means it can be asserted to a context).
	 * 
	 * @param smtFormula is the expression to check
	 * @return
	 * 		true if smtFormula is an assertable formula
	 * 		false otherwise
	 */
	boolean expressionIsAssertible(SMTExpression smtFormula);
	
	
	
	//SMTContext Utilities
	
	/**
	 * Creates a blank SMT solver context object that has as pushable/poppable stack for assertions.
	 * 
	 * @return a reference to the integrated SMT solver context object created
	 */
	public Object 	makeSMTSolverContextObject();
	
	/**
	 * Asserts an {@link SMTExpression} representing a formula to the passed in {@link SMTBasedContext}
	 * 
	 * @param smtContext is the {@link SMTBasedContext} object to be asserted onto
	 * @param smtFormula is the {@link SMTExpression} to assert
	 * @return the reference to the native SMT context object wrapped within smtContext after assertion
	 */
	public Object 	assertOntoContext(SMTBasedContext smtContext, SMTExpression smtFormula);
	
	/**
	 * Asserts the {@link SMTExpression}s representing formulas to the passed in {@link SMTBasedContext}
	 * 
	 * @param smtContext is the {@link SMTBasedContext} object to be asserted onto
	 * @param smtFormula are the {@link SMTExpression}s to assert
	 * @return the reference to the native SMT context object wrapped within smtContext after assertion
	 */
	public Object 	assertOntoContext(SMTBasedContext smtContext, SMTExpression... smtFormula);
	
	/**
	 * Adds a new stack frame to the passed in {@link SMTBasedContext}.
	 * <p>
	 * Stack frames store the most recent assertions of a context, and when popped,
	 * removes those assertions.  They act as a "save point" of the context state.
	 * 
	 * @param smtContext is the {@SMTBasedContext} to add a stack frame onto.
	 * @return the reference to the native SMT context object wrapped within smtContext after addition
	 *         of new stack frame.
	 */
	public Object 	pushStackFrame(SMTBasedContext smtContext);
	
	/**
	 * Pops a stack frame off of the passed in {@link SMTBasedContext}.
	 * <p>
	 * Stack frames store the most recent assertions of a context, and when popped,
	 * removes those assertions.  They act as a "save point" of the context state.
	 * 
	 * @param smtContext is the {@SMTBasedContext} to pop a stack frame off of.
	 * @return the reference to the native SMT context object wrapped within smtContext after popping
	 *         of the stack frame.
	 */
	public Object 	popStackFrame(SMTBasedContext smtContext);
	
	/**
	 * Checks whether the passed in {@link SMTBasedContext} is satisfiable given the current
	 * constraints asserted onto it.
	 * 
	 * @param smtContext is the {@link SMTBasedContext} object to check satisfiability of
	 * @return
	 * 		true if the passed in context is satisfiable
	 * 		false otherwise
	 */
	public boolean 	contextIsSatisfiable(SMTBasedContext smtContext);
	
	/**
	 * Checks whether the passed in {@link SMTBasedContext} would be satisfiable if the
	 * passed in {@link SMTExpression} were asserted to it.
	 * 
	 * @param smtContext is the {@link SMTBasedContext} object to check satisfiability of
	 * @return
	 * 		true if the passed in context is satisfiable after assertion of the passed in {@link SMTExpression}
	 * 		false otherwise
	 */
	public boolean 	contextIsSatisfiable(SMTBasedContext smtContext, SMTExpression smtFormula);
	
	/**
	 * Asserts an Expresso {@link Expression} representing a formula to the passed in {@link SMTBasedContext}
	 * 
	 * @param smtContext is the {@link SMTBasedContext} object to be asserted onto
	 * @param formula is the Expresso {@link Expression} to assert
	 * @return the reference to the native SMT context object wrapped within smtContext after assertion
	 */
	public Object 	assertOntoContext(SMTBasedContext smtContext, Expression formula);
	
	/**
	 * Checks whether the passed in {@link SMTBasedContext} would be satisfiable if the
	 * passed in {@link Expression} were asserted to it.
	 * 
	 * @param smtContext is the {@link SMTBasedContext} object to check satisfiability of
	 * @return
	 * 		true if the passed in context is satisfiable after assertion of the passed in {@link Expression}
	 * 		false otherwise
	 */
	public boolean 	contextIsSatisfiable(SMTBasedContext smtContext, Expression formula);
	
	/**
	 * Asserts the {@link Expression}s representing formulas to the passed in {@link SMTBasedContext}
	 * 
	 * @param smtContext is the {@link SMTBasedContext} object to be asserted onto
	 * @param smtFormula are the {@link Expression}s to assert
	 * @return the reference to the native SMT context object wrapped within smtContext after assertion
	 */
	public Object 	assertOntoContext(SMTBasedContext smtContext, Expression... formulas);
	
	/**
	 * If the passed in {@link SMTBasedContext} is satisfiable, gets a satisfiable model
	 * for the system.  The context provides the constraints, variables, and types from
	 * which to create a model.
	 * <p>
	 * Note that generated model is not necessarily the only possible satisfiable model.
	 * 
	 * @param smtContext the context for which to generate a model based on.
	 * @return
	 * 		an {@SMTModel} object representing a satisfiable solution to the system
	 */
	public SMTModel getModel(SMTBasedContext smtContext);
	
	/**
	 * If the passed in {@link SMTBasedContext} is satisfiable, generates a string representation of a 
	 * satisfiable model for the system.  The context provides the constraints, variables, and types from
	 * which to create a model.
	 * <p>
	 * Note that generated model is not necessarily the only possible satisfiable model.
	 * 
	 * @param smtContext the context for which to generate a model based on.
	 * @return
	 * 		A String representation of a satisfiable solution to the system.
	 */
	String 			getModelAsString(SMTBasedContext smtContext);
	
	/**
	 * Gets the unique value of a variable as an {@link Expression} if the variable has a unique value
	 * which satisfies the constraints on the passed in {@link SMTBasedContext}.
	 * 
	 * @param variable is the variable that a unique value is attempted to be determined for
	 * @param smtContext the context based on which the variable and its value are evaluated
	 * @return
	 * 		an {@link Expression} of the value of the variable if the variable has a unique value under the passed in context
	 * 		null otherwise
	 */
	Expression getValueOfVariable(Expression variable, SMTBasedContext smtContext);


}
