package com.sri.ai.expresso.smt.api;

public interface SMTContext {
	
	public void assertOntoContext(SMTFormula smtFormula);
	public void assertOntoContext(SMTFormula... smtFormula);
	public void pushStackFrame();
	public void popStackFrame();
	public boolean contextIsSatisfiable();
	public String getModel();

}
