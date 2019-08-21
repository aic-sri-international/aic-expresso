package com.sri.ai.expresso.smt.api;

public interface SMTContext {
	
	public SMTContext assertOntoContext(SMTFormula smtFormula);
	public SMTContext assertOntoContext(SMTFormula... smtFormulas);
	public SMTContext pushStackFrame();
	public SMTContext popStackFrame();
	public boolean contextIsSatisfiable();
	public String getModel();

}
