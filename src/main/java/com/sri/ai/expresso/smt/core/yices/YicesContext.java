package com.sri.ai.expresso.smt.core.yices;

import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.expresso.smt.api.SMTFormula;
import com.sri.yices.Config;
import com.sri.yices.Model;
import com.sri.yices.Status;
import com.sri.yices.YicesException;

public class YicesContext implements SMTContext {
	
	// DATA MEMBERS 
	//////////////////////////////////////////////////////
	private final com.sri.yices.Context yicesContext;
	
	
	
	// CONSTRUCTORS 
	//////////////////////////////////////////////////////
	public YicesContext() {
		this.yicesContext = new com.sri.yices.Context();
	}
	
	public YicesContext(String contextConfiguration) {
		Config cfg = new Config();
        cfg.set("mode", contextConfiguration);
		this.yicesContext = new com.sri.yices.Context(cfg);
	}


	
	// OVERRIDING INHERITED METHODS : SMTContext 
	//////////////////////////////////////////////////////
	@Override
	public void assertOntoContext(SMTFormula smtFormula) {
		assert(smtFormula instanceof YicesFormula);
		int yicesFormula = (int) smtFormula.getFormula();
		this.yicesContext.assertFormula(yicesFormula);
	}
	
	@Override
	public void assertOntoContext(SMTFormula... smtFormula) {
		assert(smtFormula instanceof YicesFormula[]);
		int[] yicesFormulas = new int[smtFormula.length];
		int i = 0;
		for(SMTFormula yicesFormula : smtFormula) {
			yicesFormulas[i] = (int) yicesFormula.getFormula();
			++i;
		}
		this.yicesContext.assertFormulas(yicesFormulas);
	}

	@Override
	public void pushStackFrame() {
		this.yicesContext.push();
	}

	@Override
	public void popStackFrame() {
		this.yicesContext.pop();
	}

	@Override
	public boolean contextIsSatisfiable() {
		return (yicesContext.check() == Status.SAT);
	}
	
	@Override
	public String getModel() {
		String model;
		try{
			Model m = yicesContext.getModel();
			model = m.toString().replace("\n", "").replace("\r", "");
		}
		catch (YicesException e) {
			model = "Unable to extract model given the smt context state!!!";
		}
		return model;
	}
	
}
