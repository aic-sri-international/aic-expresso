package com.sri.ai.expresso.smt.api;

public interface SMTSymbol {
	
	public boolean isRegistered();
	public Object getSymbolReference();
	public Object getTypeReference();
	public String getSymbolName();
	public String getTypeName();

}
