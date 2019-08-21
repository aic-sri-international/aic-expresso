package com.sri.ai.expresso.smt.api;

public interface SMTTerm {
	
	public Object getEmeddedReference();
	public Object getCorrespondingTypeReference();
	public String getCorrespondingTypeName();

}
