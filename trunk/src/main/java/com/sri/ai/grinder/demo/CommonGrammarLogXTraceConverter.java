package com.sri.ai.grinder.demo;

import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.util.log.LogXTraceConverter;

public class CommonGrammarLogXTraceConverter extends LogXTraceConverter {

	
	{
		// Ensure the grammar class passed in is used where necessary.
		BrewerConfiguration.setProperty(BrewerConfiguration.KEY_DEFAULT_GRAMMAR_CLASS, CommonGrammar.class.getName());
	}
}
