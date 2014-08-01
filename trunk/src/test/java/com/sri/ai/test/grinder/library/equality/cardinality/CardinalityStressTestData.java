package com.sri.ai.test.grinder.library.equality.cardinality;

import java.util.List;

/**
 * An object representing the data for a cardinality stress test batch.
 *
 * @author braz
 */
public interface CardinalityStressTestData {
	String getTitle();
	List<String> getCardinalityExpressions();
	String[] getExpectedExpressions();
	int getMaximumFormulaLength();
}