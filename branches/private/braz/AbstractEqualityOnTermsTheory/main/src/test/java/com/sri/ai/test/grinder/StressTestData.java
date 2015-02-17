package com.sri.ai.test.grinder;

import java.util.List;

/**
 * An object representing the data for a stress test batch.
 *
 * @author braz
 */
public interface StressTestData {
	String getTitle();
	List<String> getProblemExpressions();
	String[] getExpectedExpressions();
}