package com.sri.ai.test.grinder;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.util.Util;

public class DefaultStressTestData implements StressTestData {

	protected String title = null;
	protected List<String> problemExpressions = new ArrayList<String>();
	protected String[] expectedExpressions = null;

	public DefaultStressTestData(String title, List<String> problemExpressions, String[] expectedExpressions) {
		super();
		this.title = title;
		this.problemExpressions = problemExpressions;
		if (expectedExpressions.length != this.problemExpressions.size()) {
			throw new IllegalArgumentException("'expected' is not the correct length; it should be " + this.problemExpressions.size());
		}
		this.expectedExpressions = expectedExpressions;
	}

	public DefaultStressTestData(String title, List<String> problemExpressions) {
		this(title, problemExpressions, Util.makeArrayFilledOutWith(AbstractGrinderTest.IGNORE_EXPECTED, problemExpressions.size()));
	}

	@Override
	public String getTitle() {
		return title;
	}

	@Override
	public List<String> getProblemExpressions() {
		return problemExpressions;
	}

	@Override
	public String[] getExpectedExpressions() {
		return expectedExpressions;
	}
}