package com.sri.ai.test.grinder.library.equality.cardinality;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.util.Util;

public class DefaultCardinalityStressTestData implements CardinalityStressTestData {

	protected String title = null;
	protected List<String> cardinalityExpressions = new ArrayList<String>();
	protected String[] expectedExpressions = null;
	private int maximumFormulaLength = 0;

	public DefaultCardinalityStressTestData(String title, List<String> cardinalityExpressions, String[] expectedExpressions) {
		super();
		this.title = title;
		this.cardinalityExpressions = cardinalityExpressions;
		if (expectedExpressions.length != this.cardinalityExpressions.size()) {
			throw new IllegalArgumentException("'expected' is not the correct length; it should be " + this.cardinalityExpressions.size());
		}
		this.expectedExpressions = expectedExpressions;
		computeMaximumFormulaLength();
	}

	public DefaultCardinalityStressTestData(String title, List<String> cardinalityExpressions) {
		this(title, cardinalityExpressions, Util.makeArrayFilledOutWith(AbstractGrinderTest.IGNORE_EXPECTED, cardinalityExpressions.size()));
	}

	@Override
	public String getTitle() {
		return title;
	}

	@Override
	public List<String> getCardinalityExpressions() {
		return cardinalityExpressions;
	}

	@Override
	public String[] getExpectedExpressions() {
		return expectedExpressions;
	}

	public int getMaximumFormulaLength() {
		return maximumFormulaLength;
	}

	protected void computeMaximumFormulaLength() {
		for (int i = 0; i < cardinalityExpressions.size(); i++) {
			int length = cardinalityExpressions.get(i).length();
			if (length > maximumFormulaLength) {
				maximumFormulaLength = length;
			}
		}
	}
}