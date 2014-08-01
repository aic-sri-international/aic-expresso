package com.sri.ai.test.grinder.library.equality.cardinality;

import java.util.ArrayList;
import java.util.List;

public class DefaultCardinalityStressTestData implements CardinalityStressTestData {

	protected String title = null;
	protected List<String> cardinalityExpressions = new ArrayList<String>();
	protected String[] expectedExpressions = null;
	private int maxFormulaLength = 0;

	public DefaultCardinalityStressTestData() {
		super();
	}

	protected void computeMaximumFormulaLength() {
		for (int i = 0; i < cardinalityExpressions.size(); i++) {
			int length = cardinalityExpressions.get(i).length();
			if (length > maxFormulaLength) {
				maxFormulaLength = length;
			}
		}
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

	@Override
	public int getMaximumFormulaLength() {
		return maxFormulaLength;
	}

}