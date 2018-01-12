package com.sri.ai.test.grinder.library.set;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.set.CountingFormulaEquivalentExpressions;

public class CountingFormulaEquivalentExpressionsTest {
	
	@Test
	public void testGetType() {
		Assert.assertTrue(CountingFormulaEquivalentExpressions.getType().equals("Integer"));
	}

	@Test
	public void testIsCountingFormulaEquivalentExpression() {
		// Actual counting formuals
		Assert.assertTrue(CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(Expressions.parse("| : X < 5 |")));
		Assert.assertTrue(CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(Expressions.parse("| X in 1..10 : X < 5 |")));
		// Cardinality function applications on intensional multisets.
		Assert.assertTrue(CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(Expressions.parse("| {{ (on X in 1..10)  tuple(X)  : X < 5 }} |")));
		Assert.assertTrue(CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(Expressions.parse("| {{ (on X in 1..10)  p(X)  : X < 5 }} |")));
		// Cardinality function applications on intensional unisets whose head is a tuple over the indices.
		Assert.assertTrue(CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(Expressions.parse("| { (on X in 1..10)  tuple(X)  : X < 5 } |")));
		
		// Are not counting formula equivalent expressions
		Assert.assertFalse(CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(Expressions.parse("| People |")));
		Assert.assertFalse(CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(Expressions.parse("| p(a) |")));
		Assert.assertFalse(CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(Expressions.parse("| { a, b, c } |")));
		Assert.assertFalse(CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(Expressions.parse("| {{ a, b, c }} |")));
		Assert.assertFalse(CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(Expressions.parse("| { (on X in 1..10)  p(X) : X < 5 } |")));
	}
	
	@Test
	public void testGetCondition() {
		Assert.assertEquals(Expressions.parse("X < 5"), CountingFormulaEquivalentExpressions.getCondition(Expressions.parse("| X in 1..10 : X < 5 |")));
		Assert.assertEquals(Expressions.parse("X < 5"), CountingFormulaEquivalentExpressions.getCondition(Expressions.parse("| {{ (on X in 1..10)  tuple(X)  : X < 5 }} |")));
		Assert.assertEquals(Expressions.parse("X < 5"), CountingFormulaEquivalentExpressions.getCondition(Expressions.parse("| {{ (on X in 1..10)  p(X)  : X < 5 }} |")));
		Assert.assertEquals(Expressions.parse("X < 5"), CountingFormulaEquivalentExpressions.getCondition(Expressions.parse("| { (on X in 1..10)  tuple(X)  : X < 5 } |")));
	}
}