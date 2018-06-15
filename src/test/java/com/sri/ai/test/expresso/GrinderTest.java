package com.sri.ai.test.expresso;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;

public class GrinderTest {

	@Test
	public void testPastBugs() {
		Expression expression;
		Expression actual;
		Expression expected;
		Context context;
		
		// Previously, all equalities were being simplified by linear real arithmetic and different arithmetic theories
		// as if they were literals.
		// In the examples below, this led to (if I > 3 then X else Y) = Y being treated as a literal but then generating an exception
		// (since it does not conform to the properties expected from a literal).
		// This has been fixed by adding a check "isLiteral".
		context = new TrueContext(new CommonTheory());
		context = context.extendWithSymbolsAndTypes("X", "Real", "Y", "Real");
		expression = parse("there exists I in 1..5 : (if I > 3 then X else Y) = Y");
		expected = parse("true");
		actual = context.evaluate(expression);
		assertEquals(expected, actual);
		
		context = new TrueContext(new CommonTheory());
		context = context.extendWithSymbolsAndTypes("J", "Integer", "K", "Integer");
		expression = parse("there exists I in 1..5 : (if I > 3 then J else K) = K");
		expected = parse("true");
		actual = context.evaluate(expression);
		assertEquals(expected, actual);
		
		context = new TrueContext(new CommonTheory());
		context = context.extendWithSymbolsAndTypes("I", "Integer", "X", "Real", "Y", "Real");
		expression = parse("(if I > 3 then X else Y) = Y");
		expected = parse("if I > 3 then X = Y else true");
		actual = context.evaluate(expression);
		assertEquals(expected, actual);
		
		context = new TrueContext(new CommonTheory());
		context = context.extendWithSymbolsAndTypes("I", "Integer", "J", "Integer", "K", "Integer");
		expression = parse("(if I > 3 then J else K) = K");
		expected = parse("if I > 3 then J = K else true");
		actual = context.evaluate(expression);
		assertEquals(expected, actual);
	}

}
