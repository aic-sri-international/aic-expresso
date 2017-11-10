package com.sri.ai.test.expresso.helper;

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_ONE;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.getExpressionBeingMultipliedByMinusOneOrNull;
import static com.sri.ai.expresso.helper.Expressions.isMinusOne;
import static com.sri.ai.expresso.helper.Expressions.isPositiveOrNegativeInfinity;
import static com.sri.ai.expresso.helper.Expressions.isUnaryMinusOfOne;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.number.Times;
import com.sri.ai.grinder.sgdpllt.library.number.UnaryMinus;

public class ExpressionsTest {
	
	@Test
	public void testIsPositiveOrNegativeInfinity() {
		assertTrue(isPositiveOrNegativeInfinity(INFINITY));
		assertTrue(isPositiveOrNegativeInfinity(MINUS_INFINITY));
		assertTrue(isPositiveOrNegativeInfinity(Times.make(MINUS_ONE, INFINITY)));
		assertTrue(isPositiveOrNegativeInfinity(Times.make(UnaryMinus.make(ONE), INFINITY)));
		
		assertFalse(isPositiveOrNegativeInfinity(ONE));
		assertFalse(isPositiveOrNegativeInfinity(parse("f(a)")));
	}
	
	@Test
	public void testIsMinusOne() {
		assertTrue(isMinusOne(MINUS_ONE));
		assertTrue(isMinusOne(UnaryMinus.make(ONE)));
		assertTrue(isMinusOne(Times.make(MINUS_ONE, ONE)));
		assertTrue(isMinusOne(Times.make(ONE, MINUS_ONE)));
		assertTrue(isMinusOne(Times.make(UnaryMinus.make(ONE), ONE)));
		assertTrue(isMinusOne(Times.make(ONE, UnaryMinus.make(ONE))));
		
		assertFalse(isMinusOne(ONE));
		assertFalse(isMinusOne(parse("f(a)")));
	}
	
	@Test
	public void testIsUnaryMinusOfOne() {
		assertTrue(isUnaryMinusOfOne(Expressions.apply(FunctorConstants.MINUS, ONE)));

		assertFalse(isUnaryMinusOfOne(UnaryMinus.make(ONE))); // gets simplified to symbol
		assertFalse(isUnaryMinusOfOne(Times.make(ONE, UnaryMinus.make(ONE))));
		assertFalse(isUnaryMinusOfOne(ONE));
		assertFalse(isUnaryMinusOfOne(parse("f(a)")));
	}
	
	@Test
	public void testGetArgumentBeingMultipliedByMinusOneOrNull() {
		assertEquals(parse("5"), getExpressionBeingMultipliedByMinusOneOrNull(Expressions.makeSymbol(-5)));
		assertEquals(parse("5"), getExpressionBeingMultipliedByMinusOneOrNull(UnaryMinus.make(parse("5"))));
		assertEquals(parse("5"), getExpressionBeingMultipliedByMinusOneOrNull(Times.make(UnaryMinus.make(ONE),parse("5"))));
		assertEquals(parse("-5"), getExpressionBeingMultipliedByMinusOneOrNull(Times.make(UnaryMinus.make(ONE),parse("-5"))));
		assertEquals(parse("5"), getExpressionBeingMultipliedByMinusOneOrNull(Times.make(MINUS_ONE,parse("5"))));
		assertEquals(parse("5"), getExpressionBeingMultipliedByMinusOneOrNull(UnaryMinus.make(UnaryMinus.make(Times.make(MINUS_ONE,parse("5"))))));
		assertEquals(parse("5"), getExpressionBeingMultipliedByMinusOneOrNull(UnaryMinus.make(UnaryMinus.make(UnaryMinus.make(parse("5"))))));
		assertEquals(parse("5"), getExpressionBeingMultipliedByMinusOneOrNull(UnaryMinus.make(UnaryMinus.make(makeSymbol(-5)))));

		assertEquals(null, getExpressionBeingMultipliedByMinusOneOrNull(Expressions.makeSymbol(5)));
		assertEquals(null, getExpressionBeingMultipliedByMinusOneOrNull(UnaryMinus.make(parse("-5"))));
		assertEquals(parse("-5"), getExpressionBeingMultipliedByMinusOneOrNull(Times.make(MINUS_ONE,parse("-5"))));
		assertEquals(parse("-5"), getExpressionBeingMultipliedByMinusOneOrNull(Times.make(UnaryMinus.make(ONE),parse("-5"))));
		assertEquals(parse("-5"), getExpressionBeingMultipliedByMinusOneOrNull(UnaryMinus.make(UnaryMinus.make(Times.make(MINUS_ONE,parse("-5"))))));
		assertEquals(null, getExpressionBeingMultipliedByMinusOneOrNull(UnaryMinus.make(UnaryMinus.make(UnaryMinus.make(parse("-5"))))));
		assertEquals(null, getExpressionBeingMultipliedByMinusOneOrNull(UnaryMinus.make(UnaryMinus.make(parse("5")))));
		assertEquals(null, getExpressionBeingMultipliedByMinusOneOrNull(UnaryMinus.make(UnaryMinus.make(makeSymbol(5)))));
	}
}
