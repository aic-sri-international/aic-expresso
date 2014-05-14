package com.sri.ai.test.expresso;



import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;

public class DefaultSymbolTest {
	int oldPrecision;
	int oldScientificGreater;
	int oldScientificAfter;

	@Before
	public void setUp() {
		oldPrecision         = SyntaxTrees.setNumericDisplayPrecision(2);
		oldScientificGreater = SyntaxTrees.setDisplayScientificGreaterNIntegerPlaces(6);
		oldScientificAfter   = SyntaxTrees.setDisplayScientificAfterNDecimalPlaces(4); 
	}
	
	@After
	public void tearDown() {
		SyntaxTrees.setNumericDisplayPrecision(oldPrecision);
		SyntaxTrees.setDisplayScientificGreaterNIntegerPlaces(oldScientificGreater);
		SyntaxTrees.setDisplayScientificAfterNDecimalPlaces(oldScientificAfter);
	}
	
	@Test
	public void testPrecisionOutput() {
		
		//
		// Positive
		Assert.assertEquals("0.39",        Expressions.makeSymbol(0.3928208).toString());
		Assert.assertEquals("0.4",         Expressions.makeSymbol(0.3998208).toString());
		Assert.assertEquals("0.013",       Expressions.makeSymbol(0.0129).toString());
		Assert.assertEquals("123",         Expressions.makeSymbol(123).toString());
		Assert.assertEquals("10",          Expressions.makeSymbol(10.0143).toString());
		Assert.assertEquals("11",          Expressions.makeSymbol(10.926).toString());
		Assert.assertEquals("11",          Expressions.makeSymbol(11.0176).toString());
		Assert.assertEquals("12",          Expressions.makeSymbol(11.923).toString());
		Assert.assertEquals("19",          Expressions.makeSymbol(19.423).toString());
		Assert.assertEquals("20",          Expressions.makeSymbol(19.926).toString());		
		Assert.assertEquals("0.5",         Expressions.makeSymbol(0.5).toString());
		Assert.assertEquals("1",           Expressions.makeSymbol(0.999).toString());
		Assert.assertEquals("11",          Expressions.makeSymbol(10.999).toString());
		Assert.assertEquals("20",          Expressions.makeSymbol(19.999).toString());
		Assert.assertEquals("1",           Expressions.makeSymbol(0.999973).toString());
		Assert.assertEquals("100000",      Expressions.makeSymbol(100000.1).toString());
		Assert.assertEquals("100001",      Expressions.makeSymbol(100000.9).toString());
		Assert.assertEquals("123456",      Expressions.makeSymbol(123456.1).toString());
		Assert.assertEquals("123457",      Expressions.makeSymbol(123456.9).toString());
		Assert.assertEquals("19",          Expressions.makeSymbol(19.00000926).toString());
		Assert.assertEquals("1234567890",  Expressions.makeSymbol(1234567890).toString());
		
		
		//
		// Negative
		Assert.assertEquals("-0.39",        Expressions.makeSymbol(-0.3928208).toString());
		Assert.assertEquals("-0.4",         Expressions.makeSymbol(-0.3998208).toString());
		Assert.assertEquals("-0.013",       Expressions.makeSymbol(-0.0129).toString());
		Assert.assertEquals("-123",         Expressions.makeSymbol(-123).toString());
		Assert.assertEquals("-10",          Expressions.makeSymbol(-10.0143).toString());
		Assert.assertEquals("-11",          Expressions.makeSymbol(-10.926).toString());
		Assert.assertEquals("-11",          Expressions.makeSymbol(-11.0176).toString());
		Assert.assertEquals("-12",          Expressions.makeSymbol(-11.923).toString());
		Assert.assertEquals("-19",          Expressions.makeSymbol(-19.423).toString());
		Assert.assertEquals("-20",          Expressions.makeSymbol(-19.926).toString());		
		Assert.assertEquals("-0.5",         Expressions.makeSymbol(-0.5).toString());
		Assert.assertEquals("-1",           Expressions.makeSymbol(-0.999).toString());
		Assert.assertEquals("-11",          Expressions.makeSymbol(-10.999).toString());
		Assert.assertEquals("-20",          Expressions.makeSymbol(-19.999).toString());
		Assert.assertEquals("-1",           Expressions.makeSymbol(-0.999973).toString());
		Assert.assertEquals("-100000",      Expressions.makeSymbol(-100000.1).toString());
		Assert.assertEquals("-100001",      Expressions.makeSymbol(-100000.9).toString());
		Assert.assertEquals("-123456",      Expressions.makeSymbol(-123456.1).toString());
		Assert.assertEquals("-123457",      Expressions.makeSymbol(-123456.9).toString());
		Assert.assertEquals("-19",          Expressions.makeSymbol(-19.00000926).toString());
		Assert.assertEquals("-1234567890",  Expressions.makeSymbol(-1234567890).toString());
	}
	
	@Test
	public void testScientificOutput() {
		//
		// Positive
		Assert.assertEquals("1.2E-7",      Expressions.makeSymbol(0.000000123).toString());
		Assert.assertEquals("1E-9",        Expressions.makeSymbol(0.000000001).toString());
		Assert.assertEquals("1E6",         Expressions.makeSymbol(1000000.013).toString());
		Assert.assertEquals("1E6",         Expressions.makeSymbol(1000000.016).toString());
		Assert.assertEquals("1.000001E6",  Expressions.makeSymbol(1000000.9).toString());
		Assert.assertEquals("1.234567E6",  Expressions.makeSymbol(1234567.1).toString());
		Assert.assertEquals("1.234568E6",  Expressions.makeSymbol(1234567.9).toString());
		
		//
		// Negative
		Assert.assertEquals("-1.2E-7",      Expressions.makeSymbol(-0.000000123).toString());
		Assert.assertEquals("-1E-9",        Expressions.makeSymbol(-0.000000001).toString());
		Assert.assertEquals("-1E6",         Expressions.makeSymbol(-1000000.013).toString());
		Assert.assertEquals("-1E6",         Expressions.makeSymbol(-1000000.016).toString());
		Assert.assertEquals("-1.000001E6",  Expressions.makeSymbol(-1000000.9).toString());
		Assert.assertEquals("-1.234567E6",  Expressions.makeSymbol(-1234567.1).toString());
		Assert.assertEquals("-1.234568E6",  Expressions.makeSymbol(-1234567.9).toString());
	}
}
