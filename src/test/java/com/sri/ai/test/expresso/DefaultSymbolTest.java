package com.sri.ai.test.expresso;



import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.core.DefaultSymbol;

public class DefaultSymbolTest {
	int oldPrecision;
	int oldScientific;

	@Before
	public void setUp() {
		oldPrecision  = DefaultSymbol.setNumericDisplayPrecision(2);
		oldScientific = DefaultSymbol.setDisplayScientificAfterNDecimalPlaces(4); 
	}
	
	@After
	public void tearDown() {
		DefaultSymbol.setNumericDisplayPrecision(oldPrecision);
		DefaultSymbol.setDisplayScientificAfterNDecimalPlaces(oldScientific);
	}
	
	@Test
	public void testPrecisionOutput() {
		
		//
		// Positive
		Assert.assertEquals("0.39",        DefaultSymbol.createSymbol(0.3928208).toString());
		Assert.assertEquals("0.4",         DefaultSymbol.createSymbol(0.3998208).toString());
		Assert.assertEquals("0.013",       DefaultSymbol.createSymbol(0.0129).toString());
		Assert.assertEquals("123",         DefaultSymbol.createSymbol(123).toString());
		Assert.assertEquals("10.012",      DefaultSymbol.createSymbol(10.0123).toString());
		Assert.assertEquals("10.013",      DefaultSymbol.createSymbol(10.0126).toString());
		Assert.assertEquals("10.92",       DefaultSymbol.createSymbol(10.923).toString());
		Assert.assertEquals("10.93",       DefaultSymbol.createSymbol(10.926).toString());
		Assert.assertEquals("19.92",       DefaultSymbol.createSymbol(19.923).toString());
		Assert.assertEquals("19.93",       DefaultSymbol.createSymbol(19.926).toString());		
		Assert.assertEquals("0.5",         DefaultSymbol.createSymbol(0.5).toString());
		Assert.assertEquals("1",           DefaultSymbol.createSymbol(0.999).toString());
		Assert.assertEquals("11",          DefaultSymbol.createSymbol(10.999).toString());
		Assert.assertEquals("1",           DefaultSymbol.createSymbol(0.999973).toString());
		
		
		//
		// Negative
		Assert.assertEquals("-0.39",        DefaultSymbol.createSymbol(-0.3928208).toString());
		Assert.assertEquals("-0.4",         DefaultSymbol.createSymbol(-0.3998208).toString());
		Assert.assertEquals("-0.013",       DefaultSymbol.createSymbol(-0.0129).toString());
		Assert.assertEquals("-123",         DefaultSymbol.createSymbol(-123).toString());
		Assert.assertEquals("-10.012",      DefaultSymbol.createSymbol(-10.0123).toString());
		Assert.assertEquals("-10.013",      DefaultSymbol.createSymbol(-10.0126).toString());
		Assert.assertEquals("-10.92",       DefaultSymbol.createSymbol(-10.923).toString());
		Assert.assertEquals("-10.93",       DefaultSymbol.createSymbol(-10.926).toString());
		Assert.assertEquals("-19.92",       DefaultSymbol.createSymbol(-19.923).toString());
		Assert.assertEquals("-19.93",       DefaultSymbol.createSymbol(-19.926).toString());		
		Assert.assertEquals("-0.5",         DefaultSymbol.createSymbol(-0.5).toString());
		Assert.assertEquals("-1",           DefaultSymbol.createSymbol(-0.999).toString());
		Assert.assertEquals("-11",          DefaultSymbol.createSymbol(-10.999).toString());
		Assert.assertEquals("-1",           DefaultSymbol.createSymbol(-0.999973).toString());
	}
	
	@Test
	public void testScientificOutput() {
		//
		// Positive
		Assert.assertEquals("1.2E-7",      DefaultSymbol.createSymbol(0.000000123).toString());
		Assert.assertEquals("1E-9",        DefaultSymbol.createSymbol(0.000000001).toString());
		Assert.assertEquals("1.000012E1",  DefaultSymbol.createSymbol(10.000123).toString());
		Assert.assertEquals("1.000013E1",  DefaultSymbol.createSymbol(10.000126).toString());
		Assert.assertEquals("1.000092E1",  DefaultSymbol.createSymbol(10.000923).toString());
		Assert.assertEquals("1.000093E1",  DefaultSymbol.createSymbol(10.000926).toString());
		Assert.assertEquals("1.900092E1",  DefaultSymbol.createSymbol(19.000923).toString());
		Assert.assertEquals("1.900093E1",  DefaultSymbol.createSymbol(19.000926).toString());

		
		//
		// Negative
		Assert.assertEquals("-1.2E-7",      DefaultSymbol.createSymbol(-0.000000123).toString());
		Assert.assertEquals("-1E-9",        DefaultSymbol.createSymbol(-0.000000001).toString());
		Assert.assertEquals("-1.000012E1",  DefaultSymbol.createSymbol(-10.000123).toString());
		Assert.assertEquals("-1.000013E1",  DefaultSymbol.createSymbol(-10.000126).toString());
		Assert.assertEquals("-1.000092E1",  DefaultSymbol.createSymbol(-10.000923).toString());
		Assert.assertEquals("-1.000093E1",  DefaultSymbol.createSymbol(-10.000926).toString());
		Assert.assertEquals("-1.900092E1",  DefaultSymbol.createSymbol(-19.000923).toString());
		Assert.assertEquals("-1.900093E1",  DefaultSymbol.createSymbol(-19.000926).toString());
	}
}
