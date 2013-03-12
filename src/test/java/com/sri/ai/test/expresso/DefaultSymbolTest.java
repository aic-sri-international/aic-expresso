package com.sri.ai.test.expresso;



import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.core.DefaultSymbol;

public class DefaultSymbolTest {
	int oldPrecision;

	@Before
	public void setUp() {
		oldPrecision = DefaultSymbol.setNumericDisplayPrecision(2);
	}
	
	@After
	public void tearDown() {
		DefaultSymbol.setNumericDisplayPrecision(oldPrecision);
	}
	
	@Test
	public void testPrecisionOutput() {
		
		//
		// Positive
		Assert.assertEquals("0.39",        DefaultSymbol.createSymbol(0.3928208).toString());
		Assert.assertEquals("0.4",         DefaultSymbol.createSymbol(0.3998208).toString());
		Assert.assertEquals("0.013",       DefaultSymbol.createSymbol(0.0129).toString());
		Assert.assertEquals("0.00000012",  DefaultSymbol.createSymbol(0.000000123).toString());
		Assert.assertEquals("123",         DefaultSymbol.createSymbol(123).toString());
		Assert.assertEquals("10.012",      DefaultSymbol.createSymbol(10.0123).toString());
		Assert.assertEquals("10.013",      DefaultSymbol.createSymbol(10.0126).toString());
		Assert.assertEquals("10.92",       DefaultSymbol.createSymbol(10.923).toString());
		Assert.assertEquals("10.93",       DefaultSymbol.createSymbol(10.926).toString());
		Assert.assertEquals("19.92",       DefaultSymbol.createSymbol(19.923).toString());
		Assert.assertEquals("19.93",       DefaultSymbol.createSymbol(19.926).toString());
		Assert.assertEquals("0.000000001", DefaultSymbol.createSymbol(0.000000001).toString());
		Assert.assertEquals("0.5",         DefaultSymbol.createSymbol(0.5).toString());
		
		//
		// Negative
		Assert.assertEquals("-0.39",        DefaultSymbol.createSymbol(-0.3928208).toString());
		Assert.assertEquals("-0.4",         DefaultSymbol.createSymbol(-0.3998208).toString());
		Assert.assertEquals("-0.013",       DefaultSymbol.createSymbol(-0.0129).toString());
		Assert.assertEquals("-0.00000012",  DefaultSymbol.createSymbol(-0.000000123).toString());
		Assert.assertEquals("-123",         DefaultSymbol.createSymbol(-123).toString());
		Assert.assertEquals("-10.012",      DefaultSymbol.createSymbol(-10.0123).toString());
		Assert.assertEquals("-10.013",      DefaultSymbol.createSymbol(-10.0126).toString());
		Assert.assertEquals("-10.92",       DefaultSymbol.createSymbol(-10.923).toString());
		Assert.assertEquals("-10.93",       DefaultSymbol.createSymbol(-10.926).toString());
		Assert.assertEquals("-19.92",       DefaultSymbol.createSymbol(-19.923).toString());
		Assert.assertEquals("-19.93",       DefaultSymbol.createSymbol(-19.926).toString());
		Assert.assertEquals("-0.000000001", DefaultSymbol.createSymbol(-0.000000001).toString());
		Assert.assertEquals("-0.5",         DefaultSymbol.createSymbol(-0.5).toString());
	}
}
