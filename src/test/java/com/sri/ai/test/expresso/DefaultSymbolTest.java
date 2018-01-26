package com.sri.ai.test.expresso;

import java.math.MathContext;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.util.math.Rational;

public class DefaultSymbolTest {
	int oldPrecision;
	boolean oldDisplayExact;
	int oldScientificGreater;
	int oldScientificAfter;

	@Before
	public void setUp() {
		oldPrecision         = SyntaxTrees.setNumericDisplayPrecision(2);
		oldDisplayExact      = SyntaxTrees.setDisplayNumericsExactly(false);
		oldScientificGreater = SyntaxTrees.setDisplayScientificIfNumberOfIntegerPlacesIsGreaterThan(6);
		oldScientificAfter   = SyntaxTrees.setDisplayScientificIfNumberOfDecimalPlacesIsGreaterThan(4); 
		// Ensure we are in approximation mode (i.e. the default) for these tests
		Rational.resetApproximationConfiguration(true, MathContext.DECIMAL128.getPrecision(), MathContext.DECIMAL128.getRoundingMode());
	}
	
	@After
	public void tearDown() {
		SyntaxTrees.setNumericDisplayPrecision(oldPrecision);
		SyntaxTrees.setDisplayNumericsExactly(oldDisplayExact);
		SyntaxTrees.setDisplayScientificIfNumberOfIntegerPlacesIsGreaterThan(oldScientificGreater);
		SyntaxTrees.setDisplayScientificIfNumberOfDecimalPlacesIsGreaterThan(oldScientificAfter);
		Rational.resetApproximationConfigurationFromAICUtilConfiguration();
	}
	
	@Test
	public void testDisplayExact() {
		// NOTE: the tearDown method will set us back to the correct default.
		SyntaxTrees.setDisplayNumericsExactly(true);
		
		Assert.assertEquals("0.1", Expressions.makeSymbol(new Rational(1,10)).toString());
		Assert.assertEquals("1.1", Expressions.makeSymbol(new Rational(11,10)).toString());
		Assert.assertEquals("1/3", Expressions.makeSymbol(new Rational(1, 3)).toString());
		Assert.assertEquals("1/7", Expressions.makeSymbol(new Rational(1, 7)).toString());
		Assert.assertEquals("7076430434013521/18014398509481984", Expressions.makeSymbol(0.3928208).toString());
		
		Assert.assertEquals("-0.1", Expressions.makeSymbol(new Rational(-1,10)).toString());
		Assert.assertEquals("-1.1", Expressions.makeSymbol(new Rational(-11,10)).toString());
		Assert.assertEquals("-1/3", Expressions.makeSymbol(new Rational(-1, 3)).toString());
		Assert.assertEquals("-1/7", Expressions.makeSymbol(new Rational(-1, 7)).toString());
		Assert.assertEquals("-7076430434013521/18014398509481984", Expressions.makeSymbol(-0.3928208).toString());
	}
	
	@Test
	public void testDisplayLargeExact() {
		// NOTE: the tearDown method will set us back to the correct default.
		SyntaxTrees.setDisplayNumericsExactly(true);
		Rational largeRational = new Rational(3).pow(100000);
		Assert.assertEquals("1.334971414230401469458914390489782E+47712", Expressions.makeSymbol(largeRational).toString());
		
		largeRational = new Rational(3).pow(100000).divide(new Rational(7).pow(100));
		Assert.assertEquals("1.334971414230401469458914390489782E+47661/3234476509624757991344647769100217", Expressions.makeSymbol(largeRational).toString());
	}
	
	@Test
	public void testDisplayLargeApprox() {
		Rational largeRational = new Rational(3).pow(100000);
		Assert.assertEquals("1.33497E47712", Expressions.makeSymbol(largeRational).toString());
		
		largeRational = new Rational(3).pow(100000).divide(new Rational(7).pow(100));
		Assert.assertEquals("4.12732E47627", Expressions.makeSymbol(largeRational).toString());
	}
	
	@Test
	public void testPrecisionOutput() {			
		//
		// Positive
		Assert.assertEquals("100000",      Expressions.makeSymbol(100000).toString());
		Assert.assertEquals("1E6",         Expressions.makeSymbol(1000000).toString());
		Assert.assertEquals("1E7",         Expressions.makeSymbol(10000000).toString());
		
		Assert.assertEquals("0.1",         Expressions.makeSymbol(new Rational(1,10)).toString());
		Assert.assertEquals("0.01",        Expressions.makeSymbol(new Rational(1,100)).toString());
		Assert.assertEquals("0.001",       Expressions.makeSymbol(new Rational(1,1000)).toString());
		Assert.assertEquals("0.0001",      Expressions.makeSymbol(new Rational(1,10000)).toString());
		Assert.assertEquals("1E-5",        Expressions.makeSymbol(0.00001).toString());
		Assert.assertEquals("1E-6",        Expressions.makeSymbol(0.000001).toString());

		// When reading values as doubles, more decimal places are introduced and
		// scientific notation is deemed necessary
		Assert.assertEquals("1E-1",        Expressions.makeSymbol(0.1).toString());
		Assert.assertEquals("1E-2",        Expressions.makeSymbol(0.01).toString());
		Assert.assertEquals("1E-3",        Expressions.makeSymbol(0.001).toString());
		Assert.assertEquals("1E-4",        Expressions.makeSymbol(0.0001).toString());
		Assert.assertEquals("1E-5",        Expressions.makeSymbol(0.00001).toString());
		Assert.assertEquals("1E-6",        Expressions.makeSymbol(0.000001).toString());
		
		Assert.assertEquals("3.9E-1",      Expressions.makeSymbol(0.3928208).toString());
		Assert.assertEquals("4E-1",        Expressions.makeSymbol(0.3998208).toString());
		Assert.assertEquals("0.013",       Expressions.makeSymbol(new Rational(129, 10000)).toString()); // 0.0129
		Assert.assertEquals("123",         Expressions.makeSymbol(123).toString());
		Assert.assertEquals("10",          Expressions.makeSymbol(new Rational(100143, 10000)).toString()); // 10.143
		Assert.assertEquals("11",          Expressions.makeSymbol(new Rational(10926, 1000)).toString()); // 10.926
		Assert.assertEquals("11",          Expressions.makeSymbol(new Rational(110176, 10000)).toString()); // 11.0176
		Assert.assertEquals("12",          Expressions.makeSymbol(new Rational(11923, 1000)).toString()); // 11.923
		Assert.assertEquals("19",          Expressions.makeSymbol(new Rational(19423, 1000)).toString()); // 19.423
		Assert.assertEquals("20",          Expressions.makeSymbol(new Rational(19926, 1000)).toString());	// 19.926	
		Assert.assertEquals("0.5",         Expressions.makeSymbol(0.5).toString());
		Assert.assertEquals("1",           Expressions.makeSymbol(0.999).toString());
		Assert.assertEquals("11",          Expressions.makeSymbol(new Rational(10999, 1000)).toString()); // 10.999
		Assert.assertEquals("20",          Expressions.makeSymbol(new Rational(19999, 1000)).toString()); // 19.999
		Assert.assertEquals("1",           Expressions.makeSymbol(0.999973).toString());
		Assert.assertEquals("100000",      Expressions.makeSymbol(new Rational(1000001, 10)).toString()); // 100000.1
		Assert.assertEquals("100001",      Expressions.makeSymbol(new Rational(1000009, 10)).toString()); // 100000.9
		Assert.assertEquals("123456",      Expressions.makeSymbol(new Rational(1234561, 10)).toString()); // 123456.1
		Assert.assertEquals("123457",      Expressions.makeSymbol(new Rational(1234569, 10)).toString()); // 123456.9
		Assert.assertEquals("1.9E1",       Expressions.makeSymbol(new Rational(1900000926, 100000000)).toString()); // 19.00000926
		Assert.assertEquals("1.23457E9",   Expressions.makeSymbol(1234567890).toString());
		
		
		//
		// Negative
		Assert.assertEquals("-3.9E-1",      Expressions.makeSymbol(-0.3928208).toString());
		Assert.assertEquals("-4E-1",        Expressions.makeSymbol(-0.3998208).toString());
		Assert.assertEquals("-0.013",       Expressions.makeSymbol(new Rational(-129, 10000)).toString()); // 0.0129
		Assert.assertEquals("-123",         Expressions.makeSymbol(-123).toString());
		Assert.assertEquals("-10",          Expressions.makeSymbol(new Rational(-100143, 10000)).toString()); // 10.143
		Assert.assertEquals("-11",          Expressions.makeSymbol(new Rational(-10926, 1000)).toString()); // 10.926
		Assert.assertEquals("-11",          Expressions.makeSymbol(new Rational(-110176, 10000)).toString()); // 11.0176
		Assert.assertEquals("-12",          Expressions.makeSymbol(new Rational(-11923, 1000)).toString()); // 11.923
		Assert.assertEquals("-19",          Expressions.makeSymbol(new Rational(-19423, 1000)).toString()); // 19.423
		Assert.assertEquals("-20",          Expressions.makeSymbol(new Rational(-19926, 1000)).toString());	// 19.926	
		Assert.assertEquals("-0.5",         Expressions.makeSymbol(-0.5).toString());
		Assert.assertEquals("-1",           Expressions.makeSymbol(-0.999).toString());
		Assert.assertEquals("-11",          Expressions.makeSymbol(new Rational(-10999, 1000)).toString()); // 10.999
		Assert.assertEquals("-20",          Expressions.makeSymbol(new Rational(-19999, 1000)).toString()); // 19.999
		Assert.assertEquals("-1",           Expressions.makeSymbol(-0.999973).toString());
		Assert.assertEquals("-100000",      Expressions.makeSymbol(new Rational(-1000001, 10)).toString()); // 100000.1
		Assert.assertEquals("-100001",      Expressions.makeSymbol(new Rational(-1000009, 10)).toString()); // 100000.9
		Assert.assertEquals("-123456",      Expressions.makeSymbol(new Rational(-1234561, 10)).toString()); // 123456.1
		Assert.assertEquals("-123457",      Expressions.makeSymbol(new Rational(-1234569, 10)).toString()); // 123456.9
		Assert.assertEquals("-1.9E1",       Expressions.makeSymbol(new Rational(-1900000926, 100000000)).toString()); // 19.00000926
		Assert.assertEquals("-1.23457E9",   Expressions.makeSymbol(-1234567890).toString());
	}
	
	@Test
	public void testScientificOutput() {
		//
		// Positive
		Assert.assertEquals("1.2E-7",    Expressions.makeSymbol(0.000000123).toString());
		Assert.assertEquals("1E-9",      Expressions.makeSymbol(0.000000001).toString());
		Assert.assertEquals("1E6",       Expressions.makeSymbol(1000000.013).toString());
		Assert.assertEquals("1E6",       Expressions.makeSymbol(1000000.016).toString());
		Assert.assertEquals("1E6",       Expressions.makeSymbol(new Rational(10000009, 10)).toString()); // 1000000.9
		Assert.assertEquals("1.23457E6", Expressions.makeSymbol(1234567.1).toString());
		Assert.assertEquals("1.23457E6", Expressions.makeSymbol(1234567.9).toString());
		
		//
		// Negative
		Assert.assertEquals("-1.2E-7",    Expressions.makeSymbol(-0.000000123).toString());
		Assert.assertEquals("-1E-9",      Expressions.makeSymbol(-0.000000001).toString());
		Assert.assertEquals("-1E6",       Expressions.makeSymbol(-1000000.013).toString());
		Assert.assertEquals("-1E6",       Expressions.makeSymbol(-1000000.016).toString());
		Assert.assertEquals("-1E6",       Expressions.makeSymbol(new Rational(-10000009, 10)).toString()); // 1000000.9
		Assert.assertEquals("-1.23457E6", Expressions.makeSymbol(-1234567.1).toString());
		Assert.assertEquals("-1.23457E6", Expressions.makeSymbol(-1234567.9).toString());
	}
}
