package com.sri.ai.test.expresso;

import java.math.MathContext;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.util.math.Rational;

public class DefaultSymbolTest {
	int oldExactPrecision;
	int oldApproximatePrecision;
	boolean oldDisplayExact;
	int oldScientificGreater;
	int oldScientificAfter;

	@Before
	public void setUp() {
		oldExactPrecision = ExpressoConfiguration.setDisplayNumericExactPrecisionForSymbols(1500);
		oldApproximatePrecision = ExpressoConfiguration.setDisplayNumericApproximationPrecisionForSymbols(2);
		oldDisplayExact      = ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(false);
		oldScientificGreater = ExpressoConfiguration.setDisplayScientificGreaterNIntegerPlaces(6);
		oldScientificAfter   = ExpressoConfiguration.setDisplayScientificAfterNDecimalPlaces(4); 
		// Ensure we are in approximation mode (i.e. the default) for these tests
		Rational.resetApproximationConfiguration(true, MathContext.DECIMAL128.getPrecision(), MathContext.DECIMAL128.getRoundingMode());
	}
	
	@After
	public void tearDown() {
		ExpressoConfiguration.setDisplayNumericExactPrecisionForSymbols(oldExactPrecision);
		ExpressoConfiguration.setDisplayNumericApproximationPrecisionForSymbols(oldApproximatePrecision);
		ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(oldDisplayExact);
		ExpressoConfiguration.setDisplayScientificGreaterNIntegerPlaces(oldScientificGreater);
		ExpressoConfiguration.setDisplayScientificAfterNDecimalPlaces(oldScientificAfter);
		Rational.resetApproximationConfigurationFromAICUtilConfiguration();
	}
	
	@Test
	public void testDisplayExact() {
		
		// NOTE: the tearDown method will set us back to the correct default.
		ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(true);
		Assert.assertEquals("0.1", Expressions.makeSymbol(new Rational(1,10)).toString());
		Assert.assertEquals("1.1", Expressions.makeSymbol(new Rational(11,10)).toString());
		Assert.assertEquals("2/3", Expressions.makeSymbol(new Rational(2,3)).toString());
		Assert.assertEquals("0.123456789", Expressions.makeSymbol(new Rational(123456789,1000000000)).toString());
		Assert.assertEquals("0.999999999999999",                  Expressions.makeSymbol(new Rational(999999999999999L, 1000000000000000L)).toString());
		Assert.assertEquals("1/3", Expressions.makeSymbol(new Rational(1, 3)).toString());
		Assert.assertEquals("1/7", Expressions.makeSymbol(new Rational(1, 7)).toString());
		
		Assert.assertEquals("-0.1", Expressions.makeSymbol(new Rational(-1,10)).toString());
		Assert.assertEquals("-1.1", Expressions.makeSymbol(new Rational(-11,10)).toString());
		Assert.assertEquals("-2/3", Expressions.makeSymbol(new Rational(-2,3)).toString());
		Assert.assertEquals("-0.123456789", Expressions.makeSymbol(new Rational(-123456789,1000000000)).toString());
		Assert.assertEquals("-0.999999999999999",                  Expressions.makeSymbol(new Rational(-999999999999999L, 1000000000000000L)).toString());
		Assert.assertEquals("-1/3", Expressions.makeSymbol(new Rational(-1, 3)).toString());
		Assert.assertEquals("-1/7", Expressions.makeSymbol(new Rational(-1, 7)).toString());
	}
	
	@Test
	public void testDisplayLargeExact() {

		// NOTE: the tearDown method will set us back to the correct default.
		ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(true);
		Rational largeRational = new Rational(3).pow(100000);
		Assert.assertEquals("1.334971414230401469458914390489782E47712", Expressions.makeSymbol(largeRational).toString());
		
		largeRational = new Rational(3).pow(100000).divide(new Rational(7).pow(100));
		Assert.assertEquals("1.334971414230401469458914390489782E+47661/3234476509624757991344647769100217", Expressions.makeSymbol(largeRational).toString());
	}
	
	@Test
	public void testDisplayLargeApproximateNumber() {

		Rational largeRational = new Rational(3).pow(100000);
		Assert.assertEquals("1.33E47712", Expressions.makeSymbol(largeRational).toString());
		
		largeRational = new Rational(3).pow(100000).divide(new Rational(7).pow(100));
		Assert.assertEquals("4.13E47627", Expressions.makeSymbol(largeRational).toString());
	}
	
	@Test
	public void testPrecisionOutput() {			

		Assert.assertEquals("12345.68",    Expressions.makeSymbol(12345.6789).toString());
		
		//
		// Positive
		Assert.assertEquals("100000",      Expressions.makeSymbol(100000).toString());
		Assert.assertEquals("1E6",         Expressions.makeSymbol(1000000).toString());
		Assert.assertEquals("1E7",         Expressions.makeSymbol(10000000).toString());
		
		Assert.assertEquals("0.1",         Expressions.makeSymbol(new Rational(1,10)).toString());
		Assert.assertEquals("0.01",        Expressions.makeSymbol(new Rational(1,100)).toString());
		Assert.assertEquals("1E-3",        Expressions.makeSymbol(new Rational(1,1000)).toString());
		Assert.assertEquals("1E-4",        Expressions.makeSymbol(new Rational(1,10000)).toString());
		Assert.assertEquals("1E-5",        Expressions.makeSymbol(0.00001).toString());
		Assert.assertEquals("1E-6",        Expressions.makeSymbol(0.000001).toString());

		Assert.assertEquals("0.39",        Expressions.makeSymbol(0.3928208).toString());
		Assert.assertEquals("0.4",         Expressions.makeSymbol(0.3998208).toString());
		Assert.assertEquals("0.01",        Expressions.makeSymbol(new Rational(129, 10000)).toString()); // 0.0129
		Assert.assertEquals("123",         Expressions.makeSymbol(123).toString());
		Assert.assertEquals("10.01",       Expressions.makeSymbol(new Rational(100143, 10000)).toString()); // 10.0143
		Assert.assertEquals("10.93",       Expressions.makeSymbol(new Rational(10926, 1000)).toString()); // 10.926
		Assert.assertEquals("0.5",         Expressions.makeSymbol(0.5).toString());
		Assert.assertEquals("0.99",        Expressions.makeSymbol(0.99).toString());
		Assert.assertEquals("1",           Expressions.makeSymbol(0.999).toString());
		Assert.assertEquals("11",          Expressions.makeSymbol(new Rational(10999, 1000)).toString()); // 10.999
		Assert.assertEquals("20",          Expressions.makeSymbol(new Rational(19999, 1000)).toString()); // 19.999
		Assert.assertEquals("19",          Expressions.makeSymbol(new Rational(1900000926, 100000000)).toString()); // 19.00000926
		Assert.assertEquals("1",           Expressions.makeSymbol(0.999973).toString());
		Assert.assertEquals("100000.1",    Expressions.makeSymbol(new Rational(1000001, 10)).toString()); // 100000.1
		Assert.assertEquals("100000.9",    Expressions.makeSymbol(new Rational(1000009, 10)).toString()); // 100000.9
		Assert.assertEquals("123456.1",    Expressions.makeSymbol(new Rational(1234561, 10)).toString()); // 123456.1
		Assert.assertEquals("123456.9",    Expressions.makeSymbol(new Rational(1234569, 10)).toString()); // 123456.9
		Assert.assertEquals("1.23E6",      Expressions.makeSymbol(new Rational(12345679, 10)).toString()); // 123456.9
		Assert.assertEquals("1.23E9",      Expressions.makeSymbol(1234567890).toString());
		Assert.assertEquals("1E11",        Expressions.makeSymbol(100234567890L).toString());

		Assert.assertEquals("0.01",        Expressions.makeSymbol(new Rational(1,100)).toString());
		Assert.assertEquals("1E-3",        Expressions.makeSymbol(new Rational(1,1000)).toString());
		Assert.assertEquals("1E-4",        Expressions.makeSymbol(new Rational(1,10000)).toString());
		Assert.assertEquals("1E-5",        Expressions.makeSymbol(new Rational(1,100000)).toString());
		
		//
		// Negative
		Assert.assertEquals("-0.39",        Expressions.makeSymbol(-0.3928208).toString());
		Assert.assertEquals("-0.4",         Expressions.makeSymbol(-0.3998208).toString());
		Assert.assertEquals("-0.01",        Expressions.makeSymbol(new Rational(-129, 10000)).toString()); // 0.0129
		Assert.assertEquals("-123",         Expressions.makeSymbol(-123).toString());
		Assert.assertEquals("-10.01",       Expressions.makeSymbol(new Rational(-100143, 10000)).toString()); // 10.143
		Assert.assertEquals("-10.93",       Expressions.makeSymbol(new Rational(-10926, 1000)).toString()); // 10.926
		Assert.assertEquals("-0.5",         Expressions.makeSymbol(-0.5).toString());
		Assert.assertEquals("-0.99",        Expressions.makeSymbol(-0.99).toString());
		Assert.assertEquals("-1",           Expressions.makeSymbol(-0.999).toString());
		Assert.assertEquals("-11",          Expressions.makeSymbol(new Rational(-10999, 1000)).toString()); // 10.999
		Assert.assertEquals("-20",          Expressions.makeSymbol(new Rational(-19999, 1000)).toString()); // 19.999
		Assert.assertEquals("-19",          Expressions.makeSymbol(new Rational(-1900000926, 100000000)).toString()); // 19.00000926
		Assert.assertEquals("-1",           Expressions.makeSymbol(-0.999973).toString());
		Assert.assertEquals("-100000.1",    Expressions.makeSymbol(new Rational(-1000001, 10)).toString()); // 100000.1
		Assert.assertEquals("-100000.9",    Expressions.makeSymbol(new Rational(-1000009, 10)).toString()); // 100000.9
		Assert.assertEquals("-123456.1",    Expressions.makeSymbol(new Rational(-1234561, 10)).toString()); // 123456.1
		Assert.assertEquals("-123456.9",    Expressions.makeSymbol(new Rational(-1234569, 10)).toString()); // 123456.9
		Assert.assertEquals("-1.23E6",      Expressions.makeSymbol(new Rational(-12345679, 10)).toString()); // 123456.9
		Assert.assertEquals("-1.23E9",      Expressions.makeSymbol(-1234567890).toString());
	}
	
	@Test
	public void testScientificNotationTriggeredByNumberOfDecimalPlaces() {

		ExpressoConfiguration.setDisplayNumericApproximationPrecisionForSymbols(10);
		ExpressoConfiguration.setDisplayScientificAfterNDecimalPlaces(4);
		Assert.assertEquals("0.001", Expressions.makeSymbol(new Rational(1,1000)).toString());
		Assert.assertEquals("0.0001", Expressions.makeSymbol(new Rational(1,10000)).toString());
		Assert.assertEquals("1E-5", Expressions.makeSymbol(new Rational(1,100000)).toString());
		Assert.assertEquals("1E-6", Expressions.makeSymbol(new Rational(1,1000000)).toString());

		ExpressoConfiguration.setDisplayNumericApproximationPrecisionForSymbols(3);
		ExpressoConfiguration.setDisplayScientificAfterNDecimalPlaces(12);
		Assert.assertEquals("0.001", Expressions.makeSymbol(new Rational(1,1000)).toString());
		Assert.assertEquals("1E-4", Expressions.makeSymbol(new Rational(1,10000)).toString());
		Assert.assertEquals("1E-5", Expressions.makeSymbol(new Rational(1,100000)).toString());
		Assert.assertEquals("1E-6", Expressions.makeSymbol(new Rational(1,1000000)).toString());

	}
}
