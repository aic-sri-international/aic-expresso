package com.sri.ai.test.grinder.polynomial;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.polynomial.api.Polynomial;
import com.sri.ai.grinder.polynomial.core.DefaultPolynomial;
import com.sri.ai.grinder.polynomial.core.PolynomialIntegration;

public class PolynomialIntegrationTest {

	@Test
	public void testIndefiniteIntegralsOfSingleTermPolynomials() {
		Assert.assertEquals(
				Expressions.parse("0"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("0"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("x"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("1"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("2*x"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("2"), Expressions.parse("x")));

		Assert.assertEquals(
				Expressions.parse("a*x"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("a"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("2*a*x"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("2*a"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("2*a*x"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("2*x"), Expressions.parse("a")));	
		
		Assert.assertEquals(
				Expressions.parse("0.5*a*x^2"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("a*x"), Expressions.parse("x")));

		Assert.assertEquals(
				makePolynomial("a*(x^3/3)"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("a*x^2"), Expressions.parse("x")));

		Assert.assertEquals(
				Expressions.parse("0.25*a*x^4"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("a*x^3"), Expressions.parse("x")));
		
		Assert.assertEquals(
				makePolynomial("0.2*a*x^5"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("a*x^4"), Expressions.parse("x")));
	
		Assert.assertEquals(
				Expressions.parse("0.5*a^3*x^2"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("a^3*x"), Expressions.parse("x")));

		Assert.assertEquals(
				makePolynomial("a^3*(x^3/3)"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("a^3*x^2"), Expressions.parse("x")));

		Assert.assertEquals(
				Expressions.parse("0.25*a^3*x^4"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("a^3*x^3"), Expressions.parse("x")));
		
		Assert.assertEquals(
				makePolynomial("0.2*a^3*x^5"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("a^3*x^4"), Expressions.parse("x")));
	}
	
	@Test
	public void testIndefiniteIntegralsOfMultiTermPolynomials() {
		Assert.assertEquals(
				makePolynomial("a*(x^3/3) + a*(x^2/2) + a*x + 5*x"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("a*x^2 + a*x + a + 5"), Expressions.parse("x")));		
	}
	
	@Test
	public void testIndefiniteIntegralOfMinusPower() {
		// NOTE: as the Polynomial API only support positive integer exponents the
		// implementations treats expressions like 'x^(a negative value)' 
		// as a single atomic factor
		Assert.assertEquals(
				makePolynomial("x*x^-1"), 
				PolynomialIntegration.indefiniteIntegral(makePolynomial("x^-1"), Expressions.parse("x")));
	}
	
	@Test
	public void testDefiniteIntegralsOfSingleTermPolynomials() {
		Assert.assertEquals(
				Expressions.parse("0"), 
				PolynomialIntegration.definiteIntegral(makePolynomial("0"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));
		
		Assert.assertEquals(
				makePolynomial("c + -1*b", "tuple(b, c, x)"), 
				PolynomialIntegration.definiteIntegral(makePolynomial("1"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));
		
		Assert.assertEquals(
				makePolynomial("2*c + -2*b", "tuple(b, c, x)"), 
				PolynomialIntegration.definiteIntegral(makePolynomial("2"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));

		Assert.assertEquals(
				makePolynomial("a*c + -1*a*b", "tuple(a, b, c, x)"), 
				PolynomialIntegration.definiteIntegral(makePolynomial("a"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));
		
		Assert.assertEquals(
				makePolynomial("2*a*c + -2*a*b", "tuple(a, b, c, x)"), 
				PolynomialIntegration.definiteIntegral(makePolynomial("2*a"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));
		
		Assert.assertEquals( 
				makePolynomial("2*c*x + -2*b*x", "tuple(a, b, c, x)"),
				PolynomialIntegration.definiteIntegral(makePolynomial("2*x"), Expressions.parse("a"), Expressions.parse("b"), Expressions.parse("c")));	
		
		Assert.assertEquals(
				makePolynomial("0.5*a*c^2 + -0.5*a*b^2", "tuple(a, b, c, x)"),
				PolynomialIntegration.definiteIntegral(makePolynomial("a*x"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));

		Assert.assertEquals(
				makePolynomial("a*(c^3/3) + -1*a*(b^3/3)"), 
				PolynomialIntegration.definiteIntegral(makePolynomial("a*x^2"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));

		Assert.assertEquals(
				makePolynomial("0.25*a*c^4 + -0.25*a*b^4", "tuple(a, b, c, x)"),
				PolynomialIntegration.definiteIntegral(makePolynomial("a*x^3"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));
		
		Assert.assertEquals(
				makePolynomial("0.2*a*c^5 + -0.2*a*b^5"), 
				PolynomialIntegration.definiteIntegral(makePolynomial("a*x^4"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));
	
		Assert.assertEquals( 
				makePolynomial("0.5*a^3*c^2 + -0.5*a^3*b^2", "tuple(a, b, c, x)"),
				PolynomialIntegration.definiteIntegral(makePolynomial("a^3*x"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));

		Assert.assertEquals(
				makePolynomial("a^3*(c^3/3) + -a^3*(b^3/3)", "tuple(a, b, c, x)"),
				PolynomialIntegration.definiteIntegral(makePolynomial("a^3*x^2"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));

		Assert.assertEquals(
				makePolynomial("0.25*a^3*c^4 + -0.25*a^3*b^4", "tuple(a, b, c, x)"),
				PolynomialIntegration.definiteIntegral(makePolynomial("a^3*x^3"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));
		
		Assert.assertEquals(
				makePolynomial("0.2*a^3*c^5 + -0.2*a^3*b^5", "tuple(a, b, c, x)"),
				PolynomialIntegration.definiteIntegral(makePolynomial("a^3*x^4"), Expressions.parse("x"), Expressions.parse("b"), Expressions.parse("c")));
	}
	
	@Test
	public void testDefiniteIntegralsOfMultiTermPolynomials() {
		Assert.assertEquals(
				makePolynomial("a*(c^3/3) + a*(c^2/2) + a*c + 5*c + -1*a*(b^3/3) + -1*a*(b^2/2) + -1*a*b + -5*b"), 
				PolynomialIntegration.definiteIntegral(makePolynomial("a*x^2 + a*x + a + 5"), Expressions.parse("x"),  Expressions.parse("b"), Expressions.parse("c")));		
	}
	
	@Test
	public void testDefiniteIntegralOfMinusPower() {
		// NOTE: as the Polynomial API only support positive integer exponents the
		// implementations treats expressions like 'x^(a negative value)' 
		// as a single atomic factor
		Assert.assertEquals(
				makePolynomial("c*x^-1 + -1*b*x^-1", "tuple(b, c, x, x^-1)"), 
				PolynomialIntegration.definiteIntegral(makePolynomial("x^-1"), Expressions.parse("x"),  Expressions.parse("b"),  Expressions.parse("c")));
	}
	
	private static Polynomial makePolynomial(String polynomial) {
		Polynomial result = DefaultPolynomial.make(Expressions.parse(polynomial));
		return result;
	}
	
	private static Polynomial makePolynomial(String polynomial, String tupleVariables) {
		Polynomial result = DefaultPolynomial.make(Expressions.parse(polynomial), Expressions.parse(tupleVariables).getArguments());
		return result;
	}
}
