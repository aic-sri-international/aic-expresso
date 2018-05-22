package com.sri.ai.test.expresso;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.autodifferentiation.AutomaticDifferentiation;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;

/**
 * Class to test automatic symbolic differentiation of {@link Expression}
 * 
 * @author Sarah Perrin
 */

public class AutomaticDifferentiationTest {
	
	private static AutomaticDifferentiation autoDifferentiator;
	
	@Before
	public void makeAutoDifferentiator() {
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		autoDifferentiator = new AutomaticDifferentiation(e -> context.evaluate(e));
	}
	
	@Test
	public void testSymbol() {
		assertEquals(parse("1"), derivative("a", "a"));
		assertEquals(parse("0"), derivative("b", "a"));
		assertEquals(parse("0"), derivative("2", "a")); 
	}
	
	@Test
	public void testPlus() {
		assertEquals(parse("1"), derivative("a + b", "a"));
		assertEquals(parse("1"), derivative("a + b + c", "a"));
		assertEquals(parse("1"), derivative("a + b + c", "b"));
		assertEquals(parse("1"), derivative("a + b + c", "c"));
		assertEquals(parse("1"), derivative("a + b + 1", "a"));
		
		assertEquals(parse("deriv(f(a),a)"), derivative("f(a) + b", "a"));
		assertEquals(parse("1"), derivative("f(a) + b", "b"));
		assertEquals(parse("deriv(f(a),a) + deriv(g(a),a)"), derivative("f(a) + g(a)", "a"));
		assertEquals(parse("0"), derivative("f(a) + g(a)", "b"));
	}
	
	@Test
	public void testMinus() {
		assertEquals(parse("-1"), derivative("-a", "a"));
		assertEquals(parse("1"), derivative("a-b", "a"));
		assertEquals(parse("-1"), derivative("a-b", "b"));
		assertEquals(parse("-1"), derivative("-a-b", "a"));
		assertEquals(parse("-1"), derivative("-a-b", "b"));
		assertEquals(parse("1"), derivative("a-b-c", "a"));
		assertEquals(parse("-1"), derivative("-a-b-c", "b"));
		assertEquals(parse("1"), derivative("a-b-1", "a"));
		
		assertEquals(parse("-deriv(f(a),a)"), derivative("-f(a)", "a"));
		assertEquals(parse("deriv(f(a),a)"), derivative("f(a)-b", "a"));
		assertEquals(parse("-1"), derivative("f(a)-b", "b"));
		assertEquals(parse("deriv(f(a),a)-deriv(g(a),a)"), derivative("f(a)-g(a)", "a"));
		assertEquals(parse("0"), derivative("f(a)-g(a)", "b"));
		assertEquals(parse("deriv(f(g(a)),g(a))*deriv(g(a),a)-deriv(h(a),a)"), derivative("f(g(a))-h(a)", "a"));
	}
	
	@Test
	public void testTimes() {
		assertEquals(parse("b"), derivative("a*b", "a"));
		assertEquals(parse("b*c"), derivative("a*b*c", "a"));
		assertEquals(parse("2*b"), derivative("2*a*b", "a"));
		
		assertEquals(parse("2*deriv(f(a),a)"), derivative("2*f(a)", "a"));
		assertEquals(parse("0"), derivative("2*f(a)", "b"));
		
		assertEquals(parse("2*f(a) + 2*a*deriv(f(a),a)"), derivative("2*a*f(a)", "a"));
		assertEquals(parse("2*f(b)"), derivative("2*a*f(b)", "a"));
		
		assertEquals(parse("deriv(f(a),a)*g(a) + f(a)*deriv(g(a),a)"), derivative("f(a)*g(a)", "a"));
		assertEquals(parse("0"), derivative("f(a)*g(a)", "b"));
		
		assertEquals(parse("(deriv(f(g(a)),g(a))*deriv(g(a),a))*h(a) + f(g(a))*deriv(h(a),a)"), derivative("f(g(a))*h(a)", "a"));
	}
	
	@Test
	public void testDivision() {
		assertEquals(parse("b / (b * b)"), derivative("a/b", "a"));
		assertEquals(parse("-a / (b * b)"), derivative("a/b", "b"));
		assertEquals(parse("0"), derivative("a/b", "c"));
		
		assertEquals(parse("(deriv(f(a),a)*b)/(b*b)"), derivative("f(a)/b", "a"));
		assertEquals(parse("f(b)/(f(b)*f(b))"), derivative("a/f(b)", "a"));
		
		assertEquals(parse("-1/(a*a)"), derivative("1/a", "a"));
		assertEquals(parse("1 / 3"), parse(derivative("a/3", "a").toString()));
	}
	
	@Test
	public void testExponentiation() {
		assertEquals(parse("a^b*(b/a)"), derivative("a^b", "a"));
		assertEquals(parse("a^b*log(a)"), derivative("a^b", "b"));
		
		assertEquals(parse("f(a)^b*((b/f(a))*deriv(f(a),a))"), derivative("f(a)^b", "a"));
		assertEquals(parse("f(a)^b*log(f(a))"), derivative("f(a)^b", "b"));
		assertEquals(parse("a^f(b)*(log(a)*deriv(f(b),b))"), derivative("a^f(b)", "b"));
		
		assertEquals(parse("a^b*((b/a)*deriv(a, g(a)))"), derivative("a^b", "g(a)"));
		assertEquals(parse("a^b*(log(a)*deriv(b, g(h(a), b)) + (b/a)*deriv(a, g(h(a), b)))"), derivative("a^b", "g(h(a), b)"));
	}
	
	@Test
	public void testComposition() {
		assertEquals(parse("0"), derivative("f(g(a))", "b"));
		assertEquals(parse("deriv(f(g(a)), g(a))*deriv(g(a), a)"), derivative("f(g(a))", "a"));
		assertEquals(parse("deriv(f(g(a), 10), g(a))*deriv(g(a), a)"), derivative("f(g(a),10)", "a"));
		assertEquals(parse("deriv(f(g(h(a))), g(h(a)))*(deriv(g(h(a)), h(a))*deriv(h(a), a))"), derivative("f(g(h(a)))", "a"));
		assertEquals(parse("deriv(f(a*b), a*b)*b"), derivative("f(a*b)", "a"));
	}
	
	@Test
	public void testExpression() {
		assertEquals(parse("deriv(f(a),a)"), derivative("f(a)", "a"));
		assertEquals(parse("0"), derivative("f(a)", "b"));
		
		assertEquals(parse("1"), derivative("a + b*(b + c)", "a"));
		assertEquals(parse("1 + b"), derivative("a + b*(c + a)", "a"));
		assertEquals(parse("(a^b*(b/a)*(b + c*a)-a^b*c)/((b + c*a)*(b + c*a))"), parse(derivative("(a^b)/(b + c*a)", "a").toString()));
		
	}
	
	public static Expression derivative(String expressionString, String withRespectToString) {
		Expression expression = parse(expressionString);
		Expression a = Expressions.parse(withRespectToString);
		Expression result = autoDifferentiator.differentiateExpression(expression, a);
		return result;
	}

}
