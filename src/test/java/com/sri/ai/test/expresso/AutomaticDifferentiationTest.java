package com.sri.ai.test.expresso;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
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
		Assert.assertEquals(parse("1"), derivExpression("a","a"));
		Assert.assertEquals(parse("0"), derivExpression("b","a"));
		Assert.assertEquals(parse("0"), derivExpression("2","a")); 
	}
	
	@Test
	public void testPlus() {
		Assert.assertEquals(parse("1"), derivExpression("a+b","a"));
		Assert.assertEquals(parse("1"), derivExpression("a+b+c","a"));
		Assert.assertEquals(parse("1"), derivExpression("a+b+c","b"));
		Assert.assertEquals(parse("1"), derivExpression("a+b+c","c"));
		Assert.assertEquals(parse("1"), derivExpression("a+b+1","a"));
		
		Assert.assertEquals(parse("deriv(f(a),a)"), derivExpression("f(a)+b","a"));
		Assert.assertEquals(parse("1"), derivExpression("f(a)+b","b"));
		Assert.assertEquals(parse("deriv(f(a),a)+deriv(g(a),a)"), derivExpression("f(a)+g(a)","a"));
		Assert.assertEquals(parse("0"), derivExpression("f(a)+g(a)","b"));
	}
	
	@Test
	public void testMinus() {
		Assert.assertEquals(parse("-1"), derivExpression("-a","a"));
		Assert.assertEquals(parse("1"), derivExpression("a-b","a"));
		Assert.assertEquals(parse("-1"), derivExpression("a-b","b"));
		Assert.assertEquals(parse("-1"), derivExpression("-a-b","a"));
		Assert.assertEquals(parse("-1"), derivExpression("-a-b","b"));
		Assert.assertEquals(parse("1"), derivExpression("a-b-c","a"));
		Assert.assertEquals(parse("-1"), derivExpression("-a-b-c","b"));
		Assert.assertEquals(parse("1"), derivExpression("a-b-1","a"));
		
		Assert.assertEquals(parse("-deriv(f(a),a)"), derivExpression("-f(a)","a"));
		Assert.assertEquals(parse("deriv(f(a),a)"), derivExpression("f(a)-b","a"));
		Assert.assertEquals(parse("-1"), derivExpression("f(a)-b","b"));
		Assert.assertEquals(parse("deriv(f(a),a)-deriv(g(a),a)"), derivExpression("f(a)-g(a)","a"));
		Assert.assertEquals(parse("0"), derivExpression("f(a)-g(a)","b"));
		Assert.assertEquals(parse("deriv(f(g(a)),g(a))*deriv(g(a),a)-deriv(h(a),a)"), derivExpression("f(g(a))-h(a)","a"));
	}
	
	@Test
	public void testTimes() {
		Assert.assertEquals(parse("b"), derivExpression("a*b","a"));
		Assert.assertEquals(parse("b*c"), derivExpression("a*b*c","a"));
		Assert.assertEquals(parse("2*b"), derivExpression("2*a*b","a"));
		
		Assert.assertEquals(parse("2*deriv(f(a),a)"), derivExpression("2*f(a)","a"));
		Assert.assertEquals(parse("0"), derivExpression("2*f(a)","b"));
		
		Assert.assertEquals(parse("2*f(a)+2*a*deriv(f(a),a)"), derivExpression("2*a*f(a)","a"));
		Assert.assertEquals(parse("2*f(b)"), derivExpression("2*a*f(b)","a"));
		
		Assert.assertEquals(parse("deriv(f(a),a)*g(a)+f(a)*deriv(g(a),a)"), derivExpression("f(a)*g(a)","a"));
		Assert.assertEquals(parse("0"), derivExpression("f(a)*g(a)","b"));
		
		Assert.assertEquals(parse("(deriv(f(g(a)),g(a))*deriv(g(a),a))*h(a)+f(g(a))*deriv(h(a),a)"), derivExpression("f(g(a))*h(a)","a"));
	}
	
	@Test
	public void testDivision() {
		Assert.assertEquals(parse("b / (b * b)"), derivExpression("a/b","a"));
		Assert.assertEquals(parse("-a / (b * b)"), derivExpression("a/b","b"));
		Assert.assertEquals(parse("0"), derivExpression("a/b","c"));
		
		Assert.assertEquals(parse("(deriv(f(a),a)*b)/(b*b)"), derivExpression("f(a)/b","a"));
		Assert.assertEquals(parse("f(b)/(f(b)*f(b))"), derivExpression("a/f(b)","a"));
		
		Assert.assertEquals(parse("-1/(a*a)"), derivExpression("1/a","a"));
		Assert.assertEquals(parse("1 / 3"), parse(derivExpression("a/3","a").toString()));
	}
	
	@Test
	public void testExponentiation() {
		Assert.assertEquals(parse("a^b*(b/a)"), derivExpression("a^b","a"));
		Assert.assertEquals(parse("a^b*log(a)"), derivExpression("a^b","b"));
		
		Assert.assertEquals(parse("f(a)^b*((b/f(a))*deriv(f(a),a))"), derivExpression("f(a)^b","a"));
		Assert.assertEquals(parse("f(a)^b*log(f(a))"), derivExpression("f(a)^b","b"));
		Assert.assertEquals(parse("a^f(b)*(log(a)*deriv(f(b),b))"), derivExpression("a^f(b)","b"));
		
		Assert.assertEquals(parse("a^b*((b/a)*deriv(a, g(a)))"), derivExpression("a^b","g(a)"));
		Assert.assertEquals(parse("a^b*(log(a)*deriv(b, g(h(a), b)) + (b/a)*deriv(a, g(h(a), b)))"), derivExpression("a^b","g(h(a), b)"));
	}
	
	@Test
	public void testComposition() {
		Assert.assertEquals(parse("0"), derivExpression("f(g(a))","b"));
		Assert.assertEquals(parse("deriv(f(g(a)), g(a))*deriv(g(a), a)"), derivExpression("f(g(a))","a"));
		Assert.assertEquals(parse("deriv(f(g(a), 10), g(a))*deriv(g(a), a)"), derivExpression("f(g(a),10)","a"));
		Assert.assertEquals(parse("deriv(f(g(h(a))), g(h(a)))*(deriv(g(h(a)), h(a))*deriv(h(a), a))"), derivExpression("f(g(h(a)))","a"));
		Assert.assertEquals(parse("deriv(f(a*b), a*b)*b"), derivExpression("f(a*b)","a"));
	}
	
	@Test
	public void testExpression() {
		Assert.assertEquals(parse("deriv(f(a),a)"), derivExpression("f(a)","a"));
		Assert.assertEquals(parse("0"), derivExpression("f(a)","b"));
		
		Assert.assertEquals(parse("1"), derivExpression("a+b*(b+c)","a"));
		Assert.assertEquals(parse("1+b"), derivExpression("a+b*(c+a)","a"));
		Assert.assertEquals(parse("(a^b*(b/a)*(b+c*a)-a^b*c)/((b+c*a)*(b+c*a))"), parse(derivExpression("(a^b)/(b+c*a)","a").toString()));
		
	}
	
	public static Expression derivExpression(String s, String arg) {
		Expression expression = parse(s);
		Expression a = Expressions.parse(arg);
		return AutomaticDifferentiationTest.autoDifferentiator.differentiateExpression(expression, a);
	}

}
