package com.sri.ai.test.grinder.sgdpllt.theory.differencearithmetic;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticLiteralSide.DifferenceArithmeticLiteralSideException;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.LinearExpression;

/** 
 * @author braz
 */
public class LinearExpressionTest {

//	@Test
//	public void testNegate() {
//		Expression expression;
//		Expression expected;
//		
//		expression = parse("X");
//		expected = parse("-X");
//		runNegateTest(expression, expected);
//		
//		expression = parse("-X");
//		expected = parse("X");
//		runNegateTest(expression, expected);
//		
//		
//		
//		expression = parse("X - Y");
//		expected = parse("Y - X");
//		runNegateTest(expression, expected);
//		
//		expression = parse("-X + Y");
//		expected = parse("X - Y");
//		runNegateTest(expression, expected);
//		
//		
//		
//		expression = parse("15");
//		expected = parse("-15");
//		runNegateTest(expression, expected);
//		
//		expression = parse("-15");
//		expected = parse("15");
//		runNegateTest(expression, expected);
//
//		
//		
//		expression = parse("X + 15");
//		expected = parse("-X - 15");
//		runNegateTest(expression, expected);
//		
//		expression = parse("-X + 15");
//		expected = parse("X - 15");
//		runNegateTest(expression, expected);
//		
//		expression = parse("X - Y + 15");
//		expected = parse("Y - X - 15");
//		runNegateTest(expression, expected);
//		
//		expression = parse("-X + Y + 15");
//		expected = parse("X - Y - 15");
//		runNegateTest(expression, expected);
//
//		expression = parse("X - 15");
//		expected = parse("-X + 15");
//		runNegateTest(expression, expected);
//		
//		expression = parse("-X - 15");
//		expected = parse("X + 15");
//		runNegateTest(expression, expected);
//		
//		expression = parse("X - Y - 15");
//		expected = parse("Y - X + 15");
//		runNegateTest(expression, expected);
//		
//		expression = parse("-X + Y - 15");
//		expected = parse("X - Y + 15");
//		runNegateTest(expression, expected);
//	}
//
//	/**
//	 * @param expression
//	 * @param expected
//	 */
//	public void runNegateTest(Expression expression, Expression expected) {
//		LinearExpression literalSide = makeLinearExpression(expression);
//		LinearExpression negated = literalSide.negate();
//		assertEquals(expected, negated);
//	}
//	
//	@Test
//	public void testValidAdd() {
//		Expression expression1;
//		Expression expression2;
//		Expression expected;
//		
//		expression1 = parse("X");
//		expression2 = parse("-Y");
//		expected = parse("X - Y");
//		runValidAddTest(expression1, expression2, expected);
//		
//		expression1 = parse("-X");
//		expression2 = parse("Y");
//		expected = parse("Y - X");
//		runValidAddTest(expression1, expression2, expected);
//		
//		expression1 = parse("X");
//		expression2 = parse("-1");
//		expected = parse("X - 1");
//		runValidAddTest(expression1, expression2, expected);
//		
//		expression1 = parse("-X");
//		expression2 = parse("-1");
//		expected = parse("-X - 1");
//		runValidAddTest(expression1, expression2, expected);
//		
//		expression1 = parse("-1");
//		expression2 = parse("-1");
//		expected = parse("-2");
//		runValidAddTest(expression1, expression2, expected);
//	}
//
//	/**
//	 * @param expression1
//	 * @param expression2
//	 * @param expected
//	 */
//	public void runValidAddTest(Expression expression1, Expression expression2, Expression expected) {
//		LinearExpression literalSide1 = makeLinearExpression(expression1);
//		LinearExpression literalSide2 = makeLinearExpression(expression2);
//		LinearExpression addition = null;
//		try {
//			addition = literalSide1.add(literalSide2);
//		}
//		catch (DifferenceArithmeticLiteralSideException e) {
//			fail("Addtion of " + literalSide1 + " and " + literalSide2 + " is invalid.");
//		}
//		assertEquals(expected, addition);
//	}
//	
//	@Test
//	public void testInvalidAdd() {
//		Expression expression1;
//		Expression expression2;
//		
//		expression1 = parse("X");
//		expression2 = parse("Y");
//		runInvalidAddTest(expression1, expression2);
//		
//		expression1 = parse("-X");
//		expression2 = parse("-Y");
//		runInvalidAddTest(expression1, expression2);
//	}
//
//	/**
//	 * @param expression1
//	 * @param expression2
//	 */
//	public void runInvalidAddTest(Expression expression1, Expression expression2) {
//		LinearExpression literalSide1 = makeLinearExpression(expression1);
//		LinearExpression literalSide2 = makeLinearExpression(expression2);
//		LinearExpression addition = null;
//		try {
//			addition = literalSide1.add(literalSide2);
//			fail("Addition of " + literalSide1 + " and " + literalSide2 + " should have been invalid but resulted in " + addition);
//		}
//		catch (DifferenceArithmeticLiteralSideException e) {
//			// success!
//		}
//	}
//	
//	@Test
//	public void testValidSubtract() {
//		Expression expression1;
//		Expression expression2;
//		Expression expected;
//		
//		expression1 = parse("X");
//		expression2 = parse("Y");
//		expected = parse("X - Y");
//		runValidSubtractTest(expression1, expression2, expected);
//		
//		expression1 = parse("-X");
//		expression2 = parse("-Y");
//		expected = parse("Y - X");
//		runValidSubtractTest(expression1, expression2, expected);
//		
//		expression1 = parse("X");
//		expression2 = parse("-1");
//		expected = parse("X + 1");
//		runValidSubtractTest(expression1, expression2, expected);
//		
//		expression1 = parse("-X");
//		expression2 = parse("-1");
//		expected = parse("-X + 1");
//		runValidSubtractTest(expression1, expression2, expected);
//		
//		expression1 = parse("-1");
//		expression2 = parse("-1");
//		expected = parse("0");
//		runValidSubtractTest(expression1, expression2, expected);
//	}
//
//	/**
//	 * @param expression1
//	 * @param expression2
//	 * @param expected
//	 */
//	public void runValidSubtractTest(Expression expression1, Expression expression2, Expression expected) {
//		LinearExpression literalSide1 = makeLinearExpression(expression1);
//		LinearExpression literalSide2 = makeLinearExpression(expression2);
//		LinearExpression subtraction = null;
//		try {
//			subtraction = literalSide1.subtract(literalSide2);
//		}
//		catch (DifferenceArithmeticLiteralSideException e) {
//			fail("Subtraction of " + literalSide1 + " and " + literalSide2 + " is invalid.");
//		}
//		assertEquals(expected, subtraction);
//	}
//	
//	@Test
//	public void testInvalidSubtract() {
//		Expression expression1;
//		Expression expression2;
//		
//		expression1 = parse("-X");
//		expression2 = parse("Y");
//		runInvalidSubtractTest(expression1, expression2);
//		
//		expression1 = parse("-X");
//		expression2 = parse("Y");
//		runInvalidSubtractTest(expression1, expression2);
//	}
//
//	/**
//	 * @param expression1
//	 * @param expression2
//	 */
//	public void runInvalidSubtractTest(Expression expression1, Expression expression2) {
//		LinearExpression literalSide1 = makeLinearExpression(expression1);
//		LinearExpression literalSide2 = makeLinearExpression(expression2);
//		LinearExpression subtraction = null;
//		try {
//			subtraction = literalSide1.subtract(literalSide2);
//			fail("Subtraction of " + literalSide1 + " and " + literalSide2 + " should have been invalid but resulted in " + subtraction);
//		}
//		catch (DifferenceArithmeticLiteralSideException e) {
//			// success!
//		}
//	}
//	
//	@Test
//	public void testValidCases() {
//		Expression expression;
//		Expression expected;
//		
//		expression = parse("X - Y");
//		expected = parse("X - Y");
//		runValidCaseTest(expression, expected);
//		
//		expression = parse("X - 3");
//		expected = parse("X - 3");
//		runValidCaseTest(expression, expected);
//		
//		expression = parse("X");
//		expected = parse("X");
//		runValidCaseTest(expression, expected);
//
//		expression = parse("X - Y - 3");
//		expected = parse("X - Y - 3");
//		runValidCaseTest(expression, expected);
//
//		expression = parse("X - Y + 3");
//		expected = parse("X - Y + 3");
//		runValidCaseTest(expression, expected);
//
//		expression = parse("-X + Y - 3");
//		expected = parse("Y - X - 3");
//		runValidCaseTest(expression, expected);
//
//		expression = parse("-3 -X + Y");
//		expected = parse("Y - X - 3");
//		runValidCaseTest(expression, expected);
//
//		expression = parse("3 -X + Y");
//		expected = parse("Y - X + 3");
//		runValidCaseTest(expression, expected);
//	}
//
//	/**
//	 * @param expression
//	 * @param expected
//	 */
//	public void runValidCaseTest(Expression expression, Expression expected) {
//		LinearExpression literalSide = makeLinearExpression(expression);
//		assertEquals(expected, literalSide);
//	}
//
//	private static LinearExpression makeLinearExpression(Expression expression) {
//		LinearExpression literalSide;
//		literalSide = null;
//		try {
//			literalSide = new LinearExpression(expression);
//		} catch (DifferenceArithmeticLiteralSideException e) {
//			fail("Unexpected exception " + e);
//		}
//		return literalSide;
//	}
//	
}