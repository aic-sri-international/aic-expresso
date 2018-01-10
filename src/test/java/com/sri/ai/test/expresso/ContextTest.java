package com.sri.ai.test.expresso;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.util.base.Triple;

public class ContextTest {
	@Test
	public void testExtendWith() {
		Context context;
		ExtensionalIndexExpressionsSet indexExpressions;
		ExtensionalIndexExpressionsSet expectedNewIndexExpressionsSet;
		Triple<Context, ExtensionalIndexExpressionsSet, Expression> triple;
		Map<Expression, Expression> symbolsAndTypes;
		Expression expressionInScope;
		Expression expectedNewExpressionInScope;
		
		context = new TrueContext();
		indexExpressions = new ExtensionalIndexExpressionsSet("X", "Integer", "Y", "X..10");
		expectedNewIndexExpressionsSet = new ExtensionalIndexExpressionsSet("X'", "Integer", "Y'", "X'..10");
		expressionInScope = parse("X = 1 and Y = 2");
		expectedNewExpressionInScope = parse("X' = 1 and Y' = 2");
		symbolsAndTypes = 
				map(
						parse("X"), parse("Integer"),
						parse("Y"), parse("X..10"),
						parse("X'"), parse("Integer"),
						parse("Y'"), parse("X'..10")
				);
		context = context.extendWith(indexExpressions);
		triple = context.extendWith(indexExpressions, expressionInScope);
		println(triple);
		
		assertEquals(symbolsAndTypes, triple.first.getSymbolsAndTypes());
		assertEquals(expectedNewIndexExpressionsSet, triple.second);
		assertEquals(expectedNewExpressionInScope, triple.third);
	}
}
