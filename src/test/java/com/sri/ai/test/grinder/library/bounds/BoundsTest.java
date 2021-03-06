package com.sri.ai.test.grinder.library.bounds;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.TWO;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.anytime.Model;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.bounds.Bound;
import com.sri.ai.grinder.library.bounds.Bounds;
import com.sri.ai.grinder.library.bounds.DefaultExtensionalBound;
import com.sri.ai.grinder.library.bounds.DefaultIntensionalBound;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.theory.tuple.TupleTheory;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;

public class BoundsTest {
	
	Theory theory ;
	Context context;
	Bound setOfNumbers;
	Bound setOfFactors;
	Bound extensionalBound;
	Bound intensionalBound;
	Bound intensionalSetOfFactors1;
	Bound intensionalSetOfFactors2;
	Bound intensionalSetOfFactors3;
	Model model;

	@Before
	public void declareTheoryContextAndSetOfFactors() {
		
		ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(true);
		
		extensionalBound = new DefaultExtensionalBound();
		intensionalBound = new DefaultIntensionalBound();
		
		theory = new CompoundTheory(
								new EqualityTheory(false, true),
								new DifferenceArithmeticTheory(false, false),
								new LinearRealArithmeticTheory(false, false),
								new TupleTheory(),
								new PropositionalTheory());	
		
		context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes("X", "Boolean");
		context = context.extendWithSymbolsAndTypes("Y", "Boolean");
		context = context.extendWithSymbolsAndTypes("A", "Boolean");
		context = context.extendWithSymbolsAndTypes("B", "Boolean");
		context = context.extendWithSymbolsAndTypes("C", "1..5");
		// context = context.extendWithSymbolsAndTypes("D","{1,3,4,8}");

		// Set of functions
		Expression phi1 = parse("if X = true then 1 else if Y = true then 2 else 3");
		Expression phi2 = parse("if A = true then if Y = true then 4 else 5 else 6");
		Expression phi3 = parse("if X = true then 7 else if B = true then 8 else 9");
		Expression phi4 = parse("if B = true then 10 else if A = true then 11 else 12");
		Expression phi5 = parse("if C < 4 then 10 else if C = 4 then 11 else 12");
		
		/*
		 * This is how we create a non empty extensional bound
		 */
		setOfFactors = new DefaultExtensionalBound(arrayList(phi1, phi2, phi3, phi4, phi5)); 
	
		// Set of numbers
		setOfNumbers = new DefaultExtensionalBound(arrayList(ONE, TWO));

		Set<Expression> factor = new HashSet<Expression>();
		
		model = new Model(factor);
		
		model.context =	model.context.extendWithSymbolsAndTypes("A", "Boolean");
		model.context =	model.context.extendWithSymbolsAndTypes("B", "Boolean");
		model.context =	model.context.extendWithSymbolsAndTypes("Q", "Boolean");
		model.context =	model.context.extendWithSymbolsAndTypes("C", "1..4");
		model.context =	model.context.extendWithSymbolsAndTypes("D", "6..9");		
		
		intensionalSetOfFactors1 = new DefaultIntensionalBound(
				arrayList(
						parse("A' in Boolean"), 
						parse("C' in 1..5")
						),
				parse("if C = C' then if A = A' then 1 else 4 else 0"),
				TRUE
				);
		
		intensionalSetOfFactors2 = DefaultIntensionalBound.simplex(arrayList(parse("A")), model);
		intensionalSetOfFactors3 = DefaultIntensionalBound.simplex(arrayList(parse("C"), parse("B")), model);
	}
	
	@Test
	public void testSimplex() {
		
		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression d = DefaultSymbol.createSymbol("D");
		
		ArrayList<Expression> variables = arrayList(a, b, d);
		
		assertEquals(
				parse("{   if     A then if     B then if D = 6 then 1 else 0 else 0 else 0, "
						+ "if     A then if     B then if D = 7 then 1 else 0 else 0 else 0, "
						+ "if     A then if     B then if D = 8 then 1 else 0 else 0 else 0, "
						+ "if     A then if     B then if D = 9 then 1 else 0 else 0 else 0, "
						+ "if     A then if not B then if D = 6 then 1 else 0 else 0 else 0, "
						+ "if     A then if not B then if D = 7 then 1 else 0 else 0 else 0, "
						+ "if     A then if not B then if D = 8 then 1 else 0 else 0 else 0, "
						+ "if     A then if not B then if D = 9 then 1 else 0 else 0 else 0, "
						+ "if not A then if     B then if D = 6 then 1 else 0 else 0 else 0, "
						+ "if not A then if     B then if D = 7 then 1 else 0 else 0 else 0, "
						+ "if not A then if     B then if D = 8 then 1 else 0 else 0 else 0, "
						+ "if not A then if     B then if D = 9 then 1 else 0 else 0 else 0, "
						+ "if not A then if not B then if D = 6 then 1 else 0 else 0 else 0, "
						+ "if not A then if not B then if D = 7 then 1 else 0 else 0 else 0, "
						+ "if not A then if not B then if D = 8 then 1 else 0 else 0 else 0, "
						+ "if not A then if not B then if D = 9 then 1 else 0 else 0 else 0 }"),

				DefaultExtensionalBound.simplex(variables, model));
		
		variables.add(c);
		
		assertEquals(
				parse("{ ( on A' in Boolean, B' in Boolean, D' in 6..9, C' in 1..4 ) "
						+ "if C = C' then if D = D' then if B = B' then if A = A' then "
						+ "1 else 0 else 0 else 0 else 0 }"),
				DefaultIntensionalBound.simplex(variables, model));
		
		assertEquals(parse("{ ( on A' in Boolean) if A = A' then 1 else 0}"), intensionalSetOfFactors2);
		assertEquals(parse("{(on C' in 1..4, B' in Boolean) if B = B' then if C = C' then 1 else 0 else 0}"), intensionalSetOfFactors3);
		assertEquals(parse("{(on ) 1}"), DefaultIntensionalBound.simplex(arrayList(), model));
	}
	
	// @Test : bug was introduced when indexExpressions started requiring a type.
	public void testNormalize() {
		
		assertEquals(
				"{ if X then 1/7 else if Y then 2/7 else 3/7, "
				+ "if A then if Y then 4/21 else 5/21 else 2/7, "
				+ "if X then 7/31 else if B then 8/31 else 9/31, "
				+ "if B then 10/43 else if A then 11/43 else 12/43, "
				+ "if C < 4 then 10/53 else if C = 4 then 11/53 else 12/53 }",
				
				setOfFactors.normalize(theory, context).toString());

		assertEquals(
				parse("{ ( on A' in Boolean, C' in 1..5 ) "
						+ "if C = C' then "
							+ "if A then "
								+ "if A' then "
									+ "0.2 "
								+ "else "
									+ "0.8 "
							+ "else "
								+ "if not A' then "
									+ "0.2 "
								+ "else "
									+ "0.8"
							+ " else 0 }"),
							
				intensionalSetOfFactors1.normalize(theory, context));
		
		Bound b = Bounds.boundProduct(
				theory, context, false, intensionalSetOfFactors1, intensionalSetOfFactors2).normalize(theory, context);
		
		assertEquals(
				parse("{ ( on A' in Boolean, C' in 1..5, A'' in Boolean ) "
						+ "if C = C' then "
							+ "if A then "
								+ "if A'' then "
									+ "1 "
								+ "else "
									+ "0 "
							+ "else "
								+ "if not A'' then "
									+ "1 "
								+ "else "
									+ "0 "
						+ "else "
							+ "0 }"),
				b);
	}
	
	@Test
	public void testBoundProduct() {
		
		// Set of functions
		Expression phi = parse("if A = true then 1 else 6");
		Bound setOfFactors2 = new DefaultExtensionalBound(phi);
		
		assertEquals(
				 parse( "{ if X then if A then 1 else 6 else if Y then if A then 2 else 12 else if A then 3 else 18, "
				 		+ "if X then if A then 2 else 12 else if Y then if A then 4 else 24 else if A then 6 else 36, "
				 		+ "if A then if Y then 4 else 5 else 36, if A then if Y then 8 else 10 else 72, "
				 		+ "if X then if A then 7 else 42 else if B then if A then 8 else 48 else if A then 9 else 54, "
				 		+ "if X then if A then 14 else 84 else if B then if A then 16 else 96 else if A then 18 else 108, "
				 		+ "if B then if A then 10 else 60 else if A then 11 else 72, if B then if A then 20 else 120 else "
				 		+ "if A then 22 else 144, if C < 4 then if A then 10 else 60 else if C = 4 then if A then 11 else 66 else if A then 12 else 72, "
				 		+ "if C < 4 then if A then 20 else 120 else if C = 4 then if A then 22 else 132 else if A then 24 else 144 }"
						),
				 Bounds.boundProduct(theory, context,true,
						 setOfFactors,
						 setOfFactors2,
						 setOfNumbers
						 ));	 

		assertEquals(
				null,
				Bounds.boundProduct(theory, context, true, new DefaultExtensionalBound(new Expression[1])));
		
		assertEquals(
				parse("{ ( on A' in Boolean, C' in 1..5, C'' in 1..4, B' in Boolean ) if C = C' then if A then if A' then if B then if B' then if C' = C'' then 1 else 0 else 0 else if not B' then if C' = C'' then 1 else 0 else 0 else if B then if B' then if C' = C'' then 4 else 0 else 0 else if not B' then if C' = C'' then 4 else 0 else 0 else if not A' then if B then if B' then if C' = C'' then 1 else 0 else 0 else if not B' then if C' = C'' then 1 else 0 else 0 else if B then if B' then if C' = C'' then 4 else 0 else 0 else if not B' then if C' = C'' then 4 else 0 else 0 else 0 }"),
				Bounds.boundProduct(theory, context, false, intensionalSetOfFactors1, intensionalSetOfFactors3));	
		
		assertEquals(
				parse("{ ( on A' in Boolean ) if A then if A' then 1 else 0 else if not A' then 1 else 0 }"),
				Bounds.boundProduct(theory, context,false,  DefaultIntensionalBound.simplex(arrayList(parse("A")), model)));
	}
	
	@Test
	public void testSummingOverBound() {

		assertEquals(
			"{ if Y then 12 else 16, "
			+ "if Y then 40 else 44, "
			+ "62, "
			+ "86, "
			+ "if C < 4 then 80 else if C = 4 then 88 else 96 }",
			setOfFactors.sumOut(parse("{A,B,X}"), context, theory).toString());

		
		assertEquals(
				"{ if X then 20 else if Y then 40 else 60, "
				+ "if Y then 100 else 110, "
				+ "if X then 140 else 170, "
				+ "215, "
				+ "212 }",
				setOfFactors.sumOut(parse("{A,B,C}"), context, theory).toString());
	}
	
	@Test
	public void testNormalizeOneSingleElement() {
		
		ThreadExplanationLogger.explanationBlockToFile("explanation.txt", code(() -> {
		assertEquals(
				"if A then if B then 67/76 else 7/76 else 1/76",
				Bounds.normalizeSingleExpression(
						parse("if A then if B then 67 else 7 else 1"),
						theory,
						context)
				.toString());
		}));
		
	}
}
