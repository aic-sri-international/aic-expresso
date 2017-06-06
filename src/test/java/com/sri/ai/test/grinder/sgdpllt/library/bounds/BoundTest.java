package com.sri.ai.test.grinder.sgdpllt.library.bounds;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bounds;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;


public class BoundTest {
	
	Theory theory ;
	Context context;
	Expression setOFNumbers;
	Expression setOfFactors;
	
	private void declaringTheoryContextAndSetOfFactors() {

		theory = new CompoundTheory(
								new EqualityTheory(false, true),
								new DifferenceArithmeticTheory(false, false),
								new LinearRealArithmeticTheory(false, false),
								new TupleTheory(),
								new PropositionalTheory());	
		
		context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes("X","Boolean");
		context = context.extendWithSymbolsAndTypes("Y","Boolean");
		context = context.extendWithSymbolsAndTypes("A","Boolean");
		context = context.extendWithSymbolsAndTypes("B","Boolean");
		context = context.extendWithSymbolsAndTypes("C","1..5");
		//context = context.extendWithSymbolsAndTypes("D","{1,3,4,8}");

		//Set of functions
		Expression phi1 = parse("if X = true then 1 else if Y = true then 2 else 3");
		Expression phi2 = parse("if A = true then if Y = true then 4 else 5 else 6");
		Expression phi3 = parse("if X = true then 7 else if B = true then 8 else 9");
		Expression phi4 = parse("if B = true then 10 else if A = true then 11 else 12");
		Expression phi5 = parse("if C < 4 then 10 else if C = 4 then 11 else 12");
		setOfFactors = ExtensionalSets.makeUniSet(phi1, phi2, phi3, phi4, phi5);
		
		//Set of numbers
		Expression one   = DefaultSymbol.createSymbol(1);
		Expression two   = DefaultSymbol.createSymbol(2);
		setOFNumbers = ExtensionalSets.makeUniSet(one, two);

	}
	
	@Test
	public void testSimplex(){

		
	}
	
	@Test
	public void testNormalize(){
		declaringTheoryContextAndSetOfFactors();
		
		assertEquals(
				"{ if X then 1/7 else if Y then 2/7 else 3/7, "
				+ "if A then if Y then 4/21 else 5/21 else 2/7, "
				+ "if X then 7/31 else if B then 8/31 else 9/31, "
				+ "if B then 10/43 else if A then 11/43 else 12/43, "
				+ "if C < 4 then 10/53 else if C = 4 then 11/53 else 12/53 }",
				Bounds.normalize(setOfFactors, theory, context).toString());
	}
	
	@Test
	public void testBoundProduct(){
		declaringTheoryContextAndSetOfFactors();
		
		//Set of numbers
		Expression one   = DefaultSymbol.createSymbol(1);
		Expression two   = DefaultSymbol.createSymbol(2);
		Expression setOFNumbers = ExtensionalSets.makeUniSet(one, two);

		//Set of functions
		Expression phi1 = parse("if C = 2 then 1 else 3");
		Expression phi2 = parse("if A = true then  1 else 6");
		Expression phi3 = parse("if X = true then 7 else if Y = true then 8 else 9");
		Expression setOfFactors = ExtensionalSets.makeUniSet(phi1, phi2, phi3);
		
		assertEquals(
				 parse("{ if C = 2 then 1 else 9, "
				 		+ "if C = 2 then 2 else 18, "
				 		+ "if C = 2 then if A then 1 else 6 else if A then 3 else 18, "
				 		+ "if C = 2 then if A then 2 else 12 else if A then 6 else 36, "
				 		+ "if C = 2 then if X then 7 else if Y then 8 else 9 else if X then 21 else if Y then 24 else 27, "
				 		+ "if C = 2 then if X then 14 else if Y then 16 else 18 else if X then 42 else if Y then 48 else 54, "
				 		+ "if A then if C = 2 then 1 else 3 else if C = 2 then 6 else 18, "
				 		+ "if A then if C = 2 then 2 else 6 else if C = 2 then 12 else 36, "
				 		+ "if A then 1 else 36, if A then 2 else 72, "
				 		+ "if A then if X then 7 else if Y then 8 else 9 else if X then 42 else if Y then 48 else 54, "
				 		+ "if A then if X then 14 else if Y then 16 else 18 else if X then 84 else if Y then 96 else 108, "
				 		+ "if X then if C = 2 then 7 else 21 else if Y then if C = 2 then 8 else 24 else if C = 2 then 9 else 27, "
				 		+ "if X then if C = 2 then 14 else 42 else if Y then if C = 2 then 16 else 48 else if C = 2 then 18 else 54, "
				 		+ "if X then if A then 7 else 42 else if Y then if A then 8 else 48 else if A then 9 else 54, "
				 		+ "if X then if A then 14 else 84 else if Y then if A then 16 else 96 else if A then 18 else 108, "
				 		+ "if X then 49 else if Y then 64 else 81, "
				 		+ "if X then 98 else if Y then 128 else 162 }"
						),
				 Bounds.boundProduct(theory, context, setOfFactors,setOfFactors,setOFNumbers));	 
	}
	
	@Test
	public void testApplyFunctionToBound(){
		
		declaringTheoryContextAndSetOfFactors();

		Expression phi = DefaultSymbol.createSymbol("phi");
		Expression f = parse("13*phi");
		
		assertEquals(
				parse("{ 13, 26 }"),
				Bounds.applyFunctionToBound(f, phi, setOFNumbers, theory, context)
				);
		
		assertEquals(
				parse("{ if X then 13 else if Y then 26 else 39, "
						+ "if A then if Y then 52 else 65 else 78, "
						+ "if X then 91 else if B then 104 else 117, "
						+ "if B then 130 else if A then 143 else 156, "
						+ "if C < 4 then 130 else if C = 4 then 143 else 156 }"),
				Bounds.applyFunctionToBound(f, phi, setOfFactors, theory, context)
				); 
	}
	
	@Test
	public void testSummingOverBound(){
		declaringTheoryContextAndSetOfFactors();
		
		assertEquals(
				parse("{ if Y then 12 else 16, "
						+ "if Y then 40 else 44, "
						+ "62, "
						+ "86, "
						+ "if C < 4 then 80 else if C = 4 then 88 else 96 }"),
				Bounds.summingBound(parse("{A,B,X}"), setOfFactors, context, theory));

		assertEquals(
				parse("{if X then 20 else if Y then 40 else 60, "
						+ "if Y then 100 else 110, "
						+ "if X then 140 else 170, "
						+ "215, "
						+ "212 }"),
				Bounds.summingBound(parse("{A,B,C}"), setOfFactors, context, theory));
	}
}
