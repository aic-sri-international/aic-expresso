package com.sri.ai.test.grinder.sgdpllt.library.bounds;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;
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
	

	@Test
	public void testSimplex(){

		
	}
	
	@Test
	public void testNormalize(){
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		
		Context context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes("X","Boolean");
		context = context.extendWithSymbolsAndTypes("Y","Boolean");
		
		//Set of numbers
		Expression one   = DefaultSymbol.createSymbol(1);
		Expression two   = DefaultSymbol.createSymbol(2);
		Expression three = DefaultSymbol.createSymbol(3);
		Expression setOFNumbers = ExtensionalSets.makeUniSet(one, two, three);

		//Set of functions
		Expression phi1 = parse("if X = true then 1 else if Y = true then 2 else 3");
		Expression phi2 = parse("if X = true then if Y = true then 4 else 5 else 6");
		Expression phi3 = parse("if X = true then 7 else if Y = true then 8 else 9");
		Expression phi4 = parse("if X = true then 10 else if Y = true then 11 else 12");
		Expression setOfFactors = ExtensionalSets.makeUniSet(phi1, phi2, phi3, phi4);
		
		assertEquals(
				parse("{if X then 1/7 else if Y then 2/7 else 3/7," +
						" if X then if Y then 4/21 else 5/21 else 2/7, " +
						"if X then 7/31 else if Y then 8/31 else 9/31, " +
						"if X then 10/43 else if Y then 11/43 else 12/43 }"),
				Bounds.normalize(setOfFactors, theory, context));
		assertEquals(parse("{ 1, 1, 1 }"), Bounds.normalize(setOFNumbers, theory, context));
	}
	
	@Test
	public void testBoundProduct(){
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		
		Context context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes("X","Boolean");
		context = context.extendWithSymbolsAndTypes("Y","Boolean");
		
		//Set of numbers
		Expression one   = DefaultSymbol.createSymbol(1);
		Expression two   = DefaultSymbol.createSymbol(2);
		Expression three = DefaultSymbol.createSymbol(3);
		Expression setOFNumbers = ExtensionalSets.makeUniSet(one, two, three);

		//Set of functions
		Expression phi1 = parse("if X = true then 1 else if Y = true then 2 else 3");
		Expression phi2 = parse("if X = true then if Y = true then 4 else 5 else 6");
		Expression phi3 = parse("if X = true then 7 else if Y = true then 8 else 9");
		Expression phi4 = parse("if X = true then 10 else if Y = true then 11 else 12");
		Expression setOfFactors = ExtensionalSets.makeUniSet(phi1, phi2, phi3, phi4);
		
		assertEquals(
				 parse("{ if X then 1 else if Y then 4 else 9," +
						" if X then 2 else if Y then 8 else 18," +
						" if X then 3 else if Y then 12 else 27," +
						" if X then if Y then 4 else 5 else if Y then 12 else 18, " +
						"if X then if Y then 8 else 10 else if Y then 24 else 36," +
						" if X then if Y then 12 else 15 else if Y then 36 else 54," +
						" if X then 7 else if Y then 16 else 27," +
						" if X then 14 else if Y then 32 else 54, " +
						"if X then 21 else if Y then 48 else 81, " +
						"if X then 10 else if Y then 22 else 36, " +
						"if X then 20 else if Y then 44 else 72, " +
						"if X then 30 else if Y then 66 else 108," +
						" if X then if Y then 4 else 5 else if Y then 12 else 18, " +
						"if X then if Y then 8 else 10 else if Y then 24 else 36, " +
						"if X then if Y then 12 else 15 else if Y then 36 else 54, " +
						"if X then if Y then 16 else 25 else 36, " +
						"if X then if Y then 32 else 50 else 72, " +
						"if X then if Y then 48 else 75 else 108, " +
						"if X then if Y then 28 else 35 else if Y then 48 else 54, " +
						"if X then if Y then 56 else 70 else if Y then 96 else 108, " +
						"if X then if Y then 84 else 105 else if Y then 144 else 162, " +
						"if X then if Y then 40 else 50 else if Y then 66 else 72, " +
						"if X then if Y then 80 else 100 else if Y then 132 else 144, " +
						"if X then if Y then 120 else 150 else if Y then 198 else 216, " +
						"if X then 7 else if Y then 16 else 27, " +
						"if X then 14 else if Y then 32 else 54, " +
						"if X then 21 else if Y then 48 else 81, " +
						"if X then if Y then 28 else 35 else if Y then 48 else 54, " +
						"if X then if Y then 56 else 70 else if Y then 96 else 108, " +
						"if X then if Y then 84 else 105 else if Y then 144 else 162, " +
						"if X then 49 else if Y then 64 else 81, " +
						"if X then 98 else if Y then 128 else 162, " +
						"if X then 147 else if Y then 192 else 243, " +
						"if X then 70 else if Y then 88 else 108, " +
						"if X then 140 else if Y then 176 else 216, " +
						"if X then 210 else if Y then 264 else 324, " +
						"if X then 10 else if Y then 22 else 36, " +
						"if X then 20 else if Y then 44 else 72, " +
						"if X then 30 else if Y then 66 else 108, " +
						"if X then if Y then 40 else 50 else if Y then 66 else 72, " +
						"if X then if Y then 80 else 100 else if Y then 132 else 144, " +
						"if X then if Y then 120 else 150 else if Y then 198 else 216, " +
						"if X then 70 else if Y then 88 else 108, " +
						"if X then 140 else if Y then 176 else 216," +
						" if X then 210 else if Y then 264 else 324, " +
						"if X then 100 else if Y then 121 else 144, " +
						"if X then 200 else if Y then 242 else 288, " +
						"if X then 300 else if Y then 363 else 432 }"
						),
				 Bounds.boundProduct(theory, context, setOfFactors,setOfFactors,setOFNumbers));	 
	}
	
	@Test
	public void testApplyFunctionToBound(){
		
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		
		Context context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes("X","Boolean");
		context = context.extendWithSymbolsAndTypes("Y","Boolean");
		
		//Set of numbers
		Expression one   = DefaultSymbol.createSymbol(1);
		Expression two   = DefaultSymbol.createSymbol(2);
		Expression three = DefaultSymbol.createSymbol(3);
		Expression setOFNumbers = ExtensionalSets.makeUniSet(one, two, three);

		//Set of functions
		Expression phi1 = parse("if X = true then 1 else if Y = true then 2 else 3");
		Expression phi2 = parse("if X = true then if Y = true then 4 else 5 else 6");
		Expression phi3 = parse("if X = true then 7 else if Y = true then 8 else 9");
		Expression phi4 = parse("if X = true then 10 else if Y = true then 11 else 12");
		Expression setOfFactors = ExtensionalSets.makeUniSet(phi1, phi2, phi3, phi4);
		
		Expression phi = DefaultSymbol.createSymbol("phi");
		Expression f = parse("13*phi");
		println("f: " + f);
		
		assertEquals(
				parse("{ 13, 26, 39 }"),
				Bounds.applyFunctionToBound(f, phi, setOFNumbers, theory, context)
				);
		
		assertEquals(
				parse("{ if X then 13 else if Y then 26 else 39, " +
						"if X then if Y then 52 else 65 else 78, " + 
						"if X then 91 else if Y then 104 else 117, " + 
						"if X then 130 else if Y then 143 else 156 }"),
				Bounds.applyFunctionToBound(f, phi, setOfFactors, theory, context)
				);
		println("f(setfac): " + Bounds.applyFunctionToBound(f, phi, setOfFactors, theory, context)); 
	}
}
