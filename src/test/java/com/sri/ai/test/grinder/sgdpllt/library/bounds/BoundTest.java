package com.sri.ai.test.grinder.sgdpllt.library.bounds;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bounds;
import com.sri.ai.grinder.sgdpllt.library.bounds.DefaultExtensionalBound;
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
	Bound extensionalBound;
	
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

		extensionalBound = new DefaultExtensionalBound(); 
	}
	
	@Test
	public void testSimplex(){

		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression d = DefaultSymbol.createSymbol("D");
		//Expression q = DefaultSymbol.createSymbol("Q");

		Set<Expression> Factor = new HashSet<Expression>();
		
		Model m = new Model(Factor);
		
		m.context =	m.context.extendWithSymbolsAndTypes("A", "Boolean");
		m.context =	m.context.extendWithSymbolsAndTypes("B", "Boolean");
		m.context =	m.context.extendWithSymbolsAndTypes("Q", "Boolean");
		m.context =	m.context.extendWithSymbolsAndTypes("C", "1..4");
		m.context =	m.context.extendWithSymbolsAndTypes("D", "6..9");
		
		//m.setType(a,  "Boolean");
		//m.setType(b,  "Boolean");
		//m.setType(q,  "Boolean");
		
		//Expression booleanExpression = parse("Boolean");
	
		//m.setValues(a,  booleanExpression);
		//m.setValues(b,  booleanExpression);
		//m.setValues(q,  booleanExpression);
		
		//VariableComponent ComponentResultat = new VariableComponent(q, null, m, new HashSet<Expression>());
		
		ArrayList<Expression> Variables = new ArrayList<>();
		Variables.add(a);
		Variables.add(b);
		Variables.add(c);
		Variables.add(d);

		extensionalBound = new DefaultExtensionalBound(); 

		
		assertEquals(
				parse("{ if A = true then 1 else 0, "
					+ "if A = false then 1 else 0, "
					+ "if B = true then 1 else 0, "
					+ "if B = false then 1 else 0, "
					+ "if C = 1 then 1 else 0, "
					+ "if C = 2 then 1 else 0, "
					+ "if C = 3 then 1 else 0, "
					+ "if C = 4 then 1 else 0, "
					+ "if D = 6 then 1 else 0, "
					+ "if D = 7 then 1 else 0, "
					+ "if D = 8 then 1 else 0, "
					+ "if D = 9 then 1 else 0 }"),
				extensionalBound.simplex(Variables, m));
		
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
				extensionalBound.normalize(setOfFactors, theory, context).toString());
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
				 extensionalBound.boundProduct(theory, context, setOfFactors,setOfFactors,setOFNumbers));	 

		assertEquals(parse("{1}"),Bounds.boundProduct(theory, context, new Expression[0]));
		//Util.println(Bounds.boundProduct(theory, context, parse("{}"), parse("{1,2}")));
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
				extensionalBound.applyFunctionToBound(f, phi, setOfFactors, theory, context)
				); 
	}
	
	@Test
	public void testSummingOverBound(){
		declaringTheoryContextAndSetOfFactors();
		
		assertEquals(
			  "{ if Y then 3/7 else 4/7, "
			+ "if Y then 10/21 else 11/21, "
			+ "1, "
			+ "1, "
			+ "if C < 4 then 10/53 else if C = 4 then 11/53 else 12/53 }",
			extensionalBound.summingBound(parse("{A,B,X}"), setOfFactors, context, theory).toString());

		assertEquals(
				"{ if X then 1/7 else if Y then 2/7 else 3/7, "
				+ "if Y then 10/21 else 11/21, "
				+ "if X then 14/31 else 17/31, "
				+ "1, "
				+ "1 }",
				extensionalBound.summingBound(parse("{A,B,C}"), setOfFactors, context, theory).toString());
	}
}
