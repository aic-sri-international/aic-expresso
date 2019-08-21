package com.sri.ai.test.expresso.smt.yices;

import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.print;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.ArrayList;
import java.util.Random;

import org.junit.Before;
import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.yices.Terms;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.core.yices.YicesConjunction;
import com.sri.ai.expresso.type.Categorical;

public class YicesConjunctionTest {
	
	final String tab = "\t";
	final String stab_big = "        ";
	final String stab = "    ";

	public Random makeRandom() {
		return new Random();
	}
	
	
	
	
	@Before
	public void clearYicesTerms() throws Exception {
        yicesUnregister_ABCDEF_VWXYZ();
	}
	

	@Test
	public void blankTest() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		
		
		println();
		println();
		println();
	}
	
	
	
	
	@Test
	public void testBooleans() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		String[] symbolsAndTypes = new String[] {
				"W", "Boolean",
				"X", "Boolean", 
				"Y", "Boolean", 
				"Z", "Boolean"
		};
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);
		
		YicesConjunction conjunction = new YicesConjunction();
		ArrayList<String> conjoinedLiterals = new ArrayList<String>(10);
		
		String literalString;
		
		String[] satisfiableLiteralStrings = new String[] {
				"X",
				"Y = X",
				"not Z",
				"X != W",
		};
		conjoinSatisfiableLiteralsAndPrintInfo(satisfiableLiteralStrings, conjunction, context, conjoinedLiterals);
		
		literalString = "Y = W";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y != Z";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		
		println();
		println();
		println();
	}
	
	
	@Test
	public void testCategoricals() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		context = context.makeNewContextWithAddedType(new Categorical("Staff",  2, makeSymbol("Karen"), makeSymbol("Rodrigo")));
		context = context.makeNewContextWithAddedType(new Categorical("Intern",  10, makeSymbol("Sarah"), makeSymbol("Redouane"), makeSymbol("Roger"), makeSymbol("Bobak")));
		String[] symbolsAndTypes = new String[] {
				"S1", "Staff",
				"S2", "Staff",
				"S3", "Staff",
				"I1", "Intern", 
				"I2", "Intern",
				"I3", "Intern"
		};
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);
		
		YicesConjunction conjunction = new YicesConjunction();
		ArrayList<String> conjoinedLiterals = new ArrayList<String>(10);
		
		String literalString;
		
		String[] satisfiableLiteralStrings = new String[] {
				"S1 = Rodrigo",
				"S2 != S1",
				"S3 != S1",
				"I1 = Roger",
				"I2 != I1",
				"I3 != I1",
				"I2 != I3",
		};
		conjoinSatisfiableLiteralsAndPrintInfo(satisfiableLiteralStrings, conjunction, context, conjoinedLiterals);
		
		literalString = "S3 != S2";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "I2 = Roger";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "I2 = Sarah";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		
		println();
		println();
		println();
	}
	
	
	@Test
	public void testIntegers() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		String[] symbolsAndTypes = new String[] {
				"X", "Integer", 
				"Y", "Integer", 
				"Z", "Integer"
		};
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);

		YicesConjunction conjunction = new YicesConjunction();
		ArrayList<String> conjoinedLiterals = new ArrayList<String>(10);
		
		String literalString;
		
		literalString = "X = 1/3";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		String[] satisfiableLiteralStrings = new String[] {
				"X < 10",
				"Y < X",
				"Z > -10.33",
				"Y > Z"
		};
		conjoinSatisfiableLiteralsAndPrintInfo(satisfiableLiteralStrings, conjunction, context, conjoinedLiterals);
		
		literalString = "Y = -100";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = -1";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		
		println();
		println();
		println();
	}
	
	
	
	@Test
	public void testReals() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		String[] symbolsAndTypes = new String[] {
				"X", "Real", 
				"Y", "Real", 
				"Z", "Real"
		};
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);

		YicesConjunction conjunction = new YicesConjunction();
		ArrayList<String> conjoinedLiterals = new ArrayList<String>(10);
		
		String literalString;
		
		String[] satisfiableLiteralStrings = new String[] {
				"X < 10",
				"Y < X",
				"Z > -10.33",
				"Y > Z",
		};
		conjoinSatisfiableLiteralsAndPrintInfo(satisfiableLiteralStrings, conjunction, context, conjoinedLiterals);
		
		literalString = "Y = -100/3";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = -0.5";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		
		println();
		println();
		println();
	}


	@Test
	public void testClosedIntegerIntervals() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		String[] symbolsAndTypes = new String[] {
				"X", "1..(10+2)", 
				"Y", "-10..10", 
				"Z", "-10..0"
		};
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);

		YicesConjunction conjunction = new YicesConjunction();
		ArrayList<String> conjoinedLiterals = new ArrayList<String>(10);
		
		String literalString;
		
		literalString = "X = 0";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "X = 13";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "X = 12";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = -11";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = -10";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		String[] satisfiableLiteralStrings = new String[] {
				"X < 10",
				"Y < X",
				"Z > -10.33",
				"Y > Z",
		};
		conjoinSatisfiableLiteralsAndPrintInfo(satisfiableLiteralStrings, conjunction, context, conjoinedLiterals);
		
		literalString = "Y = -100";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = -1";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		
		println();
		println();
		println();
	}
	
	
	@Test
	public void testOpenIntegerIntervals() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		String[] symbolsAndTypes = new String[] {
				"X", "1..infinity", 
				"Y", "-infinity..-1", 
				"Z", "-10..10"
		};
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);

		YicesConjunction conjunction = new YicesConjunction();
		ArrayList<String> conjoinedLiterals = new ArrayList<String>(10);
		
		String literalString;
		
		literalString = "X = 0";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "X = 1";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "X = 100000000000000000000000";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = 0";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = -100000000000000000000000";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);

		String[] satisfiableLiteralStrings = new String[] {
				"X < 10",
				"Y < X",
				"X = 2*2",
				"Z = 7",
		};
		conjoinSatisfiableLiteralsAndPrintInfo(satisfiableLiteralStrings, conjunction, context, conjoinedLiterals);
		
		literalString = "Y = 6";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = -8";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		
		println();
		println();
		println();
	}
	
	
	
	@Test
	public void testRealIntervals() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		String[] symbolsAndTypes = new String[] {
				"X", "[   1/2  ;  (10+0.25) ]", 
				"Y", "[ -10.5  ;  10+1/3    ]", 
				"Z", "[ -10/3  ;  0         ]"
		};
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);

		YicesConjunction conjunction = new YicesConjunction();
		ArrayList<String> conjoinedLiterals = new ArrayList<String>(10);
		
		String literalString;
		
		literalString = "X = 0.25";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "X = 10 + 1/2";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "X = 10+1/4";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = -11";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = -10";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		String[] satisfiableLiteralStrings = new String[] {
				"X < 10",
				"Y < X",
				"Z > -10.33",
				"Y > Z",
		};
		conjoinSatisfiableLiteralsAndPrintInfo(satisfiableLiteralStrings, conjunction, context, conjoinedLiterals);
		
		literalString = "Y = -100/3";
		conjoinUnsatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		backtrackAndPrintInfo(conjunction, conjoinedLiterals);
		
		literalString = "Y = -1/3";
		conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		
		println();
		println();
		println();
	}
	
	
	
	@Test
	public void simpleGeneralTest() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		Theory theory = new CommonTheory();
		
		Context context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes("X", "Integer", "Y", "Real");
		
		String[] literalStrings = new String[] {
				"X < 1",
				"Y > X",
				"Y < 0.5",
				"X > -1",
				"Y = 0"
		};
		
		YicesConjunction conjunction = new YicesConjunction();
		
		for (String literalString : literalStrings) {
			println("and(" + literalString + "):");
			Expression literal = parse(literalString);
			conjunction.and(literal, context);
			println(stab + "isSatisfiable: " + conjunction.isSatisfiable());
			println(stab + "getModel: " + conjunction.getModel());
			println();
		}
		
		println("backtracking to undo (Y = 0)...");
		conjunction.backtrack();
		println(stab + "isSatisfiable: " + conjunction.isSatisfiable());
		println(stab + "getModel: " + conjunction.getModel());
		println();
		
		println("and(Y = 0.3):");
		Expression literal = parse("Y = 0.3");
		conjunction.and(literal, context);
		println(stab + "isSatisfiable: " + conjunction.isSatisfiable());
		println(stab + "getModel: " + conjunction.getModel());
		println();
		
        Terms.removeName("X");
        Terms.removeName("Y");
        Terms.removeName("Z");
        
		
		println();
		println();
		println();
	}
	
	
	
	
	
	
	private void hypenSepartorLine() {
		println();
		println("-------------------------------------------------------------");
		println();
	}
	
	private void sparseHypenSepartorLine(boolean withLeadingNewline) {
		if(withLeadingNewline) {
			println();
		}
		println("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -");
		println();
	}
	
	private void printSymbolsAndTypes(String[] symbolsAndTypes) {
		println("Symbols and Types:");
		for(int i = 0; i < symbolsAndTypes.length/2; ++i) {
			String symbol = symbolsAndTypes[i*2];
			String type = symbolsAndTypes[i*2 + 1];
			println(stab + symbol + ":  " + type);
		}
		hypenSepartorLine();
	}

	private void backtrackAndPrintInfo(YicesConjunction conjunction, ArrayList<String> conjoinedLiterals) {
		sparseHypenSepartorLine(false);
		int lastElement = conjoinedLiterals.size()-1;
		println("BACKTRACKING to undo (" + conjoinedLiterals.get(lastElement) + ")...");
		conjoinedLiterals.remove(lastElement);
		printlnArrayListElements(conjoinedLiterals);
		conjunction.backtrack();
		println(stab + "isSatisfiable: " + conjunction.isSatisfiable());
		myAssert(conjunction.isSatisfiable(), ()->"ERROR: Conjunction should have been satisfiable!!!");
		println(stab + "getModel: " + conjunction.getModel());
		sparseHypenSepartorLine(true);
	}

	private void conjoinUnsatisfiableLiteralAndPrintInfo(String literalString, YicesConjunction conjunction,
			Context context, ArrayList<String> conjoinedLiterals) {
		conjoinedLiterals.add(literalString);
		printlnArrayListElements(conjoinedLiterals);
		Expression literal = parse(literalString);
		conjunction.and(literal, context);
		println(stab + "isSatisfiable: " + conjunction.isSatisfiable());
		myAssert(!conjunction.isSatisfiable(), ()->"ERROR: Conjunction should NOT have been satisfiable!!!");
		println(stab + "getModel: " + conjunction.getModel());
		println();
	}

	private void conjoinSatisfiableLiteralsAndPrintInfo(String[] satisfiableLiteralStrings, YicesConjunction conjunction,
			Context context, ArrayList<String> conjoinedLiterals) {
		for (String literalString : satisfiableLiteralStrings) {
			conjoinSatisfiableLiteralAndPrintInfo(literalString, conjunction, context, conjoinedLiterals);
		}
	}
	
	private void conjoinSatisfiableLiteralAndPrintInfo(String literalString, YicesConjunction conjunction,
			Context context, ArrayList<String> conjoinedLiterals) {
		conjoinedLiterals.add(literalString);
		printlnArrayListElements(conjoinedLiterals);
		Expression literal = parse(literalString);
		conjunction.and(literal, context);
		println(stab + "isSatisfiable: " + conjunction.isSatisfiable());
		myAssert(conjunction.isSatisfiable(), ()->"ERROR: Conjunction should have been satisfiable!!!");
		println(stab + "getModel: " + conjunction.getModel());
		println();
	}
	
	private void printlnArrayListElements(ArrayList<?> list) {
		for(int i = 0; i < list.size()-1; ++i) {
			print("("+ list.get(i) + ") ");
		}
		if(list.size()-1 >= 0) {
			print("("+ list.get(list.size()-1) + ")");
		}
		println();
	}
	
	private void yicesUnregister_ABCDEF_VWXYZ() {
		Terms.removeName("A");
        Terms.removeName("B");
        Terms.removeName("C");
        Terms.removeName("D");
        Terms.removeName("E");
        Terms.removeName("F");
        Terms.removeName("V");
        Terms.removeName("W");
        Terms.removeName("X");
        Terms.removeName("Y");
        Terms.removeName("Z");
	}
	
	
}




		

