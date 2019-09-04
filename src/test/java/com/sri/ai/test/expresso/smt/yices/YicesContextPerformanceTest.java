package com.sri.ai.test.expresso.smt.yices;

import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.tester.TheoryTestingSupport;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.compound.CompoundTheoryTestingSupport;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.function.BruteForceFunctionTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.theory.tuple.TupleTheory;
import com.sri.ai.util.Timer;
import com.sri.yices.Yices;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.smt.api.SMTBasedContext;
import com.sri.ai.expresso.smt.core.yices.YicesBasedContext;

public class YicesContextPerformanceTest {
	
	static final int startingNumLiterals = 1;
	static final int maxNumLiterals = 15;//50;
	static final double numLiteralsMultiplier = 1.25;
	static final int inflatedMaxNumLiterals = ceilToInt(maxNumLiterals*10);
	static final boolean printLiterals = true;
	static final int numVariables = 5;
	
	static final int maxNumDigitsForConstantsInRandomlyCreatedLiterals = 3;
	static final String integerIntervalUpperBound = String.valueOf((int) Math.pow(10, maxNumDigitsForConstantsInRandomlyCreatedLiterals));
	static final String integerIntervalLowerBound = "-"+integerIntervalUpperBound;
	static final String integerIntervalType = integerIntervalLowerBound + ".." + integerIntervalUpperBound;
	static final String integerType = "Integer";
	static final String booleanType = "Boolean";
	
	//static final String[] types = new String[] {booleanType, integerType, integerIntervalType};
	//static final String[] types = new String[] {booleanType};
	//static final String[] types = new String[] {integerType};
	static final String[] types = new String[] {integerIntervalType};
	
	static final int numLiteralsForBurnIn = Math.min(10, maxNumLiterals);
	static final int numBurnInIterations = 3;

	static final Theory theory = new CommonTheory();
	
	static final ArrayList<Expression> literals = new ArrayList<Expression>(inflatedMaxNumLiterals);
	static final String[] symbolsAndTypes = new String[numVariables*2];
	
	static int numLiterals = -1;
	static boolean burningIn = true;
	static SMTBasedContext[] yicesContexts = new SMTBasedContext[inflatedMaxNumLiterals];

	@Test
	public void conjoinLiteralsUntilUnsatisfiableThenContinuingOnAnEmptyContextWithSMTBasedContextsPreparedAheadOfTime() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		repeatNtimes(()->conjoinLiteralsToTrueContextResettingWhenUnsatisfiable(), numBurnInIterations);
		repeatNtimes(()->conjoinLiteralsToSMTBasedContextUsingPremadeEmptyContextWhenUnsatisfiable(), numBurnInIterations);
		burningIn = false;
		
		long trueContextConjoinTime;
		long smtBasedContextConjoinTime;
		
		for(; numLiterals <= inflatedMaxNumLiterals; numLiterals = ceilToInt(numLiterals*numLiteralsMultiplier) ) {
			trueContextConjoinTime = Timer.time(()->conjoinLiteralsToTrueContextResettingWhenUnsatisfiable());
			smtBasedContextConjoinTime = Timer.time(()->conjoinLiteralsToSMTBasedContextMakingNewContextWhenUnsatisfiable());
			println(smallTab + "Conjoining << " + numLiterals + " >> random literals...");
			println(smallTab + "         TrueContext conjoin time: " + trueContextConjoinTime);
			println(smallTab + "     SMTBasedContext conjoin time: " + smtBasedContextConjoinTime);
			try{
				println(smallTab + "               conjoin time ratio: " + Math.round(trueContextConjoinTime/smtBasedContextConjoinTime) + ":1");
			}
			catch(ArithmeticException e) {
				println(smallTab + "               conjoin time ratio: " + trueContextConjoinTime + ":0");
			}
			println();
		}
		
		
		println();
		println();
		println();
	}

	@Test
	public void conjoinLiteralsUntilUnsatisfiableThenContinuingOnAnEmptyContext() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		repeatNtimes(()->conjoinLiteralsToTrueContextResettingWhenUnsatisfiable(), numBurnInIterations);
		repeatNtimes(()->conjoinLiteralsToSMTBasedContextMakingNewContextWhenUnsatisfiable(), numBurnInIterations);
		burningIn = false;
		
		long trueContextConjoinTime;
		long smtBasedContextConjoinTime;
		
		for(; numLiterals <= inflatedMaxNumLiterals; numLiterals = ceilToInt(numLiterals*numLiteralsMultiplier) ) {
			trueContextConjoinTime = Timer.time(()->conjoinLiteralsToTrueContextResettingWhenUnsatisfiable());
			smtBasedContextConjoinTime = Timer.time(()->conjoinLiteralsToSMTBasedContextMakingNewContextWhenUnsatisfiable());
			println(smallTab + "Conjoining << " + numLiterals + " >> random literals...");
			println(smallTab + "         TrueContext conjoin time: " + trueContextConjoinTime);
			println(smallTab + "     SMTBasedContext conjoin time: " + smtBasedContextConjoinTime);
			try{
				println(smallTab + "               conjoin time ratio: " + Math.round(trueContextConjoinTime/smtBasedContextConjoinTime) + ":1");
			}
			catch(ArithmeticException e) {
				println(smallTab + "               conjoin time ratio: " + trueContextConjoinTime + ":0");
			}
			println();
		}
		
		
		println();
		println();
		println();
	}

	




	@Test
	public void conjoinLiteralsBacktrackingWhenUnsatisfiable() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		repeatNtimes(()->conjoinLiteralsToTrueContextWithBacktracking(), numBurnInIterations);
		repeatNtimes(()->conjoinLiteralsToSMTBasedContextWithBacktracking(), numBurnInIterations);
		burningIn = false;
		
		long trueContextConjoinTime;
		long smtBasedContextConjoinTime;
		
		for(; numLiterals <= maxNumLiterals; numLiterals = ceilToInt(numLiterals*numLiteralsMultiplier) ) {
			trueContextConjoinTime = Timer.time(()->conjoinLiteralsToTrueContextWithBacktracking());
			smtBasedContextConjoinTime = Timer.time(()->conjoinLiteralsToSMTBasedContextWithBacktracking());
			println(smallTab + "Conjoining << " + numLiterals + " >> random literals...");
			println(smallTab + "         TrueContext conjoin time: " + trueContextConjoinTime);
			println(smallTab + "     SMTBasedContext conjoin time: " + smtBasedContextConjoinTime);
			try{
				println(smallTab + "               conjoin time ratio: " + Math.round(trueContextConjoinTime/smtBasedContextConjoinTime) + ":1");
			}
			catch(ArithmeticException e) {
				println(smallTab + "               conjoin time ratio: " + trueContextConjoinTime + ":0");
			}
			println();
		}

		
		println();
		println();
		println();
	}
	
	
	
	
	
	@Test
	public void conjoinLiteralsWithNoBacktrackingForTrueContext() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		repeatNtimes(()->conjoinLiteralsToTrueContextWithoutBacktracking(), numBurnInIterations);
		repeatNtimes(()->conjoinLiteralsToSMTBasedContextWithBacktracking(), numBurnInIterations);
		burningIn = false;
		
		long trueContextConjoinTime;
		long smtBasedContextConjoinTime;
		
		for(; numLiterals <= maxNumLiterals; numLiterals = ceilToInt(numLiterals*numLiteralsMultiplier) ) {
			trueContextConjoinTime = Timer.time(()->conjoinLiteralsToTrueContextWithBacktracking());
			smtBasedContextConjoinTime = Timer.time(()->conjoinLiteralsToSMTBasedContextWithBacktracking());
			println(smallTab + "Conjoining << " + numLiterals + " >> random literals...");
			println(smallTab + "         TrueContext conjoin time: " + trueContextConjoinTime);
			println(smallTab + "     SMTBasedContext conjoin time: " + smtBasedContextConjoinTime);
			try{
				println(smallTab + "               conjoin time ratio: " + Math.round(trueContextConjoinTime/smtBasedContextConjoinTime) + ":1");
			}
			catch(ArithmeticException e) {
				println(smallTab + "               conjoin time ratio: " + trueContextConjoinTime + ":0");
			}
			println();
		}

		
		println();
		println();
		println();
	}

	
	
	
	@Test
	public void assertSatisfiabilityConsensuBetweenTrueAndSMTBasedContexts() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();

		burningIn = false;
		
		boolean thereIsSatisfiabilityConsensusBetweenTrueContextAndSMTBasedContext =
															trueContextAndSMTBasedContextAgree();
		
		myAssert(thereIsSatisfiabilityConsensusBetweenTrueContextAndSMTBasedContext, ()->"TrueContext and SMTBasedContext satisfiability did not match!");
		println("TrueContext and SMTBasedContext had the same satisfiability results when conjoining the same literals =)");
			
		println();
		println();
		println();
	}

	
	private boolean trueContextAndSMTBasedContextAgree() {
		final Boolean UNSAT = false;
		numLiterals = maxNumLiterals;
		
		boolean contextsAgree = true;
		
		Context trueContext = new TrueContext(theory);
		trueContext = trueContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		
		SMTBasedContext smtBasedContext = new YicesBasedContext(theory);
		smtBasedContext = (SMTBasedContext) smtBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Context newlyConjoinedTrueContext;
		for(int i = 0; i < numLiterals; ++i) {
			Expression literalToConjoin = literals.get(i);							println(smallTab + "Asserting:  (" + literalToConjoin + ")");
			newlyConjoinedTrueContext = trueContext.conjoin(literalToConjoin);
			smtBasedContext.conjoin(literalToConjoin);
			
			Boolean contextSatisfiabilityConsensus = determineContextSatisfiabilityConsensus(newlyConjoinedTrueContext, smtBasedContext);
			if(contextSatisfiabilityConsensus == null) {
				contextsAgree = false;
				break;
			}
			else if(contextSatisfiabilityConsensus == UNSAT) {
				smtBasedContext.popStackFrame();									println(smallTab + "LITERAL WAS UNSAT!!!  BACKTRACKING...");
			}
			else {
				trueContext = newlyConjoinedTrueContext;
			}
		}
		return contextsAgree;
	}



	private Boolean determineContextSatisfiabilityConsensus(Context c1, Context c2) {
		Boolean result;
		boolean c1isContradiction = c1.isContradiction();
		boolean c2isContradiction = c2.isContradiction();
		if(c1isContradiction == c2isContradiction) {
			result = !c1isContradiction;
		}
		else {
			result = null;															//println(c1.getClass().getSimpleName() + " is Satisfiable: " + !c1.isContradiction());
																					//println(c2.getClass().getSimpleName() + " is Satisfiable: " + !c2.isContradiction());
		}
		return result;
	}
	
	
	



	private Object conjoinLiteralsToTrueContextResettingWhenUnsatisfiable() {
		Context blankContext = new TrueContext(theory);
		blankContext = blankContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		Context context = blankContext;
		Context newlyConjoinedContext;
		int maxIter;
		if(burningIn) {
			maxIter = numLiteralsForBurnIn;
		}
		else {
			maxIter = numLiterals;
		}
		for(int i = 0; i < maxIter; ++i) {
			Expression literal = literals.get(i);
			newlyConjoinedContext = context.conjoin(literal);
			if(newlyConjoinedContext.isContradiction()) {
				context = blankContext;
			}
			else {
				context = newlyConjoinedContext;
			}
		}
		return context;
	}
	
	private Object conjoinLiteralsToSMTBasedContextMakingNewContextWhenUnsatisfiable() {
		SMTBasedContext context = new YicesBasedContext(theory);
		context = (SMTBasedContext) context.extendWithSymbolsAndTypes(symbolsAndTypes);
		int maxIter;
		if(burningIn) {
			maxIter = numLiteralsForBurnIn;
		}
		else {
			maxIter = numLiterals;
		}
		for(int i = 0; i < maxIter; ++i) {
			Expression literal = literals.get(i);
			context.conjoin(literal);
			if(context.isContradiction()) {
				context = new YicesBasedContext(theory);
				context = (SMTBasedContext) context.extendWithSymbolsAndTypes(symbolsAndTypes);
			}
		}
		Yices.reset();
		return context;
	}
	
	private Object conjoinLiteralsToSMTBasedContextUsingPremadeEmptyContextWhenUnsatisfiable() {
		int contextIdx = 0;
		SMTBasedContext context = yicesContexts[contextIdx++];
		int maxIter;
		if(burningIn) {
			maxIter = numLiteralsForBurnIn;
		}
		else {
			maxIter = numLiterals;
		}
		for(int i = 0; i < maxIter; ++i) {
			Expression literal = literals.get(i);
			context.conjoin(literal);
			if(context.isContradiction()) {
				context = yicesContexts[contextIdx++];
			}
		}
		Yices.reset();
		if(burningIn) {
			reInitializeArrayOfYicesContexts();
		}
		return context;
	}



	
	
	
	
	
	private Context conjoinLiteralsToTrueContextWithBacktracking() {
		Context context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		Context newlyConjoinedContext;
		int maxIter;
		if(burningIn) {
			maxIter = numLiteralsForBurnIn;
		}
		else {
			maxIter = numLiterals;
		}
		for(int i = 0; i < maxIter; ++i) {
			Expression literal = literals.get(i);
			newlyConjoinedContext = context.conjoin(literal);
			if(!newlyConjoinedContext.isContradiction()) {
				context = newlyConjoinedContext;
			}
		}
		return context;
	}
	
	private Context conjoinLiteralsToSMTBasedContextWithBacktracking() {
		SMTBasedContext context = new YicesBasedContext(theory);
		context = (SMTBasedContext) context.extendWithSymbolsAndTypes(symbolsAndTypes);
		int maxIter;
		if(burningIn) {
			maxIter = numLiteralsForBurnIn;
		}
		else {
			maxIter = numLiterals;
		}
		for(int i = 0; i < maxIter; ++i) {
			Expression literal = literals.get(i);
			context.conjoin(literal);
			if(context.isContradiction()) {
				context.popStackFrame();
			}
		}
		Yices.reset();
		return context;
	}

	private Context conjoinLiteralsToTrueContextWithoutBacktracking() {
		Context context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		for(Expression literal : literals) {
			context = context.conjoin(literal);
		}
		return context;
	}




	
	public void blankTest() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		
		println();
		println();
		println();
	}
	
	
	
	



	private static void printTestTitle(String testFunctionName, boolean thickBorders) {
		String testName = makeTestName(testFunctionName);
		String framedTestName = frameTestTitle(testName, thickBorders);
		println(framedTestName);
	}
	
	private static String frameTestTitle(String testName, boolean thickBorders) {
		String framedTestName;
		String borderEdge;
		char b;
		if(thickBorders) {
			b = '|';
			borderEdge = "||||";
		}
		else {
			b = '/';
			borderEdge = "//";
		}
		int borderLength = testName.length() + 2*smallTab.length() + 2*borderEdge.length();
		char[] borderChars = new char[borderLength];
		Arrays.fill(borderChars, b);
		String border = new String(borderChars);
		if(thickBorders) {
			border = border + "\n" + border;
		}
		String titleLine = borderEdge + smallTab + testName + smallTab + borderEdge;
		framedTestName = border + '\n' + titleLine + '\n' + border;
		return framedTestName;
	}
	
	
	
	
	
	
	final static String tab = "\t";
	final static String stab_big = "        ";
	final static String smallTab = "    ";

	public static Random makeRandom() {
		return new Random();
	}
	
	
	
	
	
	
	
	
	@BeforeClass
	public static void prepareSymbolsAndTypesAndLiterals() throws Exception {
		printTestTitle("Yices Context Performance Test", true);
		println();
		printTestParameters();
		prepareSymbolsAndTypes();
		printSymbolsAndTypes(symbolsAndTypes);
		prepareLiterals();
		println();
		println();
		println();
	}
	
	private static void prepareSymbolsAndTypes() throws Exception {
		int numTypes = types.length;
		int numVariablesPerType = numVariables/numTypes;
		int variableNameLength = (int) Math.ceil( Math.pow(numVariables, 1.0/25) );
		
		char[] variableName = new char[variableNameLength];
		Arrays.fill(new char[variableNameLength], '\0');
		variableName[0] = 'A';
		int symbolsAndTypesIdx = 0;
		
		int typeIdx = 0;
		for(; typeIdx < numTypes-1; ++typeIdx) {
			for(int j = 0; j < numVariablesPerType; ++j) {
				symbolsAndTypes[symbolsAndTypesIdx++] = new String(variableName).replace("\0", "");
				updateVariableName(variableName);
				symbolsAndTypes[symbolsAndTypesIdx++] = types[typeIdx];
			}
		}
		for(; symbolsAndTypesIdx < symbolsAndTypes.length;) {
			symbolsAndTypes[symbolsAndTypesIdx++] = new String(variableName).replace("\0", "");
			updateVariableName(variableName);
			symbolsAndTypes[symbolsAndTypesIdx++] = types[typeIdx];
		}

	}

	private static void updateVariableName(char[] variableName) {
		updateVariableName(variableName,0);
	}
	
	private static void updateVariableName(char[] variableName, int i) {
		if(variableName[i] == '\0') {
			variableName[i] = 'A';
		}
		else if(variableName[i] == 90) {
			variableName[i] = 65;
			updateVariableName(variableName, i+1);
		}
		else {
			++variableName[i];
		}
	}
	
	private static void prepareLiterals() throws Exception {
		Random rand = makeRandom();
		Context context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		Map<String, Type> variableNamesAndTypes = map();
		for(int i = 0; i < numVariables; ++i) {
			String symbol = symbolsAndTypes[i*2];
			Expression variable = parse(symbolsAndTypes[i*2]);
			Type type = context.getTypeOfRegisteredSymbol(variable);
			variableNamesAndTypes.put(symbol,  type);
		}
		CompoundTheory cmpdTheory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory(),
				new BruteForceFunctionTheory()
			);
		TheoryTestingSupport theoryTestingSupport = new CompoundTheoryTestingSupport(cmpdTheory, makeRandom());
		theoryTestingSupport.setVariableNamesAndTypesForTesting(variableNamesAndTypes);
		context = theoryTestingSupport.makeContextWithTestingInformation();
		if(printLiterals) {
			println(smallTab + "Literals:");
		}
		for(int j = 0; j < inflatedMaxNumLiterals; ++j) {
			Expression literal = theoryTestingSupport.makeRandomLiteral(context);							//println(tab + "Original: " + literal);
			
			//reducing constants smaller numbers
			literal = boundConstantsInLiteral(literal, context, rand);
			
			literals.add(literal);
			if(printLiterals) {
				println(tab + literal);				
			}
		}
	}

	public static Expression boundConstantsInLiteral(Expression literal, Context context, Random rand) {
		List<Expression> literalArguments = literal.getArguments();
		for(Expression argument : literalArguments) {
			if(context.isUniquelyNamedConstant(argument)) {													//println(argument);
				String argStr = argument.toString();														//println(argStr);
				int numDigits = argStr.length() - (argStr.charAt(0) == '-' ? 1 : 0);						//println(tab + "        : " + numDigits);
				if(numDigits > maxNumDigitsForConstantsInRandomlyCreatedLiterals) {
					int newNumber = rand.nextInt()%((int)Math.pow(10, maxNumDigitsForConstantsInRandomlyCreatedLiterals));															
					Expression newConstant = parse(""+newNumber);
					literal = literal.replaceAllOccurrences(argument, newConstant, context);				//println(tab + "        : " + argument + " --> " + newConstant);
				}
			}
			else if(argument.getFunctor() != null) { //argument has functor
				literal = literal.replaceAllOccurrences(argument, boundConstantsInLiteral(argument, context, rand), context);
			}
		}
		return literal;
	}
	
	
	
	@Before
	public void resetForNextTest() throws Exception {
		resetNumLiterals();
		resetBurnIn();
		resetYices();
		reInitializeArrayOfYicesContexts();
	}
	
	public void resetYices() throws Exception {
		Yices.reset();
	}
	
	public void resetNumLiterals() throws Exception {
		numLiterals = startingNumLiterals;
	}
	
	public void resetBurnIn() throws Exception {
		burningIn = true;
	}
	
	public void reInitializeArrayOfYicesContexts() {
		for(int i = 0;  i < yicesContexts.length; ++i) {
			SMTBasedContext newSMTBasedContext = new YicesBasedContext(theory);
			yicesContexts[i] = (SMTBasedContext) newSMTBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		}
	}
	
	
	
	
	
	
	private static void printTestParameters() {
		println(smallTab + "Test Parameters:");
		println(tab + "numVariables           " + numVariables);
		println(tab + "startingNumLiterals    " + startingNumLiterals);
		println(tab + "maxNumLiterals         " + maxNumLiterals);
		println(tab + "inflatedMaxNumLiterals " + inflatedMaxNumLiterals);
		println(tab + "numLiteralsMultiplier  " + numLiteralsMultiplier);
		hypenSepartorLine();
	}
	
	
	
	
	


	public static int ceilToInt(double d) {
		int result = (int) Math.ceil(d);
		return result;
	}
	
	public static String makeTestName(String testObjectName) {
		String testName = null;
		StringBuilder testNameConstruction = new StringBuilder(testObjectName.length()*2);
		
		//make first letter capital
		char firstChar = testObjectName.charAt(0);
		char upperCaseFirstChar = Character.toUpperCase(firstChar);
		testNameConstruction.append(upperCaseFirstChar);
		
		//iterate through rest of chars in function name
		//for each char that is a capital, insert a space before
		for(int i = 1; i < testObjectName.length(); ++i) {
			char c = testObjectName.charAt(i);
			if(Character.isUpperCase(c)) {
				testNameConstruction.append(' ');
			}
			testNameConstruction.append(c);
		}
		testName = testNameConstruction.toString();
		return testName;
	}
	
	public static void repeatNtimes(Runnable procedure, int n) {
		for (int i = 0; i < n; ++i) {
				procedure.run();
		}
	}
	
	public static void printSymbolsAndTypes(String[] symbolsAndTypes) {
		println(smallTab + "Symbols and Types:");
		for(int i = 0; i < symbolsAndTypes.length/2; ++i) {
			String symbol = symbolsAndTypes[i*2];
			String type = symbolsAndTypes[i*2 + 1];
			println(tab + symbol + ":  " + type);
		}
		hypenSepartorLine();
	}
	
	public static void hypenSepartorLine() {
		println();
		println("-------------------------------------------------------------");
		println();
	}
	
	public void sparseHypenSepartorLine(boolean withLeadingNewline) {
		if(withLeadingNewline) {
			println();
		}
		println("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -");
		println();
	}
}
