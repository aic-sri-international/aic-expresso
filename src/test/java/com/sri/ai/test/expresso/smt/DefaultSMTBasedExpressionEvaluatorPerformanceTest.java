package com.sri.ai.test.expresso.smt;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;

import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.function.Function;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.core.AbstractSMTBasedContext;
import com.sri.ai.expresso.smt.core.yices.YicesBasedContext;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.core.TrueContextUsingExpressionEvaluator;
import com.sri.ai.grinder.tester.RandomCondtionalArithmeticExpressionGenerator;
import com.sri.ai.util.Timer;
import com.sri.ai.util.math.BigIntegerNumber;
import com.sri.ai.util.math.BigIntegerNumberExact;
import com.sri.ai.util.math.Rational;
import com.sri.yices.Yices;

public class DefaultSMTBasedExpressionEvaluatorPerformanceTest {
	
	private static final int maxNumberOfDigitsInNumeratorOrDenominatorInRandomlyCreatedConstants = 3;
	private static final Random rand = makeRandom();
	
	@Test
	public void blankTest() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		
		println();
		println();
		println();
	}
	
	
	@Test
	public void determineTimeBreakdown() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		final int maxNumberOfLiterals = 4;
		final int maxNumInternalNodes = 8;
		final int numberOfExpressionsToTestOn = 20;
		final String[] types = new String[] {"Integer"};
		final int numVariables = 4;
		final Theory theory = new CommonTheory();
		
		String[] symbolsAndTypes = prepareSymbolsAndTypes(numVariables, types);
		printTestSpecs(numberOfExpressionsToTestOn, maxNumInternalNodes, maxNumberOfLiterals, types, numVariables);
		
		AbstractSMTBasedContext smtBasedContext = new YicesBasedContext(theory);
		smtBasedContext = (AbstractSMTBasedContext) smtBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		
		RandomCondtionalArithmeticExpressionGenerator expressionGenerator =
				new RandomCondtionalArithmeticExpressionGenerator(smtBasedContext, "BalancedTree", maxNumInternalNodes, maxNumberOfLiterals, false, rand);

		ArrayList<Expression> expressions;
		
		expressions = generateRandomExpressionList(expressionGenerator,  smtBasedContext, numberOfExpressionsToTestOn);
		println(Arrays.toString(expressions.toArray()));
		time(expressions, smtBasedContext);

		smtBasedContext.printEvaluateTimeBreakdown();
		
		println();
		println();
		println();
	}


	public long time(ArrayList<Expression> expressions, Context smtBasedContext) {
		return Timer.time(() -> evaluateExpressions(expressions, smtBasedContext));
	}
	
	
	
	
	@Test
	public void comprehensiveEvaluationTest() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		final boolean printToFile = false;
		final String fileNameToPrintTo = "C:\\Users\\Bobak\\Documents\\SRI\\AIC\\2019_Summer_Internship\\comprehensiveEvaluationTest.txt";
		
		PrintStream stdout = System.out;
		if (printToFile) {
			PrintStream fout = new PrintStream(new FileOutputStream(fileNameToPrintTo));
			System.setOut(fout);
		}
		
		final int maxNumberOfVariables = 3;
		final int numberOfVariablesIncrement = 1;
		final int maxNumberOfInternalNodes = 6;
		final int numberOfInternalNodesIncrement = 1;
		final int numberOfLiteralsIncrement = 1;
		final int numberOfExpressionsToTestOn = 20;
		final int burnInIterations = 5;
		
		final Theory theory = new CommonTheory();
		final String[] types = new String[] 
//				{"Boolean", "Integer", "Real"};
//				{"Boolean", "Integer"};
//				{"Real"};
				{"Integer"};
		final String[] evaluationAndContextTypes = new String[]
				{"Expresso Context [Step Solver]", "Expresso Context [Literal Extraction]", "Yices Context [Literal Extraction]"};
		final ArrayList<Function<Theory,Context>> contextMakers = new ArrayList<Function<Theory,Context>>() { 
			private static final long serialVersionUID = 1L;
			{
				add( (theory)->new TrueContext(theory) );
				add( (theory)->new TrueContextUsingExpressionEvaluator(theory) );
				add( (theory)->new YicesBasedContext(theory) );
			}
		};
		
		println("Variable Type, Context Type [Evaluation Algorithm], Number Of Variables, Number Of Internal Nodes, Number of Literals, Time (ms)");
		for(int numberOfInternalNodes = 1; 
				numberOfInternalNodes <= maxNumberOfInternalNodes;
				numberOfInternalNodes = numberOfInternalNodes + numberOfInternalNodesIncrement)
		{
			for(int numberOfLiterals = 0; numberOfLiterals <= numberOfInternalNodes; numberOfLiterals = numberOfLiterals + numberOfLiteralsIncrement)
			{
				for(int numberOfVariables = 1; 
						numberOfVariables <= maxNumberOfVariables; 
						numberOfVariables = numberOfVariables + numberOfVariablesIncrement) 
				{
					for(String type : types) {
						String[] symbolsAndTypes = prepareSymbolsAndTypes(numberOfVariables, type);
						int contextTypeIdx = -1;
						for(Function<Theory,Context> contextMaker : contextMakers) {
							++contextTypeIdx;
							Context context = contextMaker.apply(theory);
							context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
							RandomCondtionalArithmeticExpressionGenerator expressionGenerator =
									new RandomCondtionalArithmeticExpressionGenerator(
											context, "BalancedTree", numberOfInternalNodes, numberOfLiterals, false, rand);
							//expressionGenerator.setChanceOfGeneratingALiteral(0.5);
							performEvaluationBurnIn(context, expressionGenerator, burnInIterations);
							ArrayList<Expression> expressions = generateRandomExpressionList(expressionGenerator, context, numberOfExpressionsToTestOn);
							long evaluationTime = time(expressions, context);
							println(type + "," + 
									evaluationAndContextTypes[contextTypeIdx] + "," + 
									numberOfVariables + "," + 
									numberOfInternalNodes + "," + 
									numberOfLiterals + "," + 
									evaluationTime);
						}
						resetYices();
					}
				}
			}
		}
		
		System.setOut(stdout);
		
		println();
		println();
		println();
	}
	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

	private void performEvaluationBurnIn(Context context, RandomCondtionalArithmeticExpressionGenerator expressionGenerator, int burnInIterations) {
		ArrayList<Expression> expressions = generateRandomExpressionList(expressionGenerator, context, burnInIterations);
		long time = Timer.time(()->evaluateExpressions(expressions, context));
		myAssert(time >= 0, ()->"ERROR: timing of the evaluation produced a negative number!");
	}


	private void printRatios(long trueContextEvaluationTime, long smtBasedContextEvaluationTime) {
		println(smallTab + "      trueContext.evaluate() time:  " + trueContextEvaluationTime);
		println(smallTab + "  smtBasedContext.evaluate() time:  " + smtBasedContextEvaluationTime);
		try{
			println(smallTab + "                            ratio:  " + Math.round(1.0*trueContextEvaluationTime/smtBasedContextEvaluationTime) + ":1");
		}
		catch(ArithmeticException e) {
			println(smallTab + "                            ratio:  " + trueContextEvaluationTime + ":0");
		}
	}
	
	private void printRatios(long trueContextEvaluationTime, long modifiedTrueContextEvaluateTime, long smtBasedContextEvaluationTime) {
		println(smallTab + "                trueContext.evaluate() time:  " + trueContextEvaluationTime);
		println(smallTab + "        modifiedTrueContext.evaluate() time:  " + modifiedTrueContextEvaluateTime);
		println(smallTab + "            smtBasedContext.evaluate() time:  " + smtBasedContextEvaluationTime);
		try{
			println(smallTab + "  TrueContext:ModifiedTrueContext ratio:  " + Math.round(1.0*trueContextEvaluationTime/modifiedTrueContextEvaluateTime) + ":1");
		}
		catch(ArithmeticException e) {
			println(smallTab + "  TrueContext:ModifiedTrueContext ratio:  " + trueContextEvaluationTime + ":0");
		}
		try{
			println(smallTab + "             TrueContext:SMTBased ratio:  " + Math.round(1.0*trueContextEvaluationTime/smtBasedContextEvaluationTime) + ":1");
		}
		catch(ArithmeticException e) {
			println(smallTab + "             TrueContext:SMTBased ratio:  " + trueContextEvaluationTime + ":0");
		}
	}

	private void printTestSpecs(final int numberOfExpressionsToTestOn, final int numberOfInternalNodes, final int numberOfLiterals,
			final String[] types, final int numVariables) {
		println(smallTab + "                                        Number of Variables:  " + numVariables);
		println(smallTab + "                                             Variable Types:  " + Arrays.toString(types));
		println(smallTab + "                       Number of Random Expressions to Test:  " + numberOfExpressionsToTestOn);
		println(smallTab + " Number of Internal Nodes in Randomly Generated Expressions:  " + numberOfInternalNodes);
		println(smallTab + "       Number of Literals in Randomly Generated Expressions:  " + numberOfLiterals);
		printHyphenSepartorLine();
	}
	


	
	
	
	
	
	
	private ArrayList<Expression> generateRandomExpressionList(RandomCondtionalArithmeticExpressionGenerator expressionGenerator,
			Context context, int numExpressions) {
		ArrayList<Expression> expressionsList = new ArrayList<>(numExpressions);
		for(int i = 0; i < numExpressions; ++i) {
			Expression expression = expressionGenerator.apply();
			Expression modifiedExpression = makeCopyOfExpressionAlteringAnyNumericalConstantsSoThatTheyAreWithinPresetBounds(expression, context, rand);
			expressionsList.add(modifiedExpression);
		}
		return expressionsList;
	}
	
	private Expression evaluateExpressions(ArrayList<Expression> expressions, Context context) {
		Expression evaluated = null;
		for(Expression expression : expressions) {
			evaluated = context.evaluate(expression);
		}
		assert(evaluated != null);
		return evaluated;
	}

	
	
	
	

	@BeforeClass
	public static void prepareSymbolsAndTypesAndLiterals() throws Exception {
		printTestTitle("Default SMTBasedEvaluator Performance Test", true);
		
		println();
		println();
		println();
	}
	
	
	@Before
	public void resetForNextTest() throws Exception {
		resetYices();
	}
	
	public void resetYices() throws Exception {
		Yices.reset();
	}
	
	
	final static String tab = "\t";
	final static String stab_big = "        ";
	final static String smallTab = "    ";

	public static Random makeRandom() {
		return new Random();
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
	
	private static List<Expression> parseStringsToExpressionList(String[] expressionStrings) {
		ArrayList<Expression> expressions = new ArrayList<Expression>(expressionStrings.length);
		for(String constraintString : expressionStrings) {
			Expression constraint = parse(constraintString);
			expressions.add(constraint);
		}
		return expressions;
	}
	
	private static String[] prepareSymbolsAndTypes(int numVariables, String... types) {
		myAssert(numVariables >= types.length, ()->"ERROR: number of variables requested needs to be at least as many as the number of types passed in!");
		int numTypes = types.length;
		int numVariablesPerType = numVariables/numTypes;
		int variableNameLength = (int) Math.ceil( Math.pow(numVariables, 1.0/25) );
		
		char[] variableName = new char[variableNameLength];
		Arrays.fill(new char[variableNameLength], '\0');
		variableName[0] = 'A';
		int symbolsAndTypesIdx = 0;
		
		String[] symbolsAndTypes = new String[numVariables*2];
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
		
		return symbolsAndTypes;
	}

	public Context conjoinConstraints(Context smtBasedContext, List<Expression> constraints) {
		for(Expression constraint : constraints) {
			smtBasedContext = smtBasedContext.conjoin(constraint);
		}
		return smtBasedContext;
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
	
//	private static void prepareLiterals() throws Exception {
//		Context context = new TrueContext(theory);
//		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
//		Map<String, Type> variableNamesAndTypes = map();
//		for(int i = 0; i < numVariables; ++i) {
//			String symbol = symbolsAndTypes[i*2];
//			Expression variable = parse(symbolsAndTypes[i*2]);
//			Type type = context.getTypeOfRegisteredSymbol(variable);
////			if(type == GrinderUtil.REAL_TYPE) {
////				type = GrinderUtil.INTEGER_TYPE;
////			}
//			variableNamesAndTypes.put(symbol,  type);
//		}
//		CompoundTheory cmpdTheory = new CompoundTheory(
//				new EqualityTheory(false, true),
//				new DifferenceArithmeticTheory(false, false),
//				new LinearRealArithmeticTheory(false, false),
//				new TupleTheory(),
//				new PropositionalTheory(),
//				new BruteForceFunctionTheory()
//			);
//		TheoryTestingSupport theoryTestingSupport = new CompoundTheoryTestingSupport(cmpdTheory, rand);
//		theoryTestingSupport.setVariableNamesAndTypesForTesting(variableNamesAndTypes);
//		//context = theoryTestingSupport.makeContextWithTestingInformation();
//		if(printLiterals) {
//			println(smallTab + "Literals:");
//		}
//		for(int j = 0; j < inflatedMaxNumLiterals; ++j) {
//			Expression literal = theoryTestingSupport.makeRandomLiteral(context);							//println(tab + "Original: " + literal);
//			
//			//reducing constants smaller numbers
//			literal = makeCopyOfExpressionAlteringAnyConstantsSoThatTheyAreWithinPredeterminedBounds(literal, context, rand);
//			
//			literals.add(literal);
//			if(printLiterals) {
//				println(tab + literal);				
//			}
//		}
//	}

	public static Expression makeCopyOfExpressionAlteringAnyNumericalConstantsSoThatTheyAreWithinPresetBounds(Expression expression, Context context, Random rand) {
		List<Expression> literalArguments = expression.getArguments();
		final BigIntegerNumberExact one = new BigIntegerNumberExact(1);
		final int exclusiveIntMaxNumOrDen = (int)Math.pow(10, maxNumberOfDigitsInNumeratorOrDenominatorInRandomlyCreatedConstants);
		final BigIntegerNumber exclusiveBigIntegerMaxNumOrDen = new BigIntegerNumberExact(exclusiveIntMaxNumOrDen);
		for(Expression argument : literalArguments) {
			if(context.isUniquelyNamedConstant(argument)) {													//println(argument);
				Rational rationalValue = argument.rationalValue();
				if(rationalValue != null) {
					BigIntegerNumber num = rationalValue.getNumerator().abs();
					BigIntegerNumber den = rationalValue.getDenominator().abs();							//println(argStr);
					if(num.compareTo(exclusiveBigIntegerMaxNumOrDen)>= 0) {
						int newNumber = rand.nextInt()%(exclusiveIntMaxNumOrDen);															
						num = new BigIntegerNumberExact(newNumber);
					}
					if(den.compareTo(exclusiveBigIntegerMaxNumOrDen)>0) {
						int newNumber = rand.nextInt()%(exclusiveIntMaxNumOrDen);
						while(newNumber == 0) {
							newNumber = rand.nextInt()%(exclusiveIntMaxNumOrDen);
						}
						den = new BigIntegerNumberExact(newNumber);
					}
					Expression newConstant = makeSymbol(new Rational(num,den));
					expression = expression.replaceAllOccurrences(argument, newConstant, context);			//println(tab + "        : " + argument + " --> " + newConstant);
				}
			}
			else if(argument.getFunctor() != null) { //argument has functor
				expression = expression.replaceAllOccurrences(argument, makeCopyOfExpressionAlteringAnyNumericalConstantsSoThatTheyAreWithinPresetBounds(argument, context, rand), context);
			}
		}
		return expression;
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
		printHyphenSepartorLine();
	}
	
	public static void printHyphenSepartorLine() {
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
