package com.sri.ai.grinder.tester;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.myAssert;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.compound.CompoundTheoryTestingSupport;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.function.BruteForceFunctionTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.theory.tuple.TupleTheory;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.math.BigIntegerNumber;
import com.sri.ai.util.math.BigIntegerNumberExact;
import com.sri.ai.util.math.Rational;

public class RandomCondtionalArithmeticExpressionGenerator implements NullaryFunction<Expression> {
	
	static final int maxNumberOfDigitsInNumeratorOrDenominatorInRandomlyCreatedConstants = 3;
	
	private final Context context;
	private final TheoryTestingSupport theoryTestingSupport;
	private final boolean randomizeTreeSize;
	private final int numberOfFunctionalNodes;
	private final int numberOfLiterals;
	private final String treeType;
	private final Random rand;
	
	private final ArrayList<Expression> arithmeticVariableSymbols;
	
	private double presetChanceOfLiteral = -1;
	
	private static final ArrayList<Expression> DIFFERENCE_ARITHMETIC_FUNCTORS =
	new ArrayList<>() {
		private static final long serialVersionUID = 1L;
		{
			add(Expressions.makeSymbol("+"));
			add(Expressions.makeSymbol("-"));
			add(Expressions.makeSymbol("*"));
			add(Expressions.makeSymbol("/"));
		}
	};
	private final Expression DIVISION_FUNCTOR = Expressions.makeSymbol("/");
	
	private final Map<String, BiFunction<Integer, Integer, Expression>> randomExpressionGeneratorMethods = new HashMap<>() {
		{
			put("LinearTree", (numFunctions,numLiterals) -> applyAsLinearExpressionTree(numFunctions,numLiterals));
			put("BalancedTree", (numFunctions,numLiterals) -> applyAsFullExpressionTree(numFunctions,numLiterals));
		}
	};
	
	public RandomCondtionalArithmeticExpressionGenerator(
			Context context, String treeType, int numberOfFunctionalNodes, int numberOfLiterals, boolean randomizeTreeSize, Random randomNumberGenerator) {
		myAssert(numberOfFunctionalNodes > 0, ()->"ERROR: must set a target of at least 1 for the total number of internal nodes!!!");
		myAssert(numberOfFunctionalNodes >= 0, ()->"ERROR: the set target for number of literals cannot be negative!!!");
		myAssert(numberOfFunctionalNodes <= numberOfFunctionalNodes, ()->"ERROR: set target for number of literals cannot be greater than the set target for number of total internal nodes");
		myAssert(randomExpressionGeneratorMethods.containsKey(treeType), ()->"ERROR: there is no tree generation method mapped to passed in tree type: '" + treeType + "'!!!");
		this.context = context;
		this.treeType = treeType;
		this.arithmeticVariableSymbols = extractSymbolsWithArithmeticTypes();
		this.numberOfFunctionalNodes = numberOfFunctionalNodes;
		this.numberOfLiterals = numberOfLiterals;
		this.randomizeTreeSize = randomizeTreeSize;
		this.rand = randomNumberGenerator;
		this.theoryTestingSupport = consructDifferenceArithmeticTheoryTestingSupport(context, randomNumberGenerator);
	}
	
	private ArrayList<Expression> extractSymbolsWithArithmeticTypes() {
		ArrayList<Expression> arithmeticSymbols = new ArrayList<Expression>();
		Set<Expression> symbols = context.getSymbols();
		for(Expression symbol : symbols) {
			if(isArithmeticType(symbol)) {
				arithmeticSymbols.add(symbol);
			}
		}
		return arithmeticSymbols;
	}
	
	private boolean isArithmeticType(Expression symbol) {
		boolean symbolIsArithmetic = false;
		Type type = context.getTypeOfRegisteredSymbol(symbol);
		if(type == GrinderUtil.INTEGER_TYPE) {
			symbolIsArithmetic = true;
		}
		else if(type instanceof IntegerInterval) {
			symbolIsArithmetic = true;
		}
		else if(type == GrinderUtil.REAL_TYPE) {
			symbolIsArithmetic = true;
		}
		else if(type instanceof RealInterval) {
			symbolIsArithmetic = true;
		}
		return symbolIsArithmetic;
	}

	@Override
	public Expression apply() {
		BiFunction<Integer, Integer,Expression> randomExpressionGeneratorMethod = randomExpressionGeneratorMethods.get(treeType);
		Expression result = randomExpressionGeneratorMethod.apply(numberOfFunctionalNodes, numberOfLiterals);
		return result;
	}
	
	public void setChanceOfGeneratingALiteral(double chanceOfGeneratingLiteral) {
		presetChanceOfLiteral = chanceOfGeneratingLiteral;
	}
	
	private Expression applyAsLinearExpressionTree(int numberOfFunctionalNodesLeft, int numberOfLiteralsLeft) {
		myAssert(numberOfFunctionalNodesLeft >= numberOfLiteralsLeft, ()->"ERROR: for some reason the number of functional nodes remaining is fewer than the number of literals remaining!!!");
		if(randomizeTreeSize) {
			if(numberOfFunctionalNodesLeft != 0 && rand.nextInt(numberOfFunctionalNodesLeft)==0) {
				numberOfFunctionalNodesLeft = 0;
			}
		}
		Expression result;
		if(numberOfFunctionalNodesLeft == 0) {
			result = createArithmeticLeaf();
		}
		else {
			double chanceOfLiteral = presetChanceOfLiteral < 0 ? (double)numberOfLiteralsLeft / numberOfFunctionalNodesLeft : 
				numberOfLiteralsLeft == 0 ? 0 : numberOfLiteralsLeft == numberOfFunctionalNodesLeft ? 1 : presetChanceOfLiteral;
			int periodForGettingLiteral = chanceOfLiteral == 0 ? -1 : Math.toIntExact(Math.round(1/chanceOfLiteral));
			
			int newNumberOfFunctionalNodesLeft = numberOfFunctionalNodesLeft-1;
			
			boolean leftLeafIsExpandable = rand.nextBoolean();
			
			if(chanceOfLiteral != 0 && rand.nextInt(periodForGettingLiteral)==0) {
				Expression literal = theoryTestingSupport.makeRandomLiteral(context);
				literal = makeCopyOfExpressionAlteringAnyNumericalConstantsSoThatTheyAreWithinPresetBounds(literal,context,rand);
				
				int newNumberOfLiteralsLeft = numberOfLiteralsLeft-1;
				
				if(leftLeafIsExpandable) {
					result = IfThenElse.make(literal, applyAsLinearExpressionTree(newNumberOfFunctionalNodesLeft, newNumberOfLiteralsLeft), createArithmeticLeaf());							
				}
				else {
					result = IfThenElse.make(literal, createArithmeticLeaf(), applyAsLinearExpressionTree(newNumberOfFunctionalNodesLeft, newNumberOfLiteralsLeft));		
				}
			}
			else {
				int functorIdx = rand.nextInt(DIFFERENCE_ARITHMETIC_FUNCTORS.size());
				Expression functor = DIFFERENCE_ARITHMETIC_FUNCTORS.get(functorIdx);
				ArrayList<Expression> functionArguments = new ArrayList<Expression>(2);
				if(functor != DIVISION_FUNCTOR) {
					if(leftLeafIsExpandable) {
						functionArguments.add(applyAsLinearExpressionTree(newNumberOfFunctionalNodesLeft, numberOfLiteralsLeft));
						functionArguments.add(createArithmeticLeaf());
					}
					else {
						functionArguments.add(createArithmeticLeaf());
						functionArguments.add(applyAsLinearExpressionTree(newNumberOfFunctionalNodesLeft, numberOfLiteralsLeft));
					}
				}
				else {
					functionArguments.add(applyAsLinearExpressionTree(newNumberOfFunctionalNodesLeft, numberOfLiteralsLeft));
					functionArguments.add(createNonZeroArithmeticConstant());											
				}
				result = new DefaultFunctionApplication(functor, functionArguments);
			}
		}
		return result;
	}
	
	private Expression applyAsFullExpressionTree(int numberOfFunctionalNodesLeft, int numberOfLiteralsLeft) {
		myAssert(numberOfFunctionalNodesLeft >= numberOfLiteralsLeft, ()->"ERROR: for some reason the number of functional nodes remaining is fewer than the number of literals remaining!!!");
		if(randomizeTreeSize) {
			if(numberOfFunctionalNodesLeft != 0 && rand.nextInt(numberOfFunctionalNodesLeft)==0) {
				numberOfFunctionalNodesLeft = 0;
			}
		}
		Expression result;
		if(numberOfFunctionalNodesLeft == 0) {
			result = createArithmeticLeaf();
		}
		else {
			double chanceOfLiteral = presetChanceOfLiteral < 0 ? (double)numberOfLiteralsLeft / numberOfFunctionalNodesLeft : 
				numberOfLiteralsLeft == 0 ? 0 : numberOfLiteralsLeft == numberOfFunctionalNodesLeft ? 1 : presetChanceOfLiteral;
			int periodForGettingLiteral = chanceOfLiteral == 0 ? -1 : Math.toIntExact(Math.round(1/chanceOfLiteral));
			
			int newNumberOfFunctionalNodesLeft = numberOfFunctionalNodesLeft - 1;
			int leftSubtreeNumberOfFunctionalNodes = (newNumberOfFunctionalNodesLeft)/2;
			int rightSubtreeNumberOfFunctionalNodes = newNumberOfFunctionalNodesLeft - leftSubtreeNumberOfFunctionalNodes;
			
			int newNumberOfLiteralsLeft;
			int leftSubtreeNumberOfLiterals;
			int rightSubtreeNumberOfLiterals;
			
			if(chanceOfLiteral != 0 && rand.nextInt(periodForGettingLiteral)==0) {
				Expression literal = theoryTestingSupport.makeRandomLiteral(context);
				literal = makeCopyOfExpressionAlteringAnyNumericalConstantsSoThatTheyAreWithinPresetBounds(literal,context,rand);
				
				newNumberOfLiteralsLeft = numberOfLiteralsLeft - 1;
				leftSubtreeNumberOfLiterals = (newNumberOfLiteralsLeft)/2;
				rightSubtreeNumberOfLiterals = newNumberOfLiteralsLeft - leftSubtreeNumberOfLiterals;
				
				result = IfThenElse.make(
						literal, 
						applyAsFullExpressionTree(leftSubtreeNumberOfFunctionalNodes, leftSubtreeNumberOfLiterals), 
						applyAsFullExpressionTree(rightSubtreeNumberOfFunctionalNodes, rightSubtreeNumberOfLiterals)
						);							
			}
			else {
				newNumberOfLiteralsLeft = numberOfLiteralsLeft;
				leftSubtreeNumberOfLiterals = (newNumberOfLiteralsLeft)/2;
				rightSubtreeNumberOfLiterals = newNumberOfLiteralsLeft - leftSubtreeNumberOfLiterals;
				
				int functorIdx = rand.nextInt(DIFFERENCE_ARITHMETIC_FUNCTORS.size());
				Expression functor = DIFFERENCE_ARITHMETIC_FUNCTORS.get(functorIdx);
				ArrayList<Expression> functionArguments = new ArrayList<Expression>(2);
				if(functor != DIVISION_FUNCTOR) {
					functionArguments.add(applyAsFullExpressionTree(leftSubtreeNumberOfFunctionalNodes, leftSubtreeNumberOfLiterals));
					functionArguments.add(applyAsFullExpressionTree(rightSubtreeNumberOfFunctionalNodes, rightSubtreeNumberOfLiterals));
				}
				else {
					functionArguments.add(applyAsFullExpressionTree(newNumberOfFunctionalNodesLeft, newNumberOfLiteralsLeft));
					functionArguments.add(createNonZeroArithmeticConstant());											
				}
				result = new DefaultFunctionApplication(functor, functionArguments);
			}
		}
		return result;
	}


	private Expression createArithmeticLeaf() {
		Expression result;
		if(rand.nextInt(2)==0 || arithmeticVariableSymbols.size()==0) {
			result = createArithmeticConstant();
		}
		else {
			result = sampleArithmeticVariable();			
		}
		return result;
	}

	private Expression createArithmeticConstant() {
		Expression result;
		int modulusValue = (int) Math.pow(10, maxNumberOfDigitsInNumeratorOrDenominatorInRandomlyCreatedConstants);
		BigIntegerNumberExact num = new BigIntegerNumberExact( Math.abs(rand.nextInt() % (modulusValue)) ); 
		BigIntegerNumberExact den = new BigIntegerNumberExact( 1 + Math.abs(rand.nextInt() % (modulusValue-1)) ); 
		Rational r = new Rational(num,den);
		result = makeSymbol(r);
		return result;
	}
	
	private Expression createNonZeroArithmeticConstant() {
		Expression result;
		int modulusValue = (int) Math.pow(10, maxNumberOfDigitsInNumeratorOrDenominatorInRandomlyCreatedConstants);
		BigIntegerNumberExact num = new BigIntegerNumberExact( 1 + Math.abs(rand.nextInt() % (modulusValue-1)) ); 
		BigIntegerNumberExact den = new BigIntegerNumberExact( 1 + Math.abs(rand.nextInt() % (modulusValue-1)) ); 
		Rational r = new Rational(num,den);
		result = makeSymbol(r);
		return result;
	}
	
	public Expression sampleArithmeticVariable() {
		Expression result;
		int symbolIdx = rand.nextInt(arithmeticVariableSymbols.size());
		result = arithmeticVariableSymbols.get(symbolIdx);
		return result;
	}

	public TheoryTestingSupport consructDifferenceArithmeticTheoryTestingSupport(Context context, Random randomNumberGenerator) {
		Map<Expression,Expression> symbolsAndTypesAsExpressions = context.getSymbolsAndTypes();
		Map<String, Type> symbolsAndTypesAsStringsAndTypes = map();
		for(Map.Entry<Expression,Expression> entry : symbolsAndTypesAsExpressions.entrySet()) {
			Expression variable = entry.getKey();
			String symbol = variable.toString();
			Type type = context.getTypeOfRegisteredSymbol(variable);
			//TODO: extend for interval types
			symbolsAndTypesAsStringsAndTypes.put(symbol,  type);
		}
//		CompoundTheory cmpdTheory = new CompoundTheory(
//				new EqualityTheory(false, true),
//				new DifferenceArithmeticTheory(false, false),
//				new LinearRealArithmeticTheory(false, false)
//			);
		CompoundTheory cmpdTheory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory(),
				new BruteForceFunctionTheory()
			);
		TheoryTestingSupport theoryTestingSupport = new CompoundTheoryTestingSupport(cmpdTheory, randomNumberGenerator);
		theoryTestingSupport.setVariableNamesAndTypesForTesting(symbolsAndTypesAsStringsAndTypes);
		return theoryTestingSupport;
	}
	
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
//		List<Expression> literalArguments = literal.getArguments();
//		for(Expression argument : literalArguments) {
//			if(context.isUniquelyNamedConstant(argument)) {													//println(argument);
//				String argStr = argument.toString();														//println(argStr);
//				int numDigits = argStr.length() - (argStr.charAt(0) == '-' ? 1 : 0);						//println(tab + "        : " + numDigits);
//				if(numDigits > maxNumDigitsForConstantsInRandomlyCreatedLiterals) {
//					int newNumber = rand.nextInt()%((int)Math.pow(10, maxNumDigitsForConstantsInRandomlyCreatedLiterals));															
//					Expression newConstant = parse(""+newNumber);
//					literal = literal.replaceAllOccurrences(argument, newConstant, context);				//println(tab + "        : " + argument + " --> " + newConstant);
//				}
//			}
//			else if(argument.getFunctor() != null) { //argument has functor
//				literal = literal.replaceAllOccurrences(argument, makeCopyOfExpressionAlteringAnyConstantsSoThatTheyAreWithinPredeterminedBounds(argument, context, rand), context);
//			}
//		}
//		return literal;
	}
	
//	static final int maxNumDigitsForConstantsInRandomlyCreatedLiterals = 3;
//	static final double chanceOfEarlyLeafGenerationInsteadOfFunctionApplication = 0.1;
//	
//	private final Context context;
//	private final TheoryTestingSupport theoryTestingSupport;
//	private final boolean randomizeDepth;
//	private final int maxDepth;
//	private final Random rand;
//	
//	private final double chanceOfLiteral;
//	private final ArrayList<Expression> arithmeticSymbols;
//	
//	private static final ArrayList<Expression> DIFFERENCE_ARITHMETIC_FUNCTORS =
//	new ArrayList<>() {
//		private static final long serialVersionUID = 1L;
//		{
//			add(Expressions.makeSymbol("+"));
//			add(Expressions.makeSymbol("-"));
//			add(Expressions.makeSymbol("*"));
//			add(Expressions.makeSymbol("/"));
//		}
//	};
//	private final Expression DIVISION_FUNCTOR = Expressions.makeSymbol("/");
//	
//	final Map<String, Function<Integer, Expression>> randomExpressionGeneratorMethods = new HashMap<>() {
//		{
//			put("LinearTree", (d) -> applyAsLinearExpressionTree(d));
//			put("FullTree", (d) -> applyAsFullExpressionTree(d));
//		}
//	};
//	private String treeType;
//	
//	public RandomCondtionalArithmeticExpressionGenerator(
//			Context context, String treeType, int maxDepth, boolean randomizeDepth, Random randomNumberGenerator) {
//		myAssert(maxDepth >= 0, ()->"ERROR: maxDepth cannot be negative!!!");
//		this.context = context;
//		this.treeType = treeType;
//		this.arithmeticSymbols = extractSymbolsWithArithmeticTypes();
//		this.maxDepth = maxDepth;
//		this.randomizeDepth = randomizeDepth;
//		this.rand = randomNumberGenerator;
//		this.chanceOfLiteral = 0.3;
//		this.theoryTestingSupport = consructDifferenceArithmeticTheoryTestingSupport(context, randomNumberGenerator);
//	}
//
//	public RandomCondtionalArithmeticExpressionGenerator(
//			Context context, String treeType, int maxDepth, boolean randomizeDepth, double chanceOfLiteral,  Random randomNumberGenerator) {
//		myAssert(maxDepth >= 0, ()->"ERROR: maxDepth cannot be negative!!!");
//		myAssert(chanceOfLiteral < 1.0 && chanceOfLiteral >= 0.0, ()->"ERROR: chanceOfLiteral must be in the interval [0,1)!!!");
//		this.context = context;
//		this.treeType = treeType;
//		this.arithmeticSymbols = extractSymbolsWithArithmeticTypes();
//		this.maxDepth = maxDepth;
//		this.randomizeDepth = randomizeDepth;
//		this.chanceOfLiteral = chanceOfLiteral;
//		this.rand = randomNumberGenerator;
//		this.theoryTestingSupport = consructDifferenceArithmeticTheoryTestingSupport(context, randomNumberGenerator);
//	}
//	
//	private ArrayList<Expression> extractSymbolsWithArithmeticTypes() {
//		ArrayList<Expression> arithmeticSymbols = new ArrayList<Expression>();
//		Set<Expression> symbols = context.getSymbols();
//		for(Expression symbol : symbols) {
//			if(isArithmeticType(symbol)) {
//				arithmeticSymbols.add(symbol);
//			}
//		}
//		return arithmeticSymbols;
//	}
//	
//	private boolean isArithmeticType(Expression symbol) {
//		boolean symbolIsArithmetic = false;
//		Type type = context.getTypeOfRegisteredSymbol(symbol);
//		if(type == GrinderUtil.INTEGER_TYPE) {
//			symbolIsArithmetic = true;
//		}
//		else if(type instanceof IntegerInterval) {
//			symbolIsArithmetic = true;
//		}
//		else if(type == GrinderUtil.REAL_TYPE) {
//			symbolIsArithmetic = true;
//		}
//		else if(type instanceof RealInterval) {
//			symbolIsArithmetic = true;
//		}
//		return symbolIsArithmetic;
//	}
//
//	@Override
//	public Expression apply() {
//		Function<Integer,Expression> randomExpressionGeneratorMethod = randomExpressionGeneratorMethods.get(treeType);
//		Expression result = randomExpressionGeneratorMethod.apply(maxDepth);
//		return result;
//	}
//	
//	private Expression applyAsLinearExpressionTree(int depth) {
//		if(randomizeDepth) {
//			depth = rand.nextInt(depth+1);
//		}
//		Expression result;
//		if (depth != 0)  {
//			boolean leftLeafIsExpandable = rand.nextBoolean();
//			int periodForGettingLiteral = Math.toIntExact(Math.round(1/chanceOfLiteral));
//			if(rand.nextInt(periodForGettingLiteral)==0) {
//				Expression literal = theoryTestingSupport.makeRandomLiteral(context);
//				literal = makeCopyOfExpressionAlteringAnyConstantsSoThatTheyAreWithinPredeterminedBounds(literal,context,rand);
//				if(leftLeafIsExpandable) {
//					result = IfThenElse.make(literal, applyAsLinearExpressionTree(depth - 1), createArithmeticLeaf());							
//				}
//				else {
//					result = IfThenElse.make(literal, createArithmeticLeaf(), applyAsLinearExpressionTree(depth - 1));		
//				}
//			}
//			else {
//				int functorIdx = rand.nextInt(DIFFERENCE_ARITHMETIC_FUNCTORS.size());
//				Expression functor = DIFFERENCE_ARITHMETIC_FUNCTORS.get(functorIdx);
//				ArrayList<Expression> functionArguments = new ArrayList<Expression>(2);
//				if(functor != DIVISION_FUNCTOR) {
//					if(leftLeafIsExpandable) {
//						functionArguments.add(applyAsLinearExpressionTree(depth-1));
//						functionArguments.add(createArithmeticLeaf());
//					}
//					else {
//						functionArguments.add(createArithmeticLeaf());
//						functionArguments.add(applyAsLinearExpressionTree(depth-1));
//					}
//				}
//				else {
//					functionArguments.add(applyAsLinearExpressionTree(depth-1));
//					functionArguments.add(createNonZeroArithmeticConstant());											
//				}
//				result = new DefaultFunctionApplication(functor, functionArguments);
//			}
//		}
//		else{
//			result = createArithmeticLeaf();
//		}
//		return result;
//	}
//	
//	private Expression applyAsFullExpressionTree(int depth) {
//		if(randomizeDepth) {
//			depth = rand.nextInt(depth+1);
//		}
//		Expression result;
//		if (depth != 0)  {
//			int periodForGettingLiteral = Math.toIntExact(Math.round(1/chanceOfLiteral));
//			if(rand.nextInt(periodForGettingLiteral)==0) {
//				Expression literal = theoryTestingSupport.makeRandomLiteral(context);
//				literal = makeCopyOfExpressionAlteringAnyConstantsSoThatTheyAreWithinPredeterminedBounds(literal,context,rand);
//				result = IfThenElse.make(literal, applyAsFullExpressionTree(depth - 1), applyAsFullExpressionTree(depth - 1));							
//			}
//			else {
//				int functorIdx = rand.nextInt(DIFFERENCE_ARITHMETIC_FUNCTORS.size());
//				Expression functor = DIFFERENCE_ARITHMETIC_FUNCTORS.get(functorIdx);
//				ArrayList<Expression> functionArguments = new ArrayList<Expression>(2);
//				if(depth != 1) {
//					while(functor == DIVISION_FUNCTOR) {
//						functorIdx = rand.nextInt(DIFFERENCE_ARITHMETIC_FUNCTORS.size());
//						functor = DIFFERENCE_ARITHMETIC_FUNCTORS.get(functorIdx);
//					}
//					functionArguments.add(applyAsFullExpressionTree(depth - 1));
//					functionArguments.add(applyAsFullExpressionTree(depth - 1));
//				}
//				else { //depth == 1
//					if(functor != DIVISION_FUNCTOR) {
//						functionArguments.add(createArithmeticLeaf());
//						functionArguments.add(createArithmeticLeaf());
//					}
//					else {
//						functionArguments.add(createArithmeticLeaf());
//						functionArguments.add(createNonZeroArithmeticConstant());											
//					}
//				}
//				result = new DefaultFunctionApplication(functor, functionArguments);
//			}
//		}
//		else{
//			result = createArithmeticLeaf();
//		}
//		return result;
//	}
//
//
//	private Expression createArithmeticLeaf() {
//		Expression result;
//		if(rand.nextInt(2)==0 || arithmeticSymbols.size()==0) {
//			result = createArithmeticConstant();
//		}
//		else {
//			result = sampleArithmeticVariable();			
//		}
//		return result;
//	}
//
//	private Expression createArithmeticConstant() {
//		Expression result;
//		int modulusValue = (int) Math.pow(10, maxNumDigitsForConstantsInRandomlyCreatedLiterals);
//		BigIntegerNumberExact num = new BigIntegerNumberExact( Math.abs(rand.nextInt() % (modulusValue)) ); 
//		BigIntegerNumberExact den = new BigIntegerNumberExact( 1 + Math.abs(rand.nextInt() % (modulusValue-1)) ); 
//		Rational r = new Rational(num,den);
//		result = makeSymbol(r);
//		return result;
//	}
//	
//	private Expression createNonZeroArithmeticConstant() {
//		Expression result;
//		int modulusValue = (int) Math.pow(10, maxNumDigitsForConstantsInRandomlyCreatedLiterals);
//		BigIntegerNumberExact num = new BigIntegerNumberExact( 1 + Math.abs(rand.nextInt() % (modulusValue-1)) ); 
//		BigIntegerNumberExact den = new BigIntegerNumberExact( 1 + Math.abs(rand.nextInt() % (modulusValue-1)) ); 
//		Rational r = new Rational(num,den);
//		result = makeSymbol(r);
//		return result;
//	}
//	
//	public Expression sampleArithmeticVariable() {
//		Expression result;
//		int symbolIdx = rand.nextInt(arithmeticSymbols.size());
//		result = arithmeticSymbols.get(symbolIdx);
//		return result;
//	}
//
//	public TheoryTestingSupport consructDifferenceArithmeticTheoryTestingSupport(Context context, Random randomNumberGenerator) {
//		Map<Expression,Expression> symbolsAndTypesAsExpressions = context.getSymbolsAndTypes();
//		Map<String, Type> symbolsAndTypesAsStringsAndTypes = map();
//		for(Map.Entry<Expression,Expression> entry : symbolsAndTypesAsExpressions.entrySet()) {
//			Expression variable = entry.getKey();
//			String symbol = variable.toString();
//			Type type = context.getTypeOfRegisteredSymbol(variable);
//			//TODO: extend for interval types
//			if(type == GrinderUtil.REAL_TYPE || type instanceof RealInterval) {
//				//continue;
//				//TODO: once literals with mix of integers and real types can be handled, uncomment below...
////				type = GrinderUtil.INTEGER_TYPE;
//			}
//			symbolsAndTypesAsStringsAndTypes.put(symbol,  type);
//		}
////		CompoundTheory cmpdTheory = new CompoundTheory(
////				new EqualityTheory(false, true),
////				new DifferenceArithmeticTheory(false, false),
////				new LinearRealArithmeticTheory(false, false)
////			);
//		CompoundTheory cmpdTheory = new CompoundTheory(
//				new EqualityTheory(false, true),
//				new DifferenceArithmeticTheory(false, false),
//				new LinearRealArithmeticTheory(false, false),
//				new TupleTheory(),
//				new PropositionalTheory(),
//				new BruteForceFunctionTheory()
//			);
//		TheoryTestingSupport theoryTestingSupport = new CompoundTheoryTestingSupport(cmpdTheory, randomNumberGenerator);
//		theoryTestingSupport.setVariableNamesAndTypesForTesting(symbolsAndTypesAsStringsAndTypes);
//		return theoryTestingSupport;
//	}
//	
//	public static Expression makeCopyOfExpressionAlteringAnyConstantsSoThatTheyAreWithinPredeterminedBounds(Expression literal, Context context, Random rand) {
//		List<Expression> literalArguments = literal.getArguments();
//		for(Expression argument : literalArguments) {
//			if(context.isUniquelyNamedConstant(argument)) {													//println(argument);
//				String argStr = argument.toString();														//println(argStr);
//				int numDigits = argStr.length() - (argStr.charAt(0) == '-' ? 1 : 0);						//println(tab + "        : " + numDigits);
//				if(numDigits > maxNumDigitsForConstantsInRandomlyCreatedLiterals) {
//					int newNumber = rand.nextInt()%((int)Math.pow(10, maxNumDigitsForConstantsInRandomlyCreatedLiterals));															
//					Expression newConstant = parse(""+newNumber);
//					literal = literal.replaceAllOccurrences(argument, newConstant, context);				//println(tab + "        : " + argument + " --> " + newConstant);
//				}
//			}
//			else if(argument.getFunctor() != null) { //argument has functor
//				literal = literal.replaceAllOccurrences(argument, makeCopyOfExpressionAlteringAnyConstantsSoThatTheyAreWithinPredeterminedBounds(argument, context, rand), context);
//			}
//		}
//		return literal;
//	}

}
