package com.sri.ai.grinder.tester;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.myAssert;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

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
import com.sri.ai.util.math.BigIntegerNumberExact;
import com.sri.ai.util.math.Rational;

public class RandomCondtionalDifferenceArithmeticExpressionGenerator implements NullaryFunction<Expression> {
	
	static final int maxNumDigitsForConstantsInRandomlyCreatedLiterals = 1;
	static final double chanceOfEarlyLeafGenerationInsteadOfFunctionApplication = 0.1;
	
	private final Context context;
	private final TheoryTestingSupport theoryTestingSupport;
	private final int maxDepth;
	private final Random randomNumberGenerator;
	
	private final double chanceOfLiteral;
	private final ArrayList<Expression> arithmeticSymbols;
	
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
	
	public RandomCondtionalDifferenceArithmeticExpressionGenerator(
			Context context, int maxDepth, Random randomNumberGenerator) {
		myAssert(maxDepth >= 0, ()->"ERROR: maxDepth cannot be negative!!!");
		this.context = context;
		this.arithmeticSymbols = extractSymbolsWithArithmeticTypes();
		this.maxDepth = maxDepth;
		this.randomNumberGenerator = randomNumberGenerator;
		this.chanceOfLiteral = 0.3;
		this.theoryTestingSupport = consructDifferenceArithmeticTheoryTestingSupport(context, randomNumberGenerator);
	}

	public RandomCondtionalDifferenceArithmeticExpressionGenerator(
			Context context, int maxDepth, double chanceOfLiteral,  Random randomNumberGenerator) {
		myAssert(maxDepth >= 0, ()->"ERROR: maxDepth cannot be negative!!!");
		myAssert(chanceOfLiteral < 1.0 && chanceOfLiteral >= 0.0, ()->"ERROR: chanceOfLiteral must be in the interval [0,1)!!!");
		this.context = context;
		this.arithmeticSymbols = extractSymbolsWithArithmeticTypes();
		this.maxDepth = maxDepth;
		this.chanceOfLiteral = chanceOfLiteral;
		this.randomNumberGenerator = randomNumberGenerator;
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
		Expression result = apply(maxDepth);
		return result;
	}
	
	private Expression apply(int depth) {
		Expression result;
		if (depth == 0) {
			result = createArithmeticLeaf();
		}
		else {
			int periodForGettingLiteral = Math.toIntExact(Math.round(1/chanceOfLiteral));
			if(randomNumberGenerator.nextInt(periodForGettingLiteral)==0) {
				Expression literal = theoryTestingSupport.makeRandomLiteral(context);
				literal = makeCopyOfExpressionAlteringAnyConstantsSoThatTheyAreWithinPredeterminedBounds(literal,context,randomNumberGenerator);
				result = IfThenElse.make(literal, apply(depth - 1), apply(depth - 1));		
			}
			else {
				int periodForEarlyLeafGeneration = Math.toIntExact(Math.round(1/chanceOfEarlyLeafGenerationInsteadOfFunctionApplication));
				if(randomNumberGenerator.nextInt(periodForEarlyLeafGeneration)==0) {
					result = createArithmeticLeaf();
				}
				else {
					int functorIdx = randomNumberGenerator.nextInt(DIFFERENCE_ARITHMETIC_FUNCTORS.size());
					Expression functor = DIFFERENCE_ARITHMETIC_FUNCTORS.get(functorIdx);
					ArrayList<Expression> functionArguments = new ArrayList<Expression>(2);
					functionArguments.add(apply(depth-1));
					if(functor == DIVISION_FUNCTOR) {
						functionArguments.add(createNonZeroArithmeticConstant());	
					}
					else {
						functionArguments.add(apply(depth-1));						
					}
					result = new DefaultFunctionApplication(functor, functionArguments);
				}
			}
		}
		return result;
	}

	private Expression createArithmeticLeaf() {
		Expression result;
		if(randomNumberGenerator.nextInt(2)==0) {
			result = createArithmeticConstant();
		}
		else {
			result = sampleArithmeticVariable();
		}
		return result;
	}

	public Expression createArithmeticConstant() {
		Expression result;
		int modulusValue = (int) Math.pow(10, maxNumDigitsForConstantsInRandomlyCreatedLiterals);
		BigIntegerNumberExact num = new BigIntegerNumberExact( Math.abs(randomNumberGenerator.nextInt() % (modulusValue)) ); 
		BigIntegerNumberExact den = new BigIntegerNumberExact( 1 + Math.abs(randomNumberGenerator.nextInt() % (modulusValue-1)) ); 
		Rational r = new Rational(num,den);
		result = makeSymbol(r);
		return result;
	}
	
	public Expression createNonZeroArithmeticConstant() {
		Expression result;
		int modulusValue = (int) Math.pow(10, maxNumDigitsForConstantsInRandomlyCreatedLiterals);
		BigIntegerNumberExact num = new BigIntegerNumberExact( 1 + Math.abs(randomNumberGenerator.nextInt() % (modulusValue-1)) ); 
		BigIntegerNumberExact den = new BigIntegerNumberExact( 1 + Math.abs(randomNumberGenerator.nextInt() % (modulusValue-1)) ); 
		Rational r = new Rational(num,den);
		result = makeSymbol(r);
		return result;
	}
	
	public Expression sampleArithmeticVariable() {
		Expression result;
		int symbolIdx = randomNumberGenerator.nextInt(arithmeticSymbols.size());
		result = arithmeticSymbols.get(symbolIdx);
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
			if(type == GrinderUtil.REAL_TYPE || type instanceof RealInterval) {
				//continue;
				//TODO: once literals with mix of integers and real types can be handled, uncomment below...
				type = GrinderUtil.INTEGER_TYPE;
			}
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
	
	public static Expression makeCopyOfExpressionAlteringAnyConstantsSoThatTheyAreWithinPredeterminedBounds(Expression literal, Context context, Random rand) {
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
				literal = literal.replaceAllOccurrences(argument, makeCopyOfExpressionAlteringAnyConstantsSoThatTheyAreWithinPredeterminedBounds(argument, context, rand), context);
			}
		}
		return literal;
	}

}
