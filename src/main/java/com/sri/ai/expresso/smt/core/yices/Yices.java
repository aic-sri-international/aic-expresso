package com.sri.ai.expresso.smt.core.yices;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;

import java.math.BigInteger;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.ToIntFunction;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.smt.api.SMTSolver;
import com.sri.ai.expresso.smt.api.SMTBasedContext;
import com.sri.ai.expresso.smt.api.SMTExpression;
import com.sri.ai.expresso.smt.api.SMTModel;
import com.sri.ai.expresso.smt.api.SMTType;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.util.math.Rational;
import com.sri.yices.BigRational;
import com.sri.yices.Config;
import com.sri.yices.Model;
import com.sri.yices.Status;
import com.sri.yices.Terms;
import com.sri.yices.Types;
import com.sri.yices.YicesException;

public class Yices implements SMTSolver {
	
	// STATIC INSTANTIATION OF CLASS INSTANCE
	//////////////////////////////////////////////////////	
	public static final Yices YICES_SMT_SOLVER = new Yices();
	
	
	
	// CLASSES CORRESPONDING TO YICES
	//////////////////////////////////////////////////////	
	private static final Class<? extends SMTBasedContext> EXPRESSO_SMT_CONTEXT_CLASS = YicesBasedContext.class;
	private static final Class<? extends SMTExpression> EXPRESSO_SMT_EXPRESSION_CLASS = YicesExpression.class;
	private static final  Class<? extends SMTModel> EXPRESSO_SMT_MODEL_CLASS = YicesModel.class;
	private static final  Class<? extends SMTType> EXPRESSO_SMT_TYPE_CLASS = YicesType.class;
	
	@Override
	public Class<? extends SMTBasedContext> getExpressoSMTContextClass(){
		return EXPRESSO_SMT_CONTEXT_CLASS;
	}
	@Override
	public Class<? extends SMTExpression> getExpressoSMTExpressionClass() {
		return EXPRESSO_SMT_EXPRESSION_CLASS;
	}
	@Override
	public  Class<? extends SMTModel> getExpressoSMTModelClass() {
		return EXPRESSO_SMT_MODEL_CLASS;
	}
	@Override
	public  Class<? extends SMTType> getExpressoSMTTypeClass() {
		return EXPRESSO_SMT_TYPE_CLASS;
	}
	
	

	
//////////////// YICES EXPRESSION  ///////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	// STATIC DATA FIELDS
	//////////////////////////////////////////////////////
	private static final Map< Class<? extends Type>, ToIntFunction<ExpressoToYicesSymbolRegistrationInformation> > 
	EXPRESSO_SYMBOL_TYPE_TO_YICES_SYMBOL_REGISTRATION_METHODS = 
			new LinkedHashMap<>() {
				private static final long serialVersionUID = 1L;
			{
        put(Categorical.class, (symbolInfo)->registerCategoricalYicesSymbol(symbolInfo));
        put(FunctionType.class, (symbolInfo)->registerFunctionalYicesSymbol(symbolInfo));
        put(IntegerExpressoType.class, (symbolInfo)->registerIntegerYicesSymbol(symbolInfo));
        put(IntegerInterval.class, (symbolInfo)->registerIntegerIntervalYicesSymbol(symbolInfo));
        put(RealExpressoType.class, (symbolInfo)->registerRealYicesSymbol(symbolInfo));
        put(RealInterval.class, (symbolInfo)->registerRealIntervalYicesSymbol(symbolInfo));
        put(TupleType.class, (symbolInfo)->registerTupleYicesSymbol(symbolInfo));
    }};	
    
	//TODO create objects in another, more general, class such as in FunctorConstants
	private static final Expression EQUALITY_EXPRESSION_FUNCTOR  = Expressions.makeSymbol("=");
	private static final Expression DISEQUALITY_EXPRESSION_FUNCTOR  = Expressions.makeSymbol("!=");
	private static final Expression GREATER_THAN_EXPRESSION_FUNCTOR = Expressions.makeSymbol(">");
	private static final Expression LESS_THAN_EXPRESSION_FUNCTOR = Expressions.makeSymbol("<");
	private static final Expression LESS_THAN_OR_EQUAL_TO_EXPRESSION_FUNCTOR = Expressions.makeSymbol("<=");
	private static final Expression GREATER_THAN_OR_EQUAL_TO_EXPRESSION_FUNCTOR = Expressions.makeSymbol(">=");
	private static final Expression NOT_EXPRESSION_FUNCTOR = Expressions.makeSymbol("not");
	
	private static final Expression PLUS_EXPRESSION_FUNCTOR  = Expressions.makeSymbol("+");
	private static final Expression MINUS_EXPRESSION_FUNCTOR  = Expressions.makeSymbol("-");
	private static final Expression TIMES_EXPRESSION_FUNCTOR = Expressions.makeSymbol("*");
	private static final Expression DIVISION_EXPRESSION_FUNCTOR = Expressions.makeSymbol("/");
	private static final Expression EXPONENTIATION_EXPRESSION_FUNCTOR = Expressions.makeSymbol("^");
	
	private static final Map< Expression, ToIntFunction<Integer[]> > EXPRESSO_FUNCTOR_TO_YICES_FUNCTION_APPLICATION =
	new LinkedHashMap<>() {
		private static final long serialVersionUID = 1L;
	{
	put(EQUALITY_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesEqualityExpression(functionArguments));
	put(DISEQUALITY_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesDisequalityExpression(functionArguments));
	put(GREATER_THAN_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesGreaterThanExpression(functionArguments));
	put(LESS_THAN_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesLessThanExpression(functionArguments));
	put(LESS_THAN_OR_EQUAL_TO_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesLessThanOrEqualToExpression(functionArguments));
	put(GREATER_THAN_OR_EQUAL_TO_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesGreaterThanOrEqualToExpression(functionArguments));
	put(NOT_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesNotExpression(functionArguments));
	
	put(PLUS_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesPlusExpression(functionArguments));
	put(MINUS_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesMinusExpression(functionArguments));
	put(TIMES_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesTimesExpression(functionArguments));
	put(DIVISION_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesDivisionExpression(functionArguments));
	put(EXPONENTIATION_EXPRESSION_FUNCTOR, (functionArguments)->_makeYicesExponentiationExpression(functionArguments));
	}};

	
	
	// INNER CLASSES
	//////////////////////////////////////////////////////	
    private static class ExpressoToYicesSymbolRegistrationInformation {

		private final String name;
		private final Expression expression;
    	private final Type type;
    	private final SMTBasedContext smtContext;
    	
    	ExpressoToYicesSymbolRegistrationInformation(String symbolName, Expression expression, Type type, SMTBasedContext smtContext){
    		this.name = symbolName;
    		this.expression = expression;
    		this.type = type;
    		this.smtContext = smtContext;
    	}
    	
    	String getName() {
			return name;
		}
    	Expression getExpression() {
    		return expression;
    	}
		Type getType() {
			return type;
		}
		SMTBasedContext getSMTContext() {
			return smtContext;
		}
    }
	
	
	
	// UTILITY METHODS SMTExpression
	//////////////////////////////////////////////////////
	@Override
	public Object makeSMTSolverExpressionObjectFromExpressionLiteral(Expression literal, SMTBasedContext smtContext) {
		int yicesFormula = makeYicesSolverExpressionObjectFromExpressoExpression(literal, smtContext);
		return yicesFormula;
	}

	@Override
	public String getExpressionString(SMTExpression smtFormula) {
		myAssert(smtFormula instanceof YicesExpression,
				() -> "ERROR: smtContext is of type " + smtFormula.getClass().getSimpleName() + " but expected type "
						+ YicesExpression.class.getSimpleName() + "!");
		String yicesFormulaString;
		Integer yicesFormulaReference = (Integer) smtFormula.getEmeddedSMTObject();
		yicesFormulaString = Terms.getName(yicesFormulaReference);
		return yicesFormulaString;
	}
	
	@Override
	public SMTType getExpressionType(SMTExpression smtFormula) {
		myAssert(smtFormula instanceof YicesExpression,
				() -> "ERROR: smtContext is of type " + smtFormula.getClass().getSimpleName() + " but expected type "
						+ YicesExpression.class.getSimpleName() + "!");
		int yicesSMTFormula = (int) smtFormula.getEmeddedSMTObject();
		int type = Terms.typeOf(yicesSMTFormula);
		SMTType smtType = new YicesType(type);
		return smtType;
	}
	
	@Override
	public String getExpressionTypeSimpleName(SMTExpression smtFormula) {
		myAssert(smtFormula instanceof YicesExpression,
				() -> "ERROR: smtContext is of type " + smtFormula.getClass().getSimpleName() + " but expected type "
						+ YicesExpression.class.getSimpleName() + "!");
		int yicesSMTFormula = (int) smtFormula.getEmeddedSMTObject();
		int type = Terms.typeOf(yicesSMTFormula);
		String typeName = Types.getName(type);
		return typeName;
	}
	
	@Override
	public String getExpressionTypeNativeName(SMTExpression smtFormula) {
		myAssert(smtFormula instanceof YicesExpression,
				() -> "ERROR: smtContext is of type " + smtFormula.getClass().getSimpleName() + " but expected type "
						+ YicesExpression.class.getSimpleName() + "!");
		int yicesSMTFormula = (int) smtFormula.getEmeddedSMTObject();
		int type = Terms.typeOf(yicesSMTFormula);
		String typeName = Types.toString(type);
		return typeName;
	}
	
	@Override
	public boolean expressionIsAssertible(SMTExpression smtFormula) {
		myAssert(smtFormula instanceof YicesExpression,
				() -> "ERROR: smtContext is of type " + smtFormula.getClass().getSimpleName() + " but expected type "
						+ YicesExpression.class.getSimpleName() + "!");
		int yicesSMTFormula = (int) smtFormula.getEmeddedSMTObject();
		boolean isAssertible = Terms.isBool(yicesSMTFormula);
		return isAssertible;
	}
	
	@Override
	public boolean isBooleanType(SMTExpression smtExpression) {
		myAssert(smtExpression instanceof YicesExpression,
				() -> "ERROR: smtContext is of type " + smtExpression.getClass().getSimpleName() + " but expected type "
						+ YicesExpression.class.getSimpleName() + "!");
		int yicesExpression = (int) smtExpression.getEmeddedSMTObject();
		boolean isBool = Terms.isBool(yicesExpression);
		return isBool;
	}
	
	@Override
	public boolean isIntegerType(SMTExpression smtExpression) {
		myAssert(smtExpression instanceof YicesExpression,
				() -> "ERROR: smtContext is of type " + smtExpression.getClass().getSimpleName() + " but expected type "
						+ YicesExpression.class.getSimpleName() + "!");
		int yicesExpression = (int) smtExpression.getEmeddedSMTObject();
		boolean isInteger = Terms.isInteger(yicesExpression);
		return isInteger;
	}
	
	@Override
	public boolean isRealType(SMTExpression smtExpression) {
		myAssert(smtExpression instanceof YicesExpression,
				() -> "ERROR: smtContext is of type " + smtExpression.getClass().getSimpleName() + " but expected type "
						+ YicesExpression.class.getSimpleName() + "!");
		int yicesExpression = (int) smtExpression.getEmeddedSMTObject();
		boolean isReal = Terms.isReal(yicesExpression);
		return isReal;
	}



	// PRIVATE HELPER METHODS
	//////////////////////////////////////////////////////
	private static int makeYicesSolverExpressionObjectFromExpressoExpression(Expression expression, SMTBasedContext smtContext) {
		int yicesTerm = -1;
		Expression functor = expression.getFunctor();
		if (functor == null) {
			yicesTerm = makeYicesSymbolFromExpressoSymbol(expression, smtContext);
		} 
		else { 
			List<Expression> functionArguments = expression.getArguments();
			Integer[] smtSymbols = makeYicesTerms(functionArguments, smtContext);
			yicesTerm = applyYicesFunctionApplication(functor, smtSymbols);
		}
		return yicesTerm;
	}
	
	private static int makeYicesSymbolFromExpressoSymbol(Expression symbol, SMTBasedContext smtContext) {
		int yicesTermReference;
		String symbolName = symbol.toString();
		yicesTermReference = Terms.getByName(symbolName);
		//TODO currently assumes if a reference is already present, the term was registered correctly
		if(yicesTermIsRegistered(yicesTermReference)) { //term is already registered with Yices
			; // reference to yices term already properly assigned
		}
		else { //term is not already registered with Yices
			Rational symbolRationalValue = symbol.rationalValue();
			if(isValidRationalValue(symbolRationalValue)) { //symbol is a number constant
				BigRational symbolBigRationalValue = new BigRational(symbolRationalValue.toString());
				yicesTermReference = Terms.rationalConst(symbolBigRationalValue);
			}
			else {
				Type symbolType = GrinderUtil.getTypeOfExpression(symbol, smtContext);
				ExpressoToYicesSymbolRegistrationInformation symbolInfo = 
						new ExpressoToYicesSymbolRegistrationInformation(symbolName, symbol, symbolType, smtContext);
				yicesTermReference = registerSymbolWithYices(symbolType, symbolInfo);
			}
		}
		return yicesTermReference;
	}
	
	private static boolean yicesTermIsRegistered(Object smtTerm) {
		myAssert(smtTerm instanceof Integer,
				() -> "ERROR: Attempting to check Yices registration of " + smtTerm.getClass().getSimpleName() + "!");
		int yicesReference = (int) smtTerm;
		boolean symbolIsRegistered = (yicesReference >= 0);
		return symbolIsRegistered;
	}
	
	private static Integer[] makeYicesTerms(List<Expression> expressionTerms, SMTBasedContext smtContext) {
		Integer[] yicesTerms = new Integer[expressionTerms.size()];
		int i = 0;
		for(Expression expression : expressionTerms) {
			yicesTerms[i] = makeYicesSolverExpressionObjectFromExpressoExpression(expression, smtContext);
			++i;
		}
		return yicesTerms;
	}
	
	private static int applyYicesFunctionApplication(Expression functor, Integer[] functionArguments) {
		ToIntFunction<Integer[]> correspondingYicesFunctor = EXPRESSO_FUNCTOR_TO_YICES_FUNCTION_APPLICATION
				.get(functor);
		int yicesLiteral = correspondingYicesFunctor.applyAsInt(functionArguments);
		return yicesLiteral;
	}

	private static int _makeYicesEqualityExpression(Integer[] functionArguments) {
		int arg1 = functionArguments[0];
		int arg2 = functionArguments[1];
		int yicesEqualityFormula = Terms.eq(arg1, arg2);
		return yicesEqualityFormula;
	}

	private static int _makeYicesDisequalityExpression(Integer[] functionArguments) {
		int arg1 = functionArguments[0];
		int arg2 = functionArguments[1];
		int yicesDisequalityFormula = Terms.neq(arg1, arg2);
		return yicesDisequalityFormula;
	}

	private static int _makeYicesGreaterThanExpression(Integer[] functionArguments) {
		int arg1 = functionArguments[0];
		int arg2 = functionArguments[1];
		int yicesGreaterThanFormula = Terms.arithGt(arg1, arg2);
		return yicesGreaterThanFormula;
	}

	private static int _makeYicesLessThanExpression(Integer[] functionArguments) {
		int arg1 = functionArguments[0];
		int arg2 = functionArguments[1];
		int yicesLessThanFormula = Terms.arithLt(arg1, arg2);
		return yicesLessThanFormula;
	}

	private static int _makeYicesLessThanOrEqualToExpression(Integer[] functionArguments) {
		int arg1 = functionArguments[0];
		int arg2 = functionArguments[1];
		int yicesLessThanOrEqualToFormula = Terms.arithLeq(arg1, arg2);
		return yicesLessThanOrEqualToFormula;
	}

	private static int _makeYicesGreaterThanOrEqualToExpression(Integer[] functionArguments) {
		int arg1 = functionArguments[0];
		int arg2 = functionArguments[1];
		int yicesGreaterThanOrEqualToFormula = Terms.arithGeq(arg1, arg2);
		return yicesGreaterThanOrEqualToFormula;
	}

	private static int _makeYicesNotExpression(Integer[] functionArguments) {
		int arg = functionArguments[0];
		int yicesNotFormula = Terms.not(arg);
		return yicesNotFormula;
	}

	private static int _makeYicesPlusExpression(Integer[] functionArguments) {
		int[] args = new int[functionArguments.length];
		int i = 0;
		for(Integer arg : functionArguments) {
			args[i] = arg;
			++i;
		}
		int yicesPlusTerm = Terms.add(args);
		return yicesPlusTerm;
	}

	private static int _makeYicesMinusExpression(Integer[] functionArguments) {
		int yicesMinusTerm;
		if(functionArguments.length == 1) {
			int arg1 = functionArguments[0];
			yicesMinusTerm = Terms.neg(arg1);
		}
		else {
			int arg1 = functionArguments[0];
			int arg2 = functionArguments[1];
			yicesMinusTerm = Terms.sub(arg1, arg2);
		}
		return yicesMinusTerm;
	}

	private static int _makeYicesTimesExpression(Integer[] functionArguments) {
		int[] args = new int[functionArguments.length];
		int i = 0;
		for(Integer arg : functionArguments) {
			args[i] = arg;
			++i;
		}
		int yicesTimesTerm = Terms.mul(args);
		return yicesTimesTerm;
	}

	private static int _makeYicesDivisionExpression(Integer[] functionArguments) {
		int arg1 = functionArguments[0];
		int arg2 = functionArguments[1];
		int yicesDivisionTerm = Terms.div(arg1, arg2);
		return yicesDivisionTerm;
	}

	private static int _makeYicesExponentiationExpression(Integer[] functionArguments) {
		int arg1 = functionArguments[0];
		int arg2 = functionArguments[1];
		int yicesExponentiationTerm = -1;
		try {
			yicesExponentiationTerm = Terms.power(arg1, arg2);
		} 
		catch (Exception e) {
			yicesExponentiationTerm = -1;
			println(e);
			println("||| ERROR: Could not construct Yices Power Term with arguments " + arg1 + " and " + arg2 + "!|||");
		}
		return yicesExponentiationTerm;
	}
	
	
	
	private static boolean isValidRationalValue(Rational rationalValue) {
		boolean isValidRationalValue = (rationalValue != null);
		return isValidRationalValue;
	}
	
	private static int registerSymbolWithYices(Type expressoTypeOfSymbol, ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		ToIntFunction<ExpressoToYicesSymbolRegistrationInformation> appropriateFunctionToRegisterSymbolBasedOnTypeClass = 
				EXPRESSO_SYMBOL_TYPE_TO_YICES_SYMBOL_REGISTRATION_METHODS.get(expressoTypeOfSymbol.getClass());
		int registeredYicesSymbolReference = appropriateFunctionToRegisterSymbolBasedOnTypeClass.applyAsInt(symbolInfo);
		return registeredYicesSymbolReference;
	}
	
	private static boolean typeIsRegistered(int yicesTypeRegistration) {
		boolean isRegisteredType = (yicesTypeRegistration >= 0);
		return isRegisteredType;
	}
	
	
	private static int registerCategoricalYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		int yicesSymbolRegistration = -1;
		String symbolName = symbolInfo.getName();
		Categorical symbolType = (Categorical) symbolInfo.getType();
		
		if(symbolType == GrinderUtil.BOOLEAN_TYPE) {
			//TODO confirm there is no way True or False can be obtained as the symbol (ie. no boolean literal has "True" or "False" in its expression)
			try {
				yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolName, Types.BOOL);
			} 
			catch (Exception e) {
				yicesSymbolRegistration = -1;
				println(e);
				println("||| ERROR: The Boolean symbol '" + symbolName + "' was not registered with Yices!!! |||");
			}
		}
		else {
			String symbolTypeName = symbolType.getName();
			Expression symbolExpression = symbolInfo.getExpression();
			int yicesTypeRegistration = Types.getByName(symbolTypeName);
			
			//registering Type with Yices
			if(typeIsRegistered(yicesTypeRegistration)) {
				; //no need to register a new Scalar type with Yices
			}
			else {
				//TODO possibly have method in Categorical that returns known constants and register them all with Yices here (makes them immediately recognizable as they show up later, and removes the need to look each one up individually)
				int typeCardinality = symbolType.cardinality().intValue();
		    	try {
					yicesTypeRegistration = Types.newScalarType(symbolTypeName, typeCardinality);
				} catch (Exception e) {
					yicesTypeRegistration = -1;
					println(e);
					println("||| ERROR: The type '" + symbolTypeName + "' was not registered with Yices!!! |||");
				}
			}
			
			if(symbolType.contains(symbolExpression)) { // symbol is a categorical constant
				int indexOfKnownCategoricalConstant = symbolType.indexOfConstant(symbolExpression);
				yicesSymbolRegistration = Terms.mkConst(yicesTypeRegistration, indexOfKnownCategoricalConstant);
				Terms.setName(yicesSymbolRegistration, symbolName);
			}
			else {
				try {
					yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolName, yicesTypeRegistration);
				} catch (Exception e) {
					yicesSymbolRegistration = -1;
					println(e);
					println("||| ERROR: The Categorical symbol '" + symbolName + "' was not registered with Yices!!! |||");
				}
			}
		}
		return yicesSymbolRegistration;
	}
	
	private static int registerFunctionalYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		// TODO Auto-generated method stub
		int yicesSymbolRegistration = -1;
		return yicesSymbolRegistration;
	}
	
	private static int registerIntegerYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		int yicesSymbolRegistration = -1;	
    	try {
			yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolInfo.getName(), Types.INT);
		} catch (Exception e) {
			yicesSymbolRegistration = -1;
			println(e);
			println("||| ERROR: The Integer symbol '" + symbolInfo.getName() + "' was not registered with Yices!!! |||");
		}	
		return yicesSymbolRegistration;
	}
	
	private static int registerIntegerIntervalYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		//TODO we might need to handle parameterized bounds (ie. parameterized by another variable)
		//TODO handle case for infinite bounds
		int yicesSymbolRegistration = -1;	
    	try {
			yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolInfo.getName(), Types.INT);
			enforceIntegerIntervalBounds(yicesSymbolRegistration, symbolInfo);
		} catch (Exception e) {
			yicesSymbolRegistration = -1;
			println(e);
			println("||| ERROR: The Integer Interval symbol '" + symbolInfo.getName() + "' was not registered with Yices!!! |||");
		}	
		return yicesSymbolRegistration;
	}
	
	private static int registerRealYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		int yicesSymbolRegistration = -1;	
    	try {
			yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolInfo.getName(), Types.REAL);
		} catch (Exception e) {
			yicesSymbolRegistration = -1;
			println(e);
			println("||| ERROR: The Real symbol '" + symbolInfo.getName() + "' was not registered with Yices!!! |||");
		}	
		return yicesSymbolRegistration;
	}
	
	private static int registerRealIntervalYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		//TODO we might need to handle parameterized bounds (ie. parameterized by another variable)
		//TODO handle case for infinite bounds
		int yicesSymbolRegistration = -1;	
    	try {
			yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolInfo.getName(), Types.REAL);
			enforceRealIntervalBounds(yicesSymbolRegistration, symbolInfo);
		} catch (Exception e) {
			yicesSymbolRegistration = -1;
			println(e);
			println("||| ERROR: The Real Interval symbol '" + symbolInfo.getName() + "' was not registered with Yices!!! |||");
		}	
		return yicesSymbolRegistration;
	}

	private static int registerTupleYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		// TODO Auto-generated method stub
		int yicesSymbolRegistration = -1;
		return yicesSymbolRegistration;
	}
	
	private static void enforceIntegerIntervalBounds(int yicesSymbolRegistration,
			ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		//TODO use BigIntegerNumber instead of BigInteger (currently Terms.intConst(BigInteger) is incompatible with BigIntegerNumber)
		//TODO more efficient conversion from Rational to BigInteger (ie. instead of passing in the string)?
		
		SMTBasedContext smtContext = symbolInfo.getSMTContext();
		IntegerInterval symbolIntegerIntervalType = (IntegerInterval) symbolInfo.getType();
		//boolean boundWasEnforced = false;

		if (symbolIntegerIntervalType.noLowerBound()) {
			; //no lower bound to assert onto smtContext
		}
		else {
			//Expression lowerBoundExpression = simplifyExpression(symbolIntegerIntervalType.getNonStrictLowerBound(), symbolInfo.getSMTContext());
			Expression lowerBoundExpression = symbolIntegerIntervalType.getNonStrictLowerBound();
			Rational rationalLowerBound = lowerBoundExpression.rationalValue();
			BigInteger bigIntegerLowerBound = new BigInteger(rationalLowerBound.toString());
			SMTExpression yicesLowerBoundTerm = new YicesExpression( Terms.arithGeq(yicesSymbolRegistration, Terms.intConst(bigIntegerLowerBound)) );
			smtContext.assertOnExistingStackFrame(yicesLowerBoundTerm);
			//boundWasEnforced = true;
		}
		
		if (symbolIntegerIntervalType.noUpperBound()) {
			; //no upper bound to assert onto smt Context
		}
		else {
			//Expression upperBoundExpression = simplifyExpression(symbolIntegerIntervalType.getNonStrictUpperBound(), symbolInfo.getSMTContext());
			Expression upperBoundExpression = symbolIntegerIntervalType.getNonStrictUpperBound();
			Rational rationalUpperBound = upperBoundExpression.rationalValue();
			BigInteger bigIntegerUpperBound = new BigInteger(rationalUpperBound.toString());
			SMTExpression yicesUpperBoundTerm = new YicesExpression( Terms.arithLeq(yicesSymbolRegistration, Terms.intConst(bigIntegerUpperBound)) );
			smtContext.assertOnExistingStackFrame(yicesUpperBoundTerm);
			//boundWasEnforced = true;
		}
//		if(boundWasEnforced) {
//			//smtContext.pushStackFrame();
//		}
	}
	
	private static void enforceRealIntervalBounds(int yicesSymbolRegistration,
			ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		//TODO use Rational instead of BigRational (currently Terms.rationalConst(BigRational) is incompatible with Rational)
		//TODO more efficient conversion from Rational to BigRational. (ex. using Terms.parseRational(), but that still parses a string...)
		
		SMTBasedContext smtContext = symbolInfo.getSMTContext();
		RealInterval symbolRealIntervalType = (RealInterval) symbolInfo.getType();
		//boolean boundWasEnforced = false;
		
		if(symbolRealIntervalType.noLowerBound()) {
			; //no lower bound to assert onto smtContext
		}
		else {
			//Expression lowerBoundExpression = simplifyExpression(symbolRealIntervalType.getLowerBound(), symbolInfo.getSMTContext());
			Expression lowerBoundExpression = symbolRealIntervalType.getLowerBound();
			Rational rationalLowerBound = lowerBoundExpression.rationalValue();
			BigRational bigRationalLowerBound = new BigRational(rationalLowerBound.toString());
			
			SMTExpression yicesLowerBoundTerm;
			if(symbolRealIntervalType.lowerBoundIsOpen()) {
				yicesLowerBoundTerm = new YicesExpression( Terms.arithGt(yicesSymbolRegistration, Terms.rationalConst(bigRationalLowerBound)) );
			}
			else {
				yicesLowerBoundTerm = new YicesExpression( Terms.arithGeq(yicesSymbolRegistration, Terms.rationalConst(bigRationalLowerBound)) );
			}
			smtContext.assertOnExistingStackFrame(yicesLowerBoundTerm);
			//boundWasEnforced = true;
		}
		if (symbolRealIntervalType.noUpperBound()){
			; //no upper bound to assert onto smt Context
		}
		else {
			//Expression upperBoundExpression = simplifyExpression(symbolRealIntervalType.getUpperBound(), symbolInfo.getSMTContext());
			Expression upperBoundExpression = symbolRealIntervalType.getUpperBound();
			Rational rationalUpperBound = upperBoundExpression.rationalValue();
			BigRational bigRationalUpperBound = new BigRational(rationalUpperBound.toString());
			
			SMTExpression yicesUpperBoundTerm;
			if(symbolRealIntervalType.upperBoundIsOpen()) {
				yicesUpperBoundTerm = new YicesExpression( Terms.arithLt(yicesSymbolRegistration, Terms.rationalConst(bigRationalUpperBound)) );
			}
			else {
				yicesUpperBoundTerm = new YicesExpression( Terms.arithLeq(yicesSymbolRegistration, Terms.rationalConst(bigRationalUpperBound)) );
			}
			smtContext.assertOnExistingStackFrame(yicesUpperBoundTerm);
			//boundWasEnforced = true;
		}
//		if(boundWasEnforced) {
//			//smtContext.pushStackFrame();
//		}
	}
	
	
	
	
	
	//////////////// YICES CONTEXT ///////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// UTILITY METHODS SMTBasedContext
	//////////////////////////////////////////////////////	
    @Override
    public Object makeSMTSolverContextObject() {
    	com.sri.yices.Context yicesContext = new com.sri.yices.Context();
    	yicesContext.push();
    	return yicesContext;
    }
	
	public com.sri.yices.Context makeYicesSolverContextObject(String contextConfiguration) {
		Config cfg = new Config();
        cfg.set("mode", contextConfiguration);
		com.sri.yices.Context yicesContext = new com.sri.yices.Context(cfg);
		return yicesContext;
	}

	@Override
	public Object assertOntoContext(SMTBasedContext smtContext, SMTExpression smtFormula) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		myAssert(smtFormula instanceof YicesExpression, () -> "ERROR: smtFormula is of type "
				+ smtFormula.getClass().getSimpleName() + " but expected type YicesExpression!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		Integer yicesFormula = (Integer) smtFormula.getEmeddedSMTObject();
		yicesContext.assertFormula(yicesFormula);
		return yicesContext;
	}

	@Override
	public Object assertOntoContext(SMTBasedContext smtContext, SMTExpression... smtFormulas) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		myAssert(smtFormulas instanceof SMTExpression[], () -> "ERROR: smtFormula is of type "
				+ smtFormulas.getClass().getSimpleName() + " but expected type " + YicesExpression.class.getSimpleName() + " array!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		int[] yicesFormulas = new int[smtFormulas.length];
		int i = 0;
		for(SMTExpression yicesFormula : smtFormulas) {
			yicesFormulas[i] = (int) yicesFormula.getEmeddedSMTObject();
			++i;
		}
		yicesContext.assertFormulas(yicesFormulas);
		return yicesContext;
	}
	
	@Override
	public Object assertOntoContext(SMTBasedContext smtContext, Expression formula) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		int yicesFormula = makeYicesSolverExpressionObjectFromExpressoExpression(formula, smtContext);
		yicesContext.assertFormula(yicesFormula);
		return yicesContext;
	}
	
	@Override
	public Object assertOntoContext(SMTBasedContext smtContext, Expression[] formulas) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		int[] yicesFormulas = new int[formulas.length];
		int i = 0;
		for(Expression formula : formulas) {
			yicesFormulas[i] = makeYicesSolverExpressionObjectFromExpressoExpression(formula, smtContext);
			++i;
		}
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		yicesContext.assertFormulas(yicesFormulas);
		return yicesContext;
		
	}
	
	@Override
	public Object pushStackFrame(SMTBasedContext smtContext) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		yicesContext.push();
		return yicesContext;
	}

	@Override
	public Object popStackFrame(SMTBasedContext smtContext) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		yicesContext.pop();
		return yicesContext;
	}

	@Override
	public boolean contextIsSatisfiable(SMTBasedContext smtContext) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		boolean contextIsSatisfiable = (yicesContext.check() == Status.SAT);
		return contextIsSatisfiable;
	}
	
	@Override
	public boolean contextIsSatisfiable(SMTBasedContext smtContext, SMTExpression smtFormula) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		Integer yicesFormula = (Integer) smtFormula.getEmeddedSMTObject();
		yicesContext.push();
		yicesContext.assertFormula(yicesFormula);
		boolean contextIsSatisfiable = (yicesContext.check() == Status.SAT);
		yicesContext.pop();
		return contextIsSatisfiable;
	}
	
	@Override
	public boolean contextIsSatisfiable(SMTBasedContext smtContext, Expression formula) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		int yicesFormula = makeYicesSolverExpressionObjectFromExpressoExpression(formula, smtContext);
		yicesContext.push();
		yicesContext.assertFormula(yicesFormula);
		boolean contextIsSatisfiable = (yicesContext.check() == Status.SAT);
		yicesContext.pop();
		return contextIsSatisfiable;
	}
	
	@Override
	public String getModelAsString(SMTBasedContext smtContext) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		yicesContext.check();
		Model model = getYicesModelIfContextIsSatisfiable(yicesContext);
		String modelAsString = formatModelString(model);
		return modelAsString;
	}
	
	@Override
	public SMTModel getModel(SMTBasedContext smtContext) {
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		yicesContext.check();
		Model model = getYicesModelIfContextIsSatisfiable(yicesContext);
		SMTModel expressoYicesModel = new YicesModel(model);
		return expressoYicesModel;
	}
	


	// PRIVATE HELPER METHODS
	//////////////////////////////////////////////////////
	private static Model getYicesModelIfContextIsSatisfiable(com.sri.yices.Context yicesContext) {
		Model model;
		try{
			model = yicesContext.getModel();
		}
		catch (YicesException e) {
			model = null;
		}
		return model;
	}
	
	private static String formatModelString(Model model) {
		String modelAsString;
		if(model != null) {
			modelAsString = model.toString().replace("\n", " ").replace("\r", "");
		}
		else {
			modelAsString = "Unable to extract model given the smt context state!!!";
		}
		return modelAsString;
	}

	
	
	

	
	//////////////// YICES MODEL /////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	

	// UTILITY METHODS: SMTModel
	//////////////////////////////////////////////////////
	@Override
	public Expression getValueOfVariable(Expression expression, SMTModel smtModel, SMTBasedContext smtContext) {
		myAssert(smtModel instanceof YicesModel, () -> "ERROR: Expected a " + YicesModel.class.getSimpleName()
				+ " object, but received a " + smtModel.getClass().getSimpleName() + " object instead!");
		myAssert(smtContext instanceof YicesBasedContext,
				() -> "ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " but expected type "
						+ YicesBasedContext.class.getSimpleName() + "!");

		int yicesExpression = makeYicesSolverExpressionObjectFromExpressoExpression(expression, smtContext);
		Model model = (Model) smtModel.getEmbeddedSMTSolverObject();
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext.getEmbeddedSMTContext();
		
		Object variableValue;
		int yicesVariableValue;
		if(Terms.isBool(yicesExpression)) {
			boolean b = model.boolValue(yicesExpression);
			variableValue = Boolean.valueOf(b);
			yicesVariableValue = Terms.mkBoolConst(b);
		}
		else if(Terms.isInteger(yicesExpression)) {
			BigInteger i = model.bigIntegerValue(yicesExpression);
			variableValue = i;
			yicesVariableValue = Terms.intConst(i);
		}
		else if(Terms.isReal(yicesExpression)) {
			BigRational r = model.bigRationalValue(yicesExpression);
			variableValue = r;
			yicesVariableValue = Terms.rationalConst(r);
		}
		else {
			//TODO handle other types such as categorical types
			variableValue = null;
			yicesVariableValue = -1;
			throw new Error("cannot get value of variables of this type");
		}

		int yicesNotEqualExpression = Terms.neq(yicesExpression, yicesVariableValue);
		yicesContext.push();
		yicesContext.assertFormula(yicesNotEqualExpression);
		Expression result = null;
		if(yicesContext.check() == Status.UNSAT) {
//			result = Expressions.makeSymbol(variableValue); // doesn't work well because it'll make a symbol with
//															// BigRational and BigInteger which will not evaluate equal
//															// to another symbol of the same value made by a Rational or
//															// TopRewriting/Simplifying (which likely also creates the
//															// symbol with a Rational)
			result = parse(variableValue.toString());
			if(variableValue instanceof BigRational) {
				result = smtContext.getTheory().getTopRewriter().apply(result, smtContext);
			}
		}
		yicesContext.pop();
		return result;
	}
	
	///////////////////////////////////////////////////////////////////////////////////

}
