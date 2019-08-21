package com.sri.ai.expresso.smt.core.yices;

import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;

import java.math.BigInteger;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.ToIntFunction;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.smt.api.SMTSolver;
import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.expresso.smt.api.SMTFormula;
import com.sri.ai.expresso.smt.api.SMTTerm;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.api.Context;
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
	public static final Yices EXPRESSO_TO_YICES_UTIL = new Yices();
	
	
	
	// CLASSES CORRESPONDING TO YICES
	//////////////////////////////////////////////////////	
	
	private static final Class<com.sri.yices.Context> SMT_SOLVER_SMT_CONTEXT_CLASS = com.sri.yices.Context.class;
	private static final Class<Integer> SMT_SOLVER_SMT_TERM_CLASS = Integer.class;
	private static final  Class<Integer> SMT_SOLVER_SMT_FORMULA_CLASS = Integer.class;
	
	@Override
	public Class<? extends Object> getSMTSolverContextClass() {
		return SMT_SOLVER_SMT_CONTEXT_CLASS;
	}
	@Override
	public Class<? extends Object> getSMTSolverTermClass() {
		return SMT_SOLVER_SMT_TERM_CLASS;
	}
	@Override
	public Class<? extends Object> getSMTSolverFormulaClass() {
		return SMT_SOLVER_SMT_FORMULA_CLASS;
	}
	
	
	private static final Class<? extends SMTContext> EXPRESSO_SMT_CONTEXT_CLASS_BRANCH = YicesContext.class;
	private static final Class<? extends SMTTerm> EXPRESSO_SMT_TERM_CLASS_BRANCH = YicesTerm.class;
	private static final  Class<? extends SMTFormula> EXPRESSO_SMT_FORMULA_CLASS_BRANCH = YicesFormula.class;
	
	@Override
	public Class<? extends SMTContext> getCorrespondingExpressoSMTContextClassBranch(){
		return EXPRESSO_SMT_CONTEXT_CLASS_BRANCH;
	}
	@Override
	public Class<? extends SMTTerm> getCorrespondingExpressoSMTTermClassBranch() {
		return EXPRESSO_SMT_TERM_CLASS_BRANCH;
	}
	@Override
	public  Class<? extends SMTFormula> getCorrespondingExpressoSMTFormulaClassBranch() {
		return EXPRESSO_SMT_FORMULA_CLASS_BRANCH;
	}
	
	
	
	
	
	//////////////// YICES FORMULA  //////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// STATIC DATA FIELDS
	//////////////////////////////////////////////////////
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
	
	private static final Map< Expression, ToIntFunction<YicesTerm[]> > EXPRESSO_FUNCTOR_TO_YICES_FUNCTION_APPLICATION =
			new LinkedHashMap<>() {
				private static final long serialVersionUID = 1L;
			{
        put(EQUALITY_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesEqualityFormula(functionArguments));
        put(DISEQUALITY_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesDisequalityFormula(functionArguments));
        put(GREATER_THAN_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesGreaterThanFormula(functionArguments));
        put(LESS_THAN_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesLessThanFormula(functionArguments));
        put(LESS_THAN_OR_EQUAL_TO_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesLessThanOrEqualToFormula(functionArguments));
        put(GREATER_THAN_OR_EQUAL_TO_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesGreaterThanOrEqualToFormula(functionArguments));
        put(NOT_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesNotFormula(functionArguments));
        
        put(PLUS_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesPlusTerm(functionArguments));
        put(MINUS_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesMinusTerm(functionArguments));
        put(TIMES_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesTimesTerm(functionArguments));
        put(DIVISION_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesDivisionTerm(functionArguments));
        put(EXPONENTIATION_EXPRESSION_FUNCTOR, (functionArguments)->makeYicesExponentiationTerm(functionArguments));
    }};
	
    
	
	// FORMULA MAKERS
	//////////////////////////////////////////////////////
    @Override
    public Object makeSMTSolverFormulaObjectFromExpressionLiteral(Expression literal, Context context, SMTContext smtContext) {
    	int yicesFormula = makeYicesSolverFormulaObjectFromExpressoExpressionLiteral(literal, context, smtContext);
    	return yicesFormula;
    }
	
	@Override
	public SMTFormula makeExpressoSMTFormulaFromExpressionLiteral(Expression literal, Context context, SMTContext smtContext) {
		int yicesFormula = makeYicesSolverFormulaObjectFromExpressoExpressionLiteral(literal, context, smtContext);
		SMTFormula expressoYicesFormula = new YicesFormula(yicesFormula);
		return expressoYicesFormula;
	}
    
    ///////////////////////////////////////////////////////
    
	public static int makeYicesSolverFormulaObjectFromExpressoExpressionLiteral(Expression literal, Context context, SMTContext smtContext) {
		//myAssert(context.isLiteral(literal), ()->"ERROR: The Expression (" + literal.toString() + ") is not an Expresso literal!");
		myAssert(EXPRESSO_SMT_CONTEXT_CLASS_BRANCH.isAssignableFrom(smtContext.getClass()), ()->"ERROR: Attempting to construct Yices formula with " + smtContext.getClass().getSimpleName() + "!");
		int yicesFormula = -1;
		Expression functor = literal.getFunctor();
		if (functor != null){
			List<Expression> functionArguments = literal.getArguments();
			YicesTerm[] smtSymbols = makeCorrespondingYicesTerms(functionArguments, context, smtContext);
			yicesFormula = applyYicesFunctionApplication(functor, smtSymbols);
		}
		else { //(functor == null, implying expression is a Boolean symbol)
			//TODO remove assertion
			myAssert(context.getTypeOfRegisteredSymbol(literal) == GrinderUtil.BOOLEAN_TYPE, ()->"ERROR: Attempting to create a Yices Boolean symbol from" + literal.toString() + ", which is not an Expresso Boolean type!");
			YicesTerm yicesSymbol = new YicesTerm(literal, context, smtContext);
			yicesFormula = (int) yicesSymbol.getEmeddedReference();
		}
		return yicesFormula;
	}
	
	
	
	// UTILITY METHODS
	//////////////////////////////////////////////////////
    
	@Override
	public String getFormulaString(Object smtFormula) {
		String yicesFormulaString = getCorrespondingFormulaString(smtFormula);
		return yicesFormulaString;
	}
	
    ///////////////////////////////////////////////////////
	
	public static String getCorrespondingFormulaString(Object smtFormula) {
		myAssert(SMT_SOLVER_SMT_FORMULA_CLASS.isAssignableFrom(smtFormula.getClass()), ()->"ERROR: smtFormula is of type " + smtFormula.getClass().getSimpleName() + " which is not an appropriate Yices term reference!");
		Integer yicesFormulaReference = (Integer) smtFormula;
		String yicesFormulaString = Terms.getName(yicesFormulaReference);
		return yicesFormulaString;
	}

	
	
	// PRIVATE HELPER METHODS
	//////////////////////////////////////////////////////	
	private static int applyYicesFunctionApplication(Expression functor, YicesTerm[] functionArguments) {
		ToIntFunction<YicesTerm[]> correspondingYicesFunctor = 
				EXPRESSO_FUNCTOR_TO_YICES_FUNCTION_APPLICATION.get(functor);
		int yicesLiteral = correspondingYicesFunctor.applyAsInt(functionArguments);
		return yicesLiteral;
	}

	private static int makeYicesEqualityFormula(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesEqualityFormula = Terms.eq(arg1, arg2);
		return yicesEqualityFormula;
	}
	
	private static int makeYicesDisequalityFormula(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesDisequalityFormula = Terms.neq(arg1, arg2);
		return yicesDisequalityFormula;
	}
	
	private static int makeYicesGreaterThanFormula(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesGreaterThanFormula = Terms.arithGt(arg1, arg2);
		return yicesGreaterThanFormula;
	}
	
	private static int makeYicesLessThanFormula(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesLessThanFormula = Terms.arithLt(arg1, arg2);
		return yicesLessThanFormula;
	}
	
	private static int makeYicesLessThanOrEqualToFormula(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesLessThanOrEqualToFormula = Terms.arithLeq(arg1, arg2);
		return yicesLessThanOrEqualToFormula;
	}
	
	private static int makeYicesGreaterThanOrEqualToFormula(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesGreaterThanOrEqualToFormula = Terms.arithGeq(arg1, arg2);
		return yicesGreaterThanOrEqualToFormula;
	}
	
	private static int makeYicesNotFormula(YicesTerm[] functionArguments) {
		int arg = (int) functionArguments[0].getEmeddedReference();
		int yicesNotFormula = Terms.not(arg);
		return yicesNotFormula;
	}
	
	private static int makeYicesPlusTerm(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesPlusTerm = Terms.add(arg1, arg2);
		return yicesPlusTerm;
	}
	
	private static int makeYicesMinusTerm(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesMinusTerm = Terms.sub(arg1, arg2);
		return yicesMinusTerm;
	}
	
	private static int makeYicesTimesTerm(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesTimesTerm = Terms.mul(arg1, arg2);
		return yicesTimesTerm;
	}
	
	private static int makeYicesDivisionTerm(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesDivisionTerm = Terms.div(arg1, arg2);
		return yicesDivisionTerm;
	}
	
	private static int makeYicesExponentiationTerm(YicesTerm[] functionArguments) {
		int arg1 = (int) functionArguments[0].getEmeddedReference();
		int arg2 = (int) functionArguments[1].getEmeddedReference();
		int yicesExponentiationTerm = -1;
		try{
			yicesExponentiationTerm = Terms.power(arg1, arg2);
		}
		catch (Exception e) {
			yicesExponentiationTerm = -1;
			println(e);
			println("||| ERROR: Could not construct Yices Power Term with arguments " + arg1 + " and " + arg2 + "!|||");
		}
		return yicesExponentiationTerm;
	}
	
	
	
	
	
	//////////////// YICES CONTEXT ///////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// CONTEXT MAKERS
	//////////////////////////////////////////////////////	
    @Override
    public Object makeSMTSolverContextObject() {
    	com.sri.yices.Context yicesContext = makeYicesSolverContextObject();
    	return yicesContext;
    }
    
    @Override
    public SMTContext makeExpressoSMTContext() {
    	com.sri.yices.Context yicesContext = makeYicesSolverContextObject();
    	SMTContext expressoSMTContext = new YicesContext(yicesContext);
    	return expressoSMTContext;
    }
    
    ///////////////////////////////////////////////////////
    
	public static com.sri.yices.Context makeYicesSolverContextObject() {
		com.sri.yices.Context yicesContext = new com.sri.yices.Context();
		return yicesContext;
	}
	
	public static com.sri.yices.Context makeYicesSolverContextObject(String contextConfiguration) {
		Config cfg = new Config();
        cfg.set("mode", contextConfiguration);
		com.sri.yices.Context yicesContext = new com.sri.yices.Context(cfg);
		return yicesContext;
	}

	
	
	// UTILITY METHODS
	//////////////////////////////////////////////////////
	@Override
	public Object assertOntoContex(Object smtContext, SMTFormula smtFormula) {
		Object assertedSMTContext = assertOntoContext(smtContext,smtFormula);
		return assertedSMTContext;
	}

	@Override
	public Object assertOntoContex(Object smtContext, SMTFormula... smtFormula) {
		Object assertedSMTContext = assertOntoContext(smtContext,smtFormula);
		return assertedSMTContext;
	}
	
	@Override
	public Object pushStackFrame(Object smtContext) {
		Object yicesContext = pushYicesContextStackFrame(smtContext);
		return yicesContext;
	}

	@Override
	public Object popStackFrame(Object smtContext) {
		Object yicesContext = popYicesContextStackFrame(smtContext);
		return yicesContext;
	}

	@Override
	public boolean contextIsSatisfiable(Object smtContext) {
		boolean contextIsSAT = yicesContextIsSatisfiable(smtContext);
		return contextIsSAT;
	}
	
	@Override
	public String getModel(Object smtContext) {
		String model = getYicesModel(smtContext);
		return model;
	}
	
	///////////////////////////////////////////////////////////////////////////////////
	
	public static Object assertOntoContext(Object smtContext, SMTFormula smtFormula) {
		myAssert(SMT_SOLVER_SMT_CONTEXT_CLASS.isAssignableFrom(smtContext.getClass()), ()->"ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " which is not an appropriate Yices context!");
		myAssert(EXPRESSO_SMT_FORMULA_CLASS_BRANCH.isAssignableFrom(smtFormula.getClass()), ()->"ERROR: smtFormula is of type " + smtFormula.getClass().getSimpleName() + " which is not an appropriate Yices formula reference!");
		int yicesFormula = (int) smtFormula.getEmbeddedFormulaObject();
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext;
		yicesContext.assertFormula(yicesFormula);
		return yicesContext;
	}
	
	public static Object assertOntoContext(Object smtContext, SMTFormula... smtFormula) {
		myAssert(SMT_SOLVER_SMT_CONTEXT_CLASS.isAssignableFrom(smtContext.getClass()), ()->"ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " which is not an appropriate Yices context!");
		myAssert(EXPRESSO_SMT_FORMULA_CLASS_BRANCH.isAssignableFrom(smtFormula[0].getClass()), ()->"ERROR: smtFormula is array of type " + smtFormula[0].getClass().getSimpleName() + " which is not an appropriate Yices formula reference!");
		int[] embeddedYicesFormulas = new int[smtFormula.length];
		int i = 0;
		for(SMTFormula yicesFormula : smtFormula) {
			embeddedYicesFormulas[i] = (int) yicesFormula.getEmbeddedFormulaObject();
			++i;
		}
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext;
		yicesContext.assertFormulas(embeddedYicesFormulas);
		return yicesContext;
	}
	
	public static com.sri.yices.Context pushYicesContextStackFrame(Object smtContext) {
		myAssert(SMT_SOLVER_SMT_CONTEXT_CLASS.isAssignableFrom(smtContext.getClass()), ()->"ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " which is not an appropriate Yices context!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext;
		yicesContext.push();
		return yicesContext;
	}
	
	public static com.sri.yices.Context popYicesContextStackFrame(Object smtContext) {
		myAssert(SMT_SOLVER_SMT_CONTEXT_CLASS.isAssignableFrom(smtContext.getClass()), ()->"ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " which is not an appropriate Yices context!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext;
		yicesContext.pop();
		return yicesContext;
	}

	public static boolean yicesContextIsSatisfiable(Object smtContext) {
		myAssert(SMT_SOLVER_SMT_CONTEXT_CLASS.isAssignableFrom(smtContext.getClass()), ()->"ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " which is not an appropriate Yices context!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext;
		boolean yicesContextIsSatisfiable = (yicesContext.check() == Status.SAT);
		return yicesContextIsSatisfiable;
	}
	
	public static String getYicesModel(Object smtContext) {
		myAssert(SMT_SOLVER_SMT_CONTEXT_CLASS.isAssignableFrom(smtContext.getClass()), ()->"ERROR: smtContext is of type " + smtContext.getClass().getSimpleName() + " which is not an appropriate Yices context!");
		com.sri.yices.Context yicesContext = (com.sri.yices.Context) smtContext;
		String model;
		try{
			Model m = yicesContext.getModel();
			model = m.toString().replace("\n", " ").replace("\r", "");
		}
		catch (YicesException e) {
			model = "Unable to extract model given the smt context state!!!";
		}
		return model;
	}

	
	
	

	//////////////// YICES TERM //////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
	
	
	
	// INNER CLASSES
	//////////////////////////////////////////////////////	
    private static class ExpressoToYicesSymbolRegistrationInformation {

		private final String name;
		private final Expression expression;
    	private final Type type;
    	private final Context context;
    	private final SMTContext smtContext;
    	
    	ExpressoToYicesSymbolRegistrationInformation(String symbolName, Expression expression, Type type, Context context, SMTContext smtContext){
    		this.name = symbolName;
    		this.expression = expression;
    		this.type = type;
    		this.context = context;
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
		Context getContext() {
			return context;
		}
		SMTContext getSMTContext() {
			return smtContext;
		}
    }
    
    
	
	// TERM MAKERS
	//////////////////////////////////////////////////////	
    @Override
    public Object makeSMTSolverTermObjectFromExpressoExpression(Expression expression, Context context, SMTContext smtContext) {
    	int yicesTerm = makeYicesSolverTermObjectFromExpressoExpression(expression, context, smtContext);
    	return yicesTerm;
    }

	@Override
	public SMTTerm makeExpressoSMTTermFromExpressoExpression(Expression expression, Context context, SMTContext smtContext) {
		int yicesTerm = makeYicesSolverTermObjectFromExpressoExpression(expression, context, smtContext);
		SMTTerm expressoYicesTerm = new YicesTerm(yicesTerm);
		return expressoYicesTerm;
	}
    
    ///////////////////////////////////////////////////////
    
	public static int makeYicesSolverTermObjectFromExpressoExpression(Expression expression, Context context, SMTContext smtContext) {
		myAssert(EXPRESSO_SMT_CONTEXT_CLASS_BRANCH.isAssignableFrom(smtContext.getClass()), ()->"ERROR: Attempting to construct Yices term with " + smtContext.getClass().getSimpleName() + "!");

		int yicesTermReference = -1;
		
		Expression simplifiedExpression = simplifyExpression(expression, context);
		Class<? extends Expression> simplifiedExpressionClass = simplifiedExpression.getClass();
		boolean simplifiedExpressionIsSymbol = Symbol.class.isAssignableFrom(simplifiedExpressionClass);
		boolean simplifiedExpressionIsDefaultFunctionApplication = DefaultFunctionApplication.class.isAssignableFrom(simplifiedExpressionClass);
		myAssert(simplifiedExpressionIsSymbol || simplifiedExpressionIsDefaultFunctionApplication,
				()->"ERROR: In attempts to simplify a " + expression.getClass().getSimpleName() + " object, a resulting " + simplifiedExpressionClass.getSimpleName() + " object was created but a " + Symbol.class.getSimpleName() + " or a " + DefaultFunctionApplication.class.getSimpleName() + " was expected!" );

		if(Symbol.class.isAssignableFrom(simplifiedExpressionClass)) {
			yicesTermReference = makeYicesSymbolFromExpressoSymbol(simplifiedExpression, context, smtContext);
		}
		else { //simplified expression is a DefaultFunctionApplication
			Expression functor = expression.getFunctor();
			List<Expression> expressionArguments = expression.getArguments();
			YicesTerm[] yicesTerms = makeCorrespondingYicesTerms(expressionArguments, context, smtContext);
			yicesTermReference = applyYicesFunctionApplication(functor, yicesTerms);
		}

		return yicesTermReference;
	}

	private static int makeYicesSymbolFromExpressoSymbol(Expression symbol, Context context,
			SMTContext smtContext) {
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
				Type symbolType = GrinderUtil.getTypeOfExpression(symbol, context);
				ExpressoToYicesSymbolRegistrationInformation symbolInfo = 
						new ExpressoToYicesSymbolRegistrationInformation(symbolName, symbol, symbolType, context, smtContext);
				yicesTermReference = registerSymbolWithYices(symbolType, symbolInfo);
			}
		}
		return yicesTermReference;
	}
	
	private static YicesTerm[] makeCorrespondingYicesTerms(List<Expression> expressionTerms, Context context, SMTContext smtContext) {
		YicesTerm[] yicesTerms = new YicesTerm[expressionTerms.size()];
		int i = 0;
		for(Expression expression : expressionTerms) {
			yicesTerms[i] = new YicesTerm(expression, context, smtContext);
			++i;
		}
		return yicesTerms;
	}
	
	
	
	// UTILITY METHODS
	//////////////////////////////////////////////////////
    @Override
	public boolean isRegistered(Object smtTerm) {
		boolean yicesTermIsRegistered = yicesTermIsRegistered(smtTerm);
		return yicesTermIsRegistered;
	}
    
	@Override
	public Object getTypeReference(Object smtTerm) {
		Object symbolTypeReference = getCorrespondingSMTTypeReference(smtTerm);
		return symbolTypeReference;
	}

	@Override
	public String getTypeName(Object smtTerm) {
		String typeNmae = getCorrespondingSMTTypeName(smtTerm);
		return typeNmae;
	}
	
	@Override
	public String getSymbolName(Object smtTerm) {
		String symbolName = getSymbolName(smtTerm);
		return symbolName;
	}
    
    //////////////////////////////////////////////////////
	
	public static boolean yicesTermIsRegistered(Object smtTerm) {
		myAssert(SMT_SOLVER_SMT_TERM_CLASS.isAssignableFrom(smtTerm.getClass()), ()->"ERROR: Attempting to check Yices registration of " + smtTerm.getClass().getSimpleName() + "!");
		int yicesReference = (int) smtTerm;
		boolean symbolIsRegistered = (yicesReference >= 0);
		return symbolIsRegistered;
	}
	
	public static Object getCorrespondingSMTTypeReference(Object smtTerm) {
		myAssert(SMT_SOLVER_SMT_TERM_CLASS.isAssignableFrom(smtTerm.getClass()), ()->"ERROR: smtTerm is of type " + smtTerm.getClass().getSimpleName() + " which is not an appropriate Yices term reference!");
		Integer yicesTermReference = (Integer) smtTerm;
		int symbolTypeReference = Terms.typeOf(yicesTermReference);
		return symbolTypeReference;
	}

	public static String getCorrespondingSMTTypeName(Object smtTerm) {
		myAssert(SMT_SOLVER_SMT_TERM_CLASS.isAssignableFrom(smtTerm.getClass()), ()->"ERROR: smtTerm is of type " + smtTerm.getClass().getSimpleName() + " which is not an appropriate Yices term reference!");
		Integer yicesTermReference = (Integer) smtTerm;
		String typeNmae = Types.getName( Terms.typeOf(yicesTermReference) );
		return typeNmae;
	}
	
	public static String getCorrespondingSMTSymbolName(Object smtTerm) {
		myAssert(SMT_SOLVER_SMT_TERM_CLASS.isAssignableFrom(smtTerm.getClass()), ()->"ERROR: smtTerm is of type " + smtTerm.getClass().getSimpleName() + " which is not an appropriate Yices term reference!");
		Integer yicesTermReference = (Integer) smtTerm;
		String symbolName = Terms.getName(yicesTermReference);
		return symbolName;
	}

	

	// PRIVATE HELPER METHODS 
	//////////////////////////////////////////////////////
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
	
	private static Expression simplifyExpression(Expression expression, Context context) {
		Expression simplifiedExpression;
		Class<? extends Expression> expressionClass = expression.getClass();
		if( Symbol.class.isAssignableFrom(expressionClass) ) {
			simplifiedExpression = expression;
		}
		else {
			simplifiedExpression = context.simplify(expression);
		}
		return simplifiedExpression;
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
		
		SMTContext smtContext = symbolInfo.getSMTContext();
		IntegerInterval symbolIntegerIntervalType = (IntegerInterval) symbolInfo.getType();

		if (symbolIntegerIntervalType.noLowerBound()) {
			; //no lower bound to assert onto smtContext
		}
		else {
			Expression lowerBoundExpression = simplifyExpression(symbolIntegerIntervalType.getNonStrictLowerBound(), symbolInfo.getContext());
			Rational rationalLowerBound = lowerBoundExpression.rationalValue();
			BigInteger bigIntegerLowerBound = new BigInteger(rationalLowerBound.toString());
			SMTFormula yicesLowerBoundTerm = new YicesFormula( Terms.arithGeq(yicesSymbolRegistration, Terms.intConst(bigIntegerLowerBound)) );
			smtContext.assertOntoContext(yicesLowerBoundTerm);
		}
		
		if (symbolIntegerIntervalType.noUpperBound()) {
			; //no upper bound to assert onto smt Context
		}
		else {
			Expression upperBoundExpression = simplifyExpression(symbolIntegerIntervalType.getNonStrictUpperBound(), symbolInfo.getContext());
			Rational rationalUpperBound = upperBoundExpression.rationalValue();
			BigInteger bigIntegerUpperBound = new BigInteger(rationalUpperBound.toString());
			SMTFormula yicesUpperBoundTerm = new YicesFormula( Terms.arithLeq(yicesSymbolRegistration, Terms.intConst(bigIntegerUpperBound)) );
			smtContext.assertOntoContext(yicesUpperBoundTerm);
		}
	}
	
	private static void enforceRealIntervalBounds(int yicesSymbolRegistration,
			ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		//TODO use Rational instead of BigRational (currently Terms.rationalConst(BigRational) is incompatible with Rational)
		//TODO more efficient conversion from Rational to BigRational. (ex. using Terms.parseRational(), but that still parses a string...)
		
		SMTContext smtContext = symbolInfo.getSMTContext();
		RealInterval symbolRealIntervalType = (RealInterval) symbolInfo.getType();

		if(symbolRealIntervalType.noLowerBound()) {
			; //no lower bound to assert onto smtContext
		}
		else {
			Expression lowerBoundExpression = simplifyExpression(symbolRealIntervalType.getLowerBound(), symbolInfo.getContext());
			Rational rationalLowerBound = lowerBoundExpression.rationalValue();
			BigRational bigRationalLowerBound = new BigRational(rationalLowerBound.toString());
			
			SMTFormula yicesLowerBoundTerm;
			if(symbolRealIntervalType.lowerBoundIsOpen()) {
				yicesLowerBoundTerm = new YicesFormula( Terms.arithGt(yicesSymbolRegistration, Terms.rationalConst(bigRationalLowerBound)) );
			}
			else {
				yicesLowerBoundTerm = new YicesFormula( Terms.arithGeq(yicesSymbolRegistration, Terms.rationalConst(bigRationalLowerBound)) );
			}
			smtContext.assertOntoContext(yicesLowerBoundTerm);
		}
		if (symbolRealIntervalType.noUpperBound()){
			; //no upper bound to assert onto smt Context
		}
		else {
			Expression upperBoundExpression = simplifyExpression(symbolRealIntervalType.getUpperBound(), symbolInfo.getContext());
			Rational rationalUpperBound = upperBoundExpression.rationalValue();
			BigRational bigRationalUpperBound = new BigRational(rationalUpperBound.toString());
			
			SMTFormula yicesUpperBoundTerm;
			if(symbolRealIntervalType.upperBoundIsOpen()) {
				yicesUpperBoundTerm = new YicesFormula( Terms.arithLt(yicesSymbolRegistration, Terms.rationalConst(bigRationalUpperBound)) );
			}
			else {
				yicesUpperBoundTerm = new YicesFormula( Terms.arithLeq(yicesSymbolRegistration, Terms.rationalConst(bigRationalUpperBound)) );
			}
			smtContext.assertOntoContext(yicesUpperBoundTerm);
		}
	}

}
