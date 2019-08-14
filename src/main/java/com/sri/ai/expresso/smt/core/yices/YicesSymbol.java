package com.sri.ai.expresso.smt.core.yices;

import static com.sri.ai.util.Util.println;

import java.math.BigInteger;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.ToIntFunction;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.expresso.smt.api.SMTFormula;
import com.sri.ai.expresso.smt.api.SMTSymbol;
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
import com.sri.yices.Terms;
import com.sri.yices.Types;

public class YicesSymbol implements SMTSymbol {
	
	
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
	
    
    
	
	// DATA MEMBERS
	//////////////////////////////////////////////////////	
	private int yicesSymbolReference;
	
	
	
	
	// INNER CLASSES
	//////////////////////////////////////////////////////	
    private class ExpressoToYicesSymbolRegistrationInformation {

		private final String symbolName;
    	private final Type type;
    	private final SMTContext smtContext;
    	
    	ExpressoToYicesSymbolRegistrationInformation(String symbolName, Type type, SMTContext smtContext){
    		this.symbolName = symbolName;
    		this.type = type;
    		this.smtContext = smtContext;
    	}
    	
    	String getSymbolName() {
			return symbolName;
		}
		Type getType() {
			return type;
		}
		SMTContext getSMTContext() {
			return smtContext;
		}
    }
	
    
    
	
	// CONSTRUCTORS
	//////////////////////////////////////////////////////	
	public YicesSymbol(Expression symbol, Context context, SMTContext smtContext) {
		Rational symbolRationalValue = symbol.rationalValue();
		if(symbolRationalValue != null) {
			BigRational symbolBigRationalValue = new BigRational(symbolRationalValue.toString());
			yicesSymbolReference = Terms.rationalConst(symbolBigRationalValue);
		}
		else {
			//TODO currently assumes if a reference is already present, the variable was registered correctly
			String symbolName = symbol.toString();
			yicesSymbolReference = Terms.getByName(symbolName);
			if(!isRegistered()) { //symbol is not already registered with Yices
				Type expressoTypeOfSymbol = context.getTypeOfRegisteredSymbol(symbol);
				ExpressoToYicesSymbolRegistrationInformation symbolInfo = 
						new ExpressoToYicesSymbolRegistrationInformation(symbolName, expressoTypeOfSymbol, smtContext);
				yicesSymbolReference = registerSymbolWithYices(expressoTypeOfSymbol, symbolInfo);
			}
		}
	}
	
	
	
		
	// OVERRIDING INHERITED METHODS : SMTSymbol 
	//////////////////////////////////////////////////////
	@Override
	public boolean isRegistered() {
		boolean symbolIsRegistered = (yicesSymbolReference >= 0);
		return symbolIsRegistered;
	}

	@Override
	public Object getSymbolReference() {
		return yicesSymbolReference;
	}

	@Override
	public Object getTypeReference() {
		int symbolTypeReference = Terms.typeOf(yicesSymbolReference);
		return symbolTypeReference;
	}

	@Override
	public String getSymbolName() {
		String symbolName = Terms.getName(yicesSymbolReference);
		return symbolName;
	}

	@Override
	public String getTypeName() {
		String typeNmae = Types.getName( Terms.typeOf(yicesSymbolReference) );
		return typeNmae;
	}
	
	
	
	
	// CLASS METHODS 
	//////////////////////////////////////////////////////
	public int getIntSymbolReference() {
		return yicesSymbolReference;
	}

	public int getIntTypeReference() {
		int symbolTypeReference = Terms.typeOf(yicesSymbolReference);
		return symbolTypeReference;
	}
	

	// PRIVATE HELPER METHODS 
	//////////////////////////////////////////////////////
	private int registerSymbolWithYices(Type expressoTypeOfSymbol, ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		ToIntFunction<ExpressoToYicesSymbolRegistrationInformation> appropriateFunctionToRegisterSymbolBasedOnTypeClass = 
				EXPRESSO_SYMBOL_TYPE_TO_YICES_SYMBOL_REGISTRATION_METHODS.get(expressoTypeOfSymbol.getClass());
		int registeredYicesSymbolReference = appropriateFunctionToRegisterSymbolBasedOnTypeClass.applyAsInt(symbolInfo);
		return registeredYicesSymbolReference;
	}
	
	
	
	private static int registerCategoricalYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		int yicesSymbolRegistration = -1;
		if(symbolInfo.getType() == GrinderUtil.BOOLEAN_TYPE) {
			try {
				yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolInfo.getSymbolName(), Types.BOOL);
			} 
			catch (Exception e) {
				yicesSymbolRegistration = -1;
				println(e);
				println("||| ERROR: The Boolean symbol '" + symbolInfo.getSymbolName() + "' was not registered with Yices!!! |||");
			}
		}
		else {
			String typeName = symbolInfo.getType().getName();
			int typeCardinality = symbolInfo.getType().cardinality().intValue();			
	    	try {
		    	int correspondingYicesCategoricalTypeReference = Types.newScalarType(typeName, typeCardinality);
				yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolInfo.getSymbolName(), correspondingYicesCategoricalTypeReference);
			} catch (Exception e) {
				yicesSymbolRegistration = -1;
				println(e);
				println("||| ERROR: The Categorical symbol '" + symbolInfo.getSymbolName() + "' was not registered with Yices!!! |||");
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
			yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolInfo.getSymbolName(), Types.INT);
		} catch (Exception e) {
			yicesSymbolRegistration = -1;
			println(e);
			println("||| ERROR: The Integer symbol '" + symbolInfo.getSymbolName() + "' was not registered with Yices!!! |||");
		}	
		return yicesSymbolRegistration;
	}
	
	private static int registerIntegerIntervalYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		//TODO we might need to handle parameterized bounds (ie. parameterized by another variable)
		//TODO handle case for infinite bounds
		int yicesSymbolRegistration = -1;	
    	try {
			yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolInfo.getSymbolName(), Types.INT);
			enforceIntegerIntervalBounds(yicesSymbolRegistration, symbolInfo);
		} catch (Exception e) {
			yicesSymbolRegistration = -1;
			println(e);
			println("||| ERROR: The Integer Interval symbol '" + symbolInfo.getSymbolName() + "' was not registered with Yices!!! |||");
		}	
		return yicesSymbolRegistration;
	}
	
	private static int registerRealYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		int yicesSymbolRegistration = -1;	
    	try {
			yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolInfo.getSymbolName(), Types.REAL);
		} catch (Exception e) {
			yicesSymbolRegistration = -1;
			println(e);
			println("||| ERROR: The Real symbol '" + symbolInfo.getSymbolName() + "' was not registered with Yices!!! |||");
		}	
		return yicesSymbolRegistration;
	}
	
	private static int registerRealIntervalYicesSymbol(ExpressoToYicesSymbolRegistrationInformation symbolInfo) {
		//TODO we might need to handle parameterized bounds (ie. parameterized by another variable)
		//TODO handle case for infinite bounds
		int yicesSymbolRegistration = -1;	
    	try {
			yicesSymbolRegistration = Terms.newUninterpretedTerm(symbolInfo.getSymbolName(), Types.REAL);
			enforceRealIntervalBounds(yicesSymbolRegistration, symbolInfo);
		} catch (Exception e) {
			yicesSymbolRegistration = -1;
			println(e);
			println("||| ERROR: The Real Interval symbol '" + symbolInfo.getSymbolName() + "' was not registered with Yices!!! |||");
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
		
		Rational rationalLowerBound = symbolIntegerIntervalType.getNonStrictUpperBound().rationalValue();
		BigInteger bigIntegerLowerBound = new BigInteger(rationalLowerBound.toString());
		Rational rationalUpperBound = symbolIntegerIntervalType.getNonStrictLowerBound().rationalValue();
		BigInteger bigIntegerUpperBound = new BigInteger(rationalUpperBound.toString());

		if (symbolIntegerIntervalType.noLowerBound()) {
			; //no lower bound to assert onto smtContext
		}
		else {
			SMTFormula yicesLowerBoundTerm = new YicesFormula( Terms.arithGeq(yicesSymbolRegistration, Terms.intConst(bigIntegerLowerBound)) );
			smtContext.assertOntoContext(yicesLowerBoundTerm);
		}
		
		if (symbolIntegerIntervalType.noUpperBound()) {
			; //no upper bound to assert onto smt Context
		}
		else {
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
		
		Rational rationalLowerBound = symbolRealIntervalType.getUpperBound().rationalValue();
		BigRational bigRationalLowerBound = new BigRational(rationalLowerBound.toString());
		Rational rationalUpperBound = symbolRealIntervalType.getLowerBound().rationalValue();
		BigRational bigRationalUpperBound = new BigRational(rationalUpperBound.toString());

		if(symbolRealIntervalType.noLowerBound()) {
			; //no lower bound to assert onto smtContext
		}
		else {
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
