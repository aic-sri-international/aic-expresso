package com.sri.ai.expresso.smt.core.yices;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.ToIntFunction;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.smt.api.SMTContext;
import com.sri.ai.expresso.smt.api.SMTFormula;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.yices.Terms;

public class YicesFormula implements SMTFormula {
	
	
	// STATIC DATA FIELDS
	//////////////////////////////////////////////////////
	//TODO create objects in another, more general, class such as in FunctorConstants
	public static final Expression EQUALITY_EXPRESSION_FUNCTOR  = Expressions.makeSymbol("=");
	public static final Expression DISEQUALITY_EXPRESSION_FUNCTOR  = Expressions.makeSymbol("!=");
	public static final Expression GREATER_THAN_EXPRESSION_FUNCTOR = Expressions.makeSymbol(">");
	public static final Expression LESS_THAN_EXPRESSION_FUNCTOR = Expressions.makeSymbol("<");
	public static final Expression LESS_THAN_OR_EQUAL_TO_EXPRESSION_FUNCTOR = Expressions.makeSymbol("<=");
	public static final Expression GREATER_THAN_OR_EQUAL_TO_EXPRESSION_FUNCTOR = Expressions.makeSymbol(">=");
	public static final Expression NOT_EXPRESSION_FUNCTOR = Expressions.makeSymbol("not");
	
	private static final Map< Expression, ToIntFunction<YicesSymbol[]> > EXPRESSO_FUNCTOR_TO_YICES_LITERAL_FORMULA =
			new LinkedHashMap<>() {
				private static final long serialVersionUID = 1L;
			{
        put(EQUALITY_EXPRESSION_FUNCTOR, (functionArguments)->constructYicesEqualityFormula(functionArguments));
        put(DISEQUALITY_EXPRESSION_FUNCTOR, (functionArguments)->constructYicesDisequalityFormula(functionArguments));
        put(GREATER_THAN_EXPRESSION_FUNCTOR, (functionArguments)->constructYicesGreaterThanFormula(functionArguments));
        put(LESS_THAN_EXPRESSION_FUNCTOR, (functionArguments)->constructYicesLessThanFormula(functionArguments));
        put(LESS_THAN_OR_EQUAL_TO_EXPRESSION_FUNCTOR, (functionArguments)->constructYicesLessThanOrEqualToFormula(functionArguments));
        put(GREATER_THAN_OR_EQUAL_TO_EXPRESSION_FUNCTOR, (functionArguments)->constructYicesGreaterThanOrEqualToFormula(functionArguments));
        put(NOT_EXPRESSION_FUNCTOR, (functionArguments)->constructYicesNotFormula(functionArguments));
    }};
    
    
    
	
	// DATA MEMEBERS
	//////////////////////////////////////////////////////
	private final int yicesFormula;
	
	
	
	
	// CONSTRUCTORS
	//////////////////////////////////////////////////////
	public YicesFormula(int yicesFormula) {
		this.yicesFormula = yicesFormula;
	}

	public YicesFormula(Expression literal, Context context, SMTContext smtContext) {
		Expression functor = literal.getFunctor();
		if (functor != null){
			List<Expression> functionArguments = literal.getArguments();
			YicesSymbol[] smtSymbols = createCorrespondingYicesSymbols(functionArguments, context, smtContext);
			yicesFormula = constructYicesLiteral(functor, smtSymbols);
		}
		else { //(functor == null, implying expression is a Boolean symbol)
			//TODO remove assertion
			assert(context.getTypeOfRegisteredSymbol(literal) == GrinderUtil.BOOLEAN_TYPE);
			YicesSymbol yicesSymbol = new YicesSymbol(literal, context, smtContext);
			yicesFormula = yicesSymbol.getIntSymbolReference();
		}
	}
	
	
	
	
	// OVERRIDING INHERITED METHODS : SMTFormula 
	//////////////////////////////////////////////////////
	@Override
	public Object getFormula() {
		return yicesFormula;
	}
	
	
	
	
	// PRIVATE HELPER METHODS
	//////////////////////////////////////////////////////
	private YicesSymbol[] createCorrespondingYicesSymbols(List<Expression> symbols, Context context, SMTContext smtContext) {
		YicesSymbol[] smtSymbols = new YicesSymbol[symbols.size()];
		int i = 0;
		for(Expression symbol : symbols) {
			smtSymbols[i] = new YicesSymbol(symbol, context, smtContext);
			++i;
		}
		return smtSymbols;
	}
	
	private int constructYicesLiteral(Expression functor, YicesSymbol[] functionArguments) {
		ToIntFunction<YicesSymbol[]> correspondingYicesFunctor = 
				EXPRESSO_FUNCTOR_TO_YICES_LITERAL_FORMULA.get(functor);
		int yicesLiteral = correspondingYicesFunctor.applyAsInt(functionArguments);
		return yicesLiteral;
	}
	
	
	private static int constructYicesEqualityFormula(YicesSymbol[] functionArguments) {
		int arg1 = functionArguments[0].getIntSymbolReference();
		int arg2 = functionArguments[1].getIntSymbolReference();
		int yicesEqualityFormula = Terms.eq(arg1, arg2);
		return yicesEqualityFormula;
	}
	
	private static int constructYicesDisequalityFormula(YicesSymbol[] functionArguments) {
		int arg1 = functionArguments[0].getIntSymbolReference();
		int arg2 = functionArguments[1].getIntSymbolReference();
		int yicesDisequalityFormula = Terms.neq(arg1, arg2);
		return yicesDisequalityFormula;
	}
	
	private static int constructYicesGreaterThanFormula(YicesSymbol[] functionArguments) {
		int arg1 = functionArguments[0].getIntSymbolReference();
		int arg2 = functionArguments[1].getIntSymbolReference();
		int yicesGreaterThanFormula = Terms.arithGt(arg1, arg2);
		return yicesGreaterThanFormula;
	}
	
	private static int constructYicesLessThanFormula(YicesSymbol[] functionArguments) {
		int arg1 = functionArguments[0].getIntSymbolReference();
		int arg2 = functionArguments[1].getIntSymbolReference();
		int yicesLessThanFormula = Terms.arithLt(arg1, arg2);
		return yicesLessThanFormula;
	}
	
	private static int constructYicesLessThanOrEqualToFormula(YicesSymbol[] functionArguments) {
		int arg1 = functionArguments[0].getIntSymbolReference();
		int arg2 = functionArguments[1].getIntSymbolReference();
		int yicesLessThanOrEqualToFormula = Terms.arithLeq(arg1, arg2);
		return yicesLessThanOrEqualToFormula;
	}
	
	private static int constructYicesGreaterThanOrEqualToFormula(YicesSymbol[] functionArguments) {
		int arg1 = functionArguments[0].getIntSymbolReference();
		int arg2 = functionArguments[1].getIntSymbolReference();
		int yicesGreaterThanOrEqualToFormula = Terms.arithGeq(arg1, arg2);
		return yicesGreaterThanOrEqualToFormula;
	}
	
	private static int constructYicesNotFormula(YicesSymbol[] functionArguments) {
		int arg = functionArguments[0].getIntSymbolReference();
		int yicesNotFormula = Terms.not(arg);
		return yicesNotFormula;
	}

}
