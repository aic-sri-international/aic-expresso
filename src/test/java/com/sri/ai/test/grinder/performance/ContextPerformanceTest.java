package com.sri.ai.test.grinder.performance;

import static com.sri.ai.expresso.core.DefaultSymbol.createSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.List;

import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.*;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.anytime.VariableComponent;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.core.constraint.ContextSplitting;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;



public class ContextPerformanceTest {
	
	
			//////////////////////////////////////////////////////
			// GLOBAL TEST SETTINGS///////////////////////////////
			//////////////////////////////////////////////////////
		
				static final int numberOfVariables = 3;
				static final int variableCardinalities = 3;
			
			//////////////////////////////////////////////////////
			
				

	// OTHER GLOBAL CONSTANTS
	private static final Theory THEORY = new DifferenceArithmeticTheory(false, true);
	static final Context TESTINGCONTEXT = createContextBasedOnGlobalParameters();
	static final VariableAssignmentExpressions variableAssignmentExpressions = new VariableAssignmentExpressions();




	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//MAIN TESTING METHODS ///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	@Test
	public void testFxn() {
		recursivelySplitContext(TESTINGCONTEXT);
	}
	
	
	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//SUPPORT CLASSES AND METHODS ////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////


	/// STRUCTS W/ SUPPORTING METHODS //////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static class VariableAssignmentExpressions {
		
		ArrayList<ArrayList<Expression>> variableAssignmentExpressions;
		
		public VariableAssignmentExpressions() {
			variableAssignmentExpressions = new ArrayList<ArrayList<Expression>>(numberOfVariables);
				variableAssignmentExpressions.add(null); //to make 1-indexed (instead of 0-indexed)
			for(int variableNumber = 1; variableNumber <= numberOfVariables; ++variableNumber)
			{
				variableAssignmentExpressions.add(constructAssignmentExpressionsForVariable(variableNumber));
			}
		}
		
		private static ArrayList<Expression> constructAssignmentExpressionsForVariable(int variableNumber) {
			ArrayList<Expression> assignmentExpressionsForVariable =  new ArrayList<Expression>(variableCardinalities);
				assignmentExpressionsForVariable.add(null); //to make 1-indexed (instead of O-indexed)
			for(int assignmentValue = 1; assignmentValue <= variableCardinalities; ++assignmentValue)
			{
				Expression assignmentExpression = parse("X" + variableNumber + " = " + assignmentValue);
				assignmentExpressionsForVariable.add(assignmentExpression);
			}
			
			return assignmentExpressionsForVariable;
		}
		
		public Expression getVariableAssignmentExpressions(int variableNumber, int assignmentValue) {
			return variableAssignmentExpressions.get(variableNumber).get(assignmentValue);
		}
		
	}
	
	
	
	
	/// CONTEXT CONSTRUCTION METHODS ////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static Context createContextBasedOnGlobalParameters()	{
		Context context = new TrueContext(THEORY);
		
		Expression typeExpression = new IntegerInterval(1, variableCardinalities).toExpression();
		List<Expression> variableSymbols = new ArrayList<Expression>(numberOfVariables);
		
		for(int i = 1; i <= numberOfVariables; ++i) 
		{
			variableSymbols.add(createSymbol("X"+i));
			context = context.extendWithSymbolsAndTypes(variableSymbols.get(i-1), typeExpression);
		}
		
		return context;
	}
	
	
	
	
	/// CONTEXT SPLITTING METHODS //////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static void recursivelySplitContext(Context contextToSplit, int variableNumber) {
		
		// BASE CASE
		if(variableNumber > numberOfVariables)
		{
			println(contextToSplit);
			return;
		}
		
		
		int nextVariable = variableNumber + 1;
		
		// RECURSIVE SPLITTING
		for(int assignmentValue = 1; assignmentValue < variableCardinalities; ++assignmentValue)
		{
			Expression literalToSplitOn = variableAssignmentExpressions.getVariableAssignmentExpressions(variableNumber, assignmentValue);
			ContextSplitting splitter = new ContextSplitting(literalToSplitOn, contextToSplit);
			
			recursivelySplitContext(splitter.getContextAndLiteral(), nextVariable);
			
			contextToSplit = splitter.getContextAndLiteralNegation();			
		}
		
		recursivelySplitContext(contextToSplit, nextVariable);
	}
	
	private static void recursivelySplitContext(Context contextToSplit) {
		
		recursivelySplitContext(contextToSplit, 1);
		
	}

}
