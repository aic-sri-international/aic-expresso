package com.sri.ai.grinder.tester;

import static com.sri.ai.expresso.core.DefaultSymbol.createSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.sum;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.core.constraint.ContextSplitting;


public class ContextSplittingTester {
	
	// TESTING PARAMETERS
	private final int numberOfVariables;
	private final int cardinalityOfVariables;
	
	// TESTING STYLE
	private final boolean verbose;
	
	// OTHER GLOBAL CONSTANTS
	private final Theory theory;
	private final Context contextToTest;
	private final VariableAssignmentExpressions variableAssignmentExpressions;
	private final int numberOfPotentials;
	private final int totalNumberOfContextsNeededToReachTerminalContexts;
	
	// STORED RESULTS
	private ContextSplittingResults contextSplittingResults;
	private long totalSplittingTime;
	
	
	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//CONSTRUCTORS ///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public ContextSplittingTester(int numberOfVariables, int cardinalityOfVariables, boolean verbose, Theory theory)
	{
		this.numberOfVariables = numberOfVariables;
		this.cardinalityOfVariables = cardinalityOfVariables;
		this.verbose = verbose;
		this.theory = theory;
		this.contextToTest = createContextBasedOnGlobalParameters();
		this.variableAssignmentExpressions = new VariableAssignmentExpressions();
		this.numberOfPotentials = numberOfTerminalContexts(cardinalityOfVariables, numberOfVariables);
		this.totalNumberOfContextsNeededToReachTerminalContexts = numberOfTraversedContextsPerformingBinarySplittingOfVariables(numberOfPotentials);

	}
	
	


	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MAIN TESTING METHODS ///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public long performContextSplittingTest() {

		contextSplittingResults = null;
		recursivelySplitContext(contextToTest);

		return totalSplittingTime;
	}

	
	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PUBLIC PRINT METHODS ///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public void printLastTestdResults() {
		printContextSplittingResults();
	}


	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// SUPPORT CLASSES AND METHODS ////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////


	/// STRUCTS W/ SUPPORTING METHODS //////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private class VariableAssignmentExpressions {
		
		private ArrayList<ArrayList<Expression>> variableAssignmentExpressions;
		
		public VariableAssignmentExpressions() {
			variableAssignmentExpressions = new ArrayList<ArrayList<Expression>>(numberOfVariables);
				variableAssignmentExpressions.add(null); //to make 1-indexed (instead of 0-indexed)
			for(int variableNumber = 1; variableNumber <= numberOfVariables; ++variableNumber)
			{
				variableAssignmentExpressions.add(constructAssignmentExpressionsForVariable(variableNumber));
			}
		}
		
		private ArrayList<Expression> constructAssignmentExpressionsForVariable(int variableNumber) {
			ArrayList<Expression> assignmentExpressionsForVariable =  new ArrayList<Expression>(cardinalityOfVariables);
				assignmentExpressionsForVariable.add(null); //to make 1-indexed (instead of O-indexed)
			for(int assignmentValue = 1; assignmentValue <= cardinalityOfVariables; ++assignmentValue)
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
	
	
	private static class ContextSplittingResults {
		public ArrayList<Context> generatedContexts;
		public ArrayList<Expression> literalsToSplitOn;
		public ArrayList<Long> timeToSplitContext;
		public ArrayList<Context> finalContexts;
		
		public ContextSplittingResults(int estimatedNumTotalContexts, int estimatedNumTerminalContexts) {
			generatedContexts = new ArrayList<>(estimatedNumTotalContexts);
			literalsToSplitOn = new ArrayList<>(estimatedNumTotalContexts);
			timeToSplitContext =  new ArrayList<Long>(estimatedNumTotalContexts);
			finalContexts = new ArrayList<>(estimatedNumTerminalContexts);
		}
	}
	
	
	
	
	/// CONTEXT CONSTRUCTION METHODS ////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private Context createContextBasedOnGlobalParameters()	{
		Context context = new TrueContext(theory);
		
		Expression typeExpression = new IntegerInterval(1, cardinalityOfVariables).toExpression();
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
	
	private void recursivelySplitContext(Context contextToSplit, int variableNumber) {
		
		// BASE CASE
		if(variableNumber > numberOfVariables)
		{
			contextSplittingResults.generatedContexts.add(contextToSplit);
			contextSplittingResults.literalsToSplitOn.add(null);
			contextSplittingResults.timeToSplitContext.add(0L);
			contextSplittingResults.finalContexts.add(contextToSplit);
			return;
		}
	
		
		int nextVariable = variableNumber + 1;
		
		// RECURSIVE SPLITTING
		for(int assignmentValue = 1; assignmentValue < cardinalityOfVariables; ++assignmentValue)
		{
			contextSplittingResults.generatedContexts.add(contextToSplit);
			
			Expression literalToSplitOn = variableAssignmentExpressions.getVariableAssignmentExpressions(variableNumber, assignmentValue);
			contextSplittingResults.literalsToSplitOn.add(literalToSplitOn);
			
			ContextSplitting splitter = splitContextAndRecordTime(literalToSplitOn, contextToSplit);
			recursivelySplitContext(splitter.getContextAndLiteral(), nextVariable);
			contextToSplit = splitter.getContextAndLiteralNegation();
		}
		
		recursivelySplitContext(contextToSplit, nextVariable);
	}
	
	private void recursivelySplitContext(Context contextToSplit) {
		
		if(verbose) {
			contextSplittingResults = new ContextSplittingResults(totalNumberOfContextsNeededToReachTerminalContexts, numberOfPotentials);
			recursivelySplitContext(contextToSplit, 1);
			totalSplittingTime = sum(contextSplittingResults.timeToSplitContext).longValue();
		}
		else {
			long startTime;
			long endTime;
			
			startTime = System.currentTimeMillis();
			recursivelySplitContextForTotalTime(contextToTest, 1);
			endTime = System.currentTimeMillis();
			
			totalSplittingTime = endTime - startTime;
		}
	}
	
	
	
	private ContextSplitting splitContextAndRecordTime(Expression literalToSplitOn, Context contextToSplit) {
		long startTime = 0;
		long endTime = 0;
		
		startTime = System.currentTimeMillis();
		ContextSplitting splitter = new ContextSplitting(literalToSplitOn, contextToSplit);
		endTime = System.currentTimeMillis();
		
		contextSplittingResults.timeToSplitContext.add(endTime - startTime);
		
		return splitter;
	}
	
	
	
	private void recursivelySplitContextForTotalTime(Context contextToSplit, int variableNumber) {
		
		// BASE CASE
		if(variableNumber > numberOfVariables)		{
			return;
		}
	
		
		int nextVariable = variableNumber + 1;
		
		// RECURSIVE SPLITTING
		for(int assignmentValue = 1; assignmentValue < cardinalityOfVariables; ++assignmentValue)
		{
			Expression literalToSplitOn = variableAssignmentExpressions.getVariableAssignmentExpressions(variableNumber, assignmentValue);
			
			ContextSplitting splitter = new ContextSplitting(literalToSplitOn, contextToSplit);
			recursivelySplitContextForTotalTime(splitter.getContextAndLiteral(), nextVariable);
			contextToSplit = splitter.getContextAndLiteralNegation();
		}
		
		recursivelySplitContextForTotalTime(contextToSplit, nextVariable);
	}
	
	
	
	
	/// PRINTING METHODS ///////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private void printContextSplittingResults() {
		
		if(contextSplittingResults != null)
		{
			println(" Splitting Context Cosisting of:");
			println("    Number of Variables:       " + numberOfVariables);
			println("    Cardinality of Variables:  " + cardinalityOfVariables);
			println();
			println(" Contexts Generated  /  literal use to split  /  time taken to split");
			for(int i = 0; i < contextSplittingResults.generatedContexts.size(); ++i)
			{
				Context context = contextSplittingResults.generatedContexts.get(i);
				Expression literal = contextSplittingResults.literalsToSplitOn.get(i);
				long time = contextSplittingResults.timeToSplitContext.get(i);
				println("----------------------------------------------------------------------------------------");
				println(literal == null?  terminalContextResultAsString(context) : nonTerminalContextSplittingResultAsString(context, literal, time));
			}
			println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
			println("    Total Number of Contexts Generated:  " + contextSplittingResults.generatedContexts.size());
			println("    Total Number of Terminal Contexts:   " + contextSplittingResults.finalContexts.size());
			println("----------------------------------------------------------------------------------------");
		}
		
		println("    total time of splitting:  " + totalSplittingTime + " ms");
	}
	
	private static String terminalContextResultAsString(Context context) {
		String result = "    " + context + "\n"
					  + "       |||TERMINAL CONTEXT|||";
		return result;
	}
	
	private static String nonTerminalContextSplittingResultAsString(Context context, Expression literal, long time) {
		String result = "    " + context + "\n"
					  + "       split with:     " + literal + "\n"
					  + "       time to split:  " + time + " ms";
		return result;
	}
	
	
	
	
	/// OTHER SUPPORT METHODS //////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static int numberOfTraversedContextsPerformingBinarySplittingOfVariables(int numberOfTerminalContexts) {
		int totalNumberOfTraversedContexts = (int) (2.0 * numberOfTerminalContexts - 0.5) ;
		return totalNumberOfTraversedContexts;
	}
	
	private static int numberOfTerminalContexts(int branchingFactor, int height) {
		int totalNumberOfTerminalContexts = (int)(Math.pow(branchingFactor, height) + 0.5);
		return totalNumberOfTerminalContexts;
	}

}
