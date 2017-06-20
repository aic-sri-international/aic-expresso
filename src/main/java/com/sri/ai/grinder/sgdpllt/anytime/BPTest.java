package com.sri.ai.grinder.sgdpllt.anytime;
 
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;
import static java.lang.Math.min;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.Set;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.core.DefaultIntensionalUniSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParser.IntensionalMultisetContext;
import com.sri.ai.grinder.sgdpllt.anytime.Examples;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.anytime.VariableComponent;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.bounds.Bound;
import com.sri.ai.grinder.sgdpllt.library.bounds.DefaultExtensionalBound;
import com.sri.ai.grinder.sgdpllt.library.bounds.DefaultIntensionalBound;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.println;

public class BPTest {
	private static Expression randomNumberGenerator(){
		 Random randomGenerator = new Random();
		 return makeSymbol(randomGenerator.nextInt(9));
	}
	private static Expression generateProbability(Context context,int i, Expression... listOfVariables){
		if(listOfVariables.length == 0 ){
			return null;
		}
		Expression result = makeSymbol(0);//randomNumberGenerator();
		
		Expression variable = listOfVariables[i];
		//listOfVariables.remove(0);
		Type type = context.getTypeOfRegisteredSymbol(variable);
		Iterator<Expression>  iteratorToValuesInType = type.iterator();
		
		if(listOfVariables.length == i+1){
			for(Expression value : in(iteratorToValuesInType)){
				Expression varEqualsValue = apply(EQUAL,variable,value);
				Expression randProbability = randomNumberGenerator();
				result = apply(IF_THEN_ELSE,varEqualsValue, randProbability, result);
			}
		}
		else{
			for(Expression value : in(iteratorToValuesInType)){
				Expression varEqualsValue = apply(EQUAL,variable,value);
				Expression randProbability = generateProbability(context, i+1, listOfVariables);
				result = apply(IF_THEN_ELSE,varEqualsValue, randProbability, result);
			}
		}
		return result;
	}
	private static Expression generateProbability(Context context, Expression... listOfVariables){
		return generateProbability(context, 0, listOfVariables);
	}
	
	private static Model IsingModel(int nLines, int nCols,Theory theory, Context context, Expression possibleValues){
		Set<Expression> Factor = new HashSet<Expression>();
		
		Expression[][] a = new Expression[nLines][nCols];
		
		for(int i = 0; i < nLines; i++){
			for(int j = 0; j < nCols; j++){
				a[i][j] = makeSymbol("A_"+i+"_"+j);
				context = context.extendWithSymbolsAndTypes(a[i][j],possibleValues);
			}
		}
		
		for(int i = 0; i < nLines; i++){
			for(int j = 0; j < nCols; j++){
				if (j < nCols - 1){
					Expression fHor = generateProbability(context, a[i][j], a[i][j+1]);
					Factor.add(fHor);
				}
				if(i < nLines-1){
					Expression fVer = generateProbability(context, a[i][j], a[i+1][j]);
					Factor.add(fVer);
				}
			}
		}		
		Model result = new Model(Factor,theory,context);

		return result;
	}
	
	private Model randomModel(int nVariables, int nFactors , Theory theory, Context context, Expression possibleValues){
		Set<Expression> Factor = new HashSet<Expression>();
		
		Expression[] a = new Expression[nVariables];
		for(int i = 0; i < nVariables; i++){
			a[i]= makeSymbol("A_"+i);
			context = context.extendWithSymbolsAndTypes(a[i],possibleValues);
		}
		Random rand = new Random();
		
		for (int i = 0; i < nFactors; i++) {
			int n = rand.nextInt(min(15,nVariables));
			Expression[] varOfF = new Expression[n];
			for (int j = 0; j < varOfF.length; j++) {
				varOfF[j] = a[rand.nextInt(nVariables)];
			}
			Expression f = generateProbability(context, varOfF);
			Factor.add(f);
		}
		Model result = new Model(Factor,theory,context);

		return result;
		
	}

	private static void printModel(Model m) {
		for(Pair<Expression, Expression> e : in(m.map.iterator())){
			println(e.second + " -> " + e.first);
		}
	}

	public static void main(String[] args) {
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		Context context = new TrueContext(theory);
		
		Model m = IsingModel(3,4,theory, context, parse("Boolean"));
		
		//printModel(m);
				
		runTest(m);
		
	}
	
	private static void runTest(Model m) {
		VariableComponent comp ;
		println("Go!");
		runVE(m,(parse("A_0_0")),true);
		
		comp = new VariableComponent(parse("A_0_0"), null, m, new HashSet<Expression>(), true);
		runNaive(comp, true);
		
		println("Extensional");
		comp = new VariableComponent(parse("A_0_0"), null, m, new HashSet<Expression>(), true);
		runningPartialTest(comp, 50, true);
		comp.print(0);
		println("Intensional");
		comp = new VariableComponent(parse("A_0_0"), null, m, new HashSet<Expression>(), false);		
		runningPartialTest(comp, 50, true);
	}

	private static void runningPartialTest(VariableComponent ComponentResult, Integer nb_iter, Boolean withBound) {
		long startTime, endTime, totalTime;
		
		//we compute the result with our algorithm
		//we also store the computation time to compare it to the naive computation time
		startTime = System.currentTimeMillis();
		int i = 0;
		while(i < nb_iter) {
			if(!ComponentResult.entirelyDiscover) {
				ComponentResult.update(new HashSet<Expression>(), withBound);
				println("... " + i + " error :" + ComponentResult.bound);//getError(ComponentResult.bound, ComponentResult.model.theory,ComponentResult.model.context));//println("Bound at iteration " + i + " : " + ComponentResult.bound);
			}
			i++;
		}
		
		Expression normalizedMessage = ComponentResult.bound;
		endTime   = System.currentTimeMillis();
		totalTime = endTime - startTime;
		
		System.out.println("\n\nOur computation : " + normalizedMessage);
		println("totalTime: " + totalTime/1000. + " seconds");
	}
	
	private static void runNaive(VariableComponent ComponentResult, Boolean withBound) {
		long startTime;
		long endTime;
		long totalTime;
		startTime = System.currentTimeMillis();
		
		Expression naiveResult = ComponentResult.naiveCalcul();
		endTime   = System.currentTimeMillis();
		totalTime = endTime - startTime;
		
		println("\n\nNaive Result : " + naiveResult);
		println("totalTime: " + totalTime/1000. + " seconds");
	}
	
	private static void runVE(Model m, Expression query, Boolean withBound) {
		long startTime;
		long endTime;
		long totalTime;
		startTime = System.currentTimeMillis();
		
		Expression naiveResult = m.VECalculation(query);
		endTime   = System.currentTimeMillis();
		totalTime = endTime - startTime;
		
		println("\n\nVE Result : " + naiveResult);
		println("totalTime: " + totalTime/1000. + " seconds");
		//println(naiveResult.getArguments());
	}
	
	private static float getError(Bound b,Theory t, Context c){
		if(b.isExtensionalBound()){
			List<Expression> l =((DefaultExtensionalBound) b).getElementsDefinitions();
			
			Expression e1 = l.get(0);
			Expression e2 = l.get(1);
			Expression n = t.evaluate(apply("-",e1.get(2),e2.get(2)),c);
			float f = n.getArguments().get(0).intValue() / n.getArguments().get(1).intValue() ;
			return f;
		}
		return 0;
	}

}
