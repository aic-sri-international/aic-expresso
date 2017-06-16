package com.sri.ai.test.grinder.sgdpllt.anytime;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.anytime.Examples;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.anytime.VariableComponent;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
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
		 return makeSymbol(randomGenerator.nextInt(20));
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
	

	public static void main(String[] args) {
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		Context context = new TrueContext(theory);			
		context = context.add(BOOLEAN_TYPE);
		
		Model m = IsingModel(2, 2,theory, context, parse("Boolean"));
		
		for(Pair<Expression, Expression> e : in(m.map.iterator())){
			println(e.second + " -> " + e.first);
		}
				
		VariableComponent comp = new VariableComponent(parse("A_0_0"), null, m, new HashSet<Expression>(), true);
		
		 Examples.runningPartialTest(comp, 50);
		
	}
	
}
