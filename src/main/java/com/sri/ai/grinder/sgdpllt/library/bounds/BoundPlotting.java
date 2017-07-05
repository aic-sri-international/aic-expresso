package com.sri.ai.grinder.sgdpllt.library.bounds;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;
import static java.lang.Math.abs;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.anytime.BPTest;
import com.sri.ai.grinder.sgdpllt.anytime.Model;
import com.sri.ai.grinder.sgdpllt.anytime.VariableComponent;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

public class BoundPlotting {
	static private double ifElseToProbability(Expression ifElseExpression){
		String ifElseStatement = ifElseExpression.toString();
		int beginOfProbaIndex = ifElseStatement.indexOf("then ") + ("then ").length();
		int divisionIndex = ifElseStatement.indexOf(" / ");
		int endOfProbaIndex = ifElseStatement.indexOf(" else");
		
		if(divisionIndex == -1){
			String numeratorString = ifElseStatement.substring(beginOfProbaIndex, endOfProbaIndex);
			int numerator = Integer.parseInt(numeratorString);
			return numerator;
		}
		
		String numeratorString = ifElseStatement.substring(beginOfProbaIndex, divisionIndex);
		int numerator = Integer.parseInt(numeratorString);
		String denominatorString = ifElseStatement.substring(divisionIndex + 3, endOfProbaIndex);
		int denominator= Integer.parseInt(denominatorString);
		double res = ((double)numerator)/denominator;
		return res;
	}
	
	static private Pair<Double, Double> twoElementBoundToProbability(Bound b){
		if(b.isExtensionalBound()){
			List<Expression> l = b.getArguments();
			if(l.size() == 2){
				Double pair1 = ifElseToProbability(l.get(0));
				Double pair2 = ifElseToProbability(l.get(1));
				return Pair.make(pair1, pair2);
			}
		}
		return null;
	}
	
	static public void storeIntervalData(Model m, Integer nb_iter, Boolean withBound, String filename){
		VariableComponent ComponentResult = new VariableComponent(parse("A_0_0"), null, m, new HashSet<Expression>(), false);
		long startTime;
		try{
		    PrintWriter writer = new PrintWriter(filename, "UTF-8");
		    
		    startTime = System.currentTimeMillis();
			int i = 0;
			while(i < nb_iter) {
				if(!ComponentResult.entirelyDiscover) {
					ComponentResult.update(new HashSet<Expression>(), withBound);
					Pair<Double, Double>  p = twoElementBoundToProbability(ComponentResult.bound);
					writer.print(i + "\t" + p.first + "\t" + p.second + "\t" + (abs(p.first - p.second)) + "\t" + (System.currentTimeMillis()-startTime));
				}
				i++;
			}
			
		    writer.close();
		} catch (IOException e) {
		   println("Failed to print in file " + filename);
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
		
		//Model m = BPTest.IsingModel(3,3,theory, context, parse("Boolean"));
		
		//storeIntervalData(m, 50, true,"text.txt");
	}
}
