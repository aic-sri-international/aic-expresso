package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.function.Function;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.InversionSimplifier;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
import com.sri.ai.util.base.Pair;

public class InversionPerformanceEvaluationTest {
	private Context context;
	private InversionSimplifier inversionSimplifier;
	private BruteForceCommonInterpreter bruteForceInterpreter;
	private Expression[] sumProducts = new Expression[] {
			parse("sum({{(on f in 1..2 -> 1..5) product({{(on X in 1..2) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..3 -> 1..5) product({{(on X in 1..3) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..4 -> 1..5) product({{(on X in 1..4) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..5 -> 1..5) product({{(on X in 1..5) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..6 -> 1..5) product({{(on X in 1..6) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..7 -> 1..5) product({{(on X in 1..7) f(X) : true }}) : true}})")
	};
	
	@Before
	public void setUp() {
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(false, false), 
						new TupleTheory()));
		
		inversionSimplifier = new InversionSimplifier();
		bruteForceInterpreter = new BruteForceCommonInterpreter();
	}
	
	// Comment out when you want to run evaluation.
	@Ignore
	@Test
	public void evaluatePerformance() {
		for (int i = 0; i < sumProducts.length; i++) {
			evaluatePerformance(sumProducts[i]);
		}
	}
	
	private void evaluatePerformance(Expression sumProduct) {
		Pair<Expression, Long> bruteForceResultAndDuration = evaluate(sumProduct, sp -> {
			Expression r = bruteForceInterpreter.apply(sp, context);
			return r;
		});
		Pair<Expression, Long> inversionResultAndDuration = evaluate(sumProduct, sp -> {
			Expression possiblyInverted = inversionSimplifier.apply(sp, context);
			Expression r = context.getTheory().evaluate(possiblyInverted, context);
			return r;
		});
		
		System.out.println("RESULTS FOR : "+sumProduct);
		System.out.println("Brute Force Result = "+bruteForceResultAndDuration.first);
		System.out.println("Inversion Result   = "+inversionResultAndDuration.first);
		System.out.println("Brute Force Took   = "+bruteForceResultAndDuration.second+"ms.");
		System.out.println("Inversion Took     = "+inversionResultAndDuration.second+"ms.");
		System.out.println("----");
	}
	
	private Pair<Expression, Long> evaluate(Expression sumProduct, Function<Expression, Expression> evaluatorFunction) {
		long start = System.currentTimeMillis();
		
		Expression evalResult = evaluatorFunction.apply(sumProduct);
		
		long duration = System.currentTimeMillis() - start;
		
		Pair<Expression, Long> result = new Pair<>(evalResult, duration);
		
		return result;
	}
}
