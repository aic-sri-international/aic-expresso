package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.Random;
import java.util.function.Function;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.sgdpllt.interpreter.SampleCommonInterpreter;
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.InversionSimplifier;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
import com.sri.ai.util.base.Pair;

public class InversionPerformanceEvaluationTest {
	private Context context;
	private InversionSimplifier inversionSimplifier;
	private BruteForceCommonInterpreter bruteForceInterpreter;
	private SampleCommonInterpreter samplingInterpreter10;
	private SampleCommonInterpreter samplingInterpreter100;
	private SampleCommonInterpreter samplingInterpreter1000;
	private SampleCommonInterpreter samplingInterpreter10000;
	
	private Expression[] sumProducts = new Expression[] {			
			//
			parse("sum({{(on f in 1..2 -> 1..5) product({{(on X in 1..2) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..3 -> 1..5) product({{(on X in 1..3) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..4 -> 1..5) product({{(on X in 1..4) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..5 -> 1..5) product({{(on X in 1..5) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..6 -> 1..5) product({{(on X in 1..6) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..7 -> 1..5) product({{(on X in 1..7) f(X) : true }}) : true}})"),
			//
			parse("sum({{(on f in 1..2 -> 1..5) product({{(on X in 1..2) f(X) + 3 : true }}) : true}})"),
			parse("sum({{(on f in 1..3 -> 1..5) product({{(on X in 1..3) f(X) + 3 : true }}) : true}})"),
			parse("sum({{(on f in 1..4 -> 1..5) product({{(on X in 1..4) f(X) + 3 : true }}) : true}})"),
			parse("sum({{(on f in 1..5 -> 1..5) product({{(on X in 1..5) f(X) + 3 : true }}) : true}})"),
			parse("sum({{(on f in 1..6 -> 1..5) product({{(on X in 1..6) f(X) + 3 : true }}) : true}})"),
			parse("sum({{(on f in 1..7 -> 1..5) product({{(on X in 1..7) f(X) + 3 : true }}) : true}})"),
			//
			parse("sum({{(on f in 1..2 x 1..2 -> 1..5) product({{(on X in 1..2) product({{(on Y in 1..2) f(X, Y) : true }}) : true }}) : true}})"),
			parse("sum({{(on f in 1..2 x 1..3 -> 1..5) product({{(on X in 1..2) product({{(on Y in 1..3) f(X, Y) : true }}) : true }}) : true}})")
	};
	
	@Before
	public void setUp() {
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(false, false), 
						new TupleTheory()));
		
		inversionSimplifier      = new InversionSimplifier();
		bruteForceInterpreter    = new BruteForceCommonInterpreter();
		samplingInterpreter10    = new SampleCommonInterpreter(10, false, new Random(1));
		samplingInterpreter100   = new SampleCommonInterpreter(100, false, new Random(1));
		samplingInterpreter1000  = new SampleCommonInterpreter(1000, false, new Random(1));
		samplingInterpreter10000 = new SampleCommonInterpreter(10000, false, new Random(1));
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
		Pair<Expression, Long> sampling10ResultAndDuration = evaluate(sumProduct, sp -> {
			Expression r = samplingInterpreter10.apply(sp, context);
			return r;
		});
		Pair<Expression, Long> sampling100ResultAndDuration = evaluate(sumProduct, sp -> {
			Expression r = samplingInterpreter100.apply(sp, context);
			return r;
		});
		Pair<Expression, Long> sampling1000ResultAndDuration = evaluate(sumProduct, sp -> {
			Expression r = samplingInterpreter1000.apply(sp, context);
			return r;
		});
		Pair<Expression, Long> sampling10000ResultAndDuration = evaluate(sumProduct, sp -> {
			Expression r = samplingInterpreter10000.apply(sp, context);
			return r;
		});
		
		System.out.println("RESULTS FOR : "+sumProduct);
		System.out.println("Brute Force Result    = "+bruteForceResultAndDuration.first);
		System.out.println("Inversion Result      = "+inversionResultAndDuration.first);
		System.out.println("Sampling:10 Result    = "+sampling10ResultAndDuration.first);
		System.out.println("Sampling:100 Result   = "+sampling100ResultAndDuration.first);
		System.out.println("Sampling:1000 Result  = "+sampling1000ResultAndDuration.first);
		System.out.println("Sampling:10000 Result = "+sampling10000ResultAndDuration.first);
		System.out.println("Brute Force Took      = "+bruteForceResultAndDuration.second+"ms.");
		System.out.println("Inversion Took        = "+inversionResultAndDuration.second+"ms.");
		System.out.println("Sampling:10 Took      = "+sampling10ResultAndDuration.second+"ms.");
		System.out.println("Sampling:100 Took     = "+sampling100ResultAndDuration.second+"ms.");
		System.out.println("Sampling:1000 Took    = "+sampling1000ResultAndDuration.second+"ms.");
		System.out.println("Sampling:10000 Took   = "+sampling10000ResultAndDuration.second+"ms.");
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
