package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.function.BiFunction;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.sgdpllt.interpreter.SampleCommonInterpreter;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.InversionSimplifier;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

public class InversionPerformanceEvaluationTest {
	private Context context;
	
	interface SumProductInterpreter extends BiFunction<Expression, Context, Expression> {
		String getName();
	}
	
	private Expression warmUpSumProduct = parse("sum({{(on f in 1..2 -> 1..2) product({{(on X in 1..2) f(X) : true }}) : true}})");
	
	private Expression[] sumProducts = new Expression[] {			
			//
			parse("sum({{(on f in 1..2 -> 1..5) product({{(on X in 1..2) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..3 -> 1..5) product({{(on X in 1..3) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..4 -> 1..5) product({{(on X in 1..4) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..5 -> 1..5) product({{(on X in 1..5) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..6 -> 1..5) product({{(on X in 1..6) f(X) : true }}) : true}})"),
			parse("sum({{(on f in 1..7 -> 1..5) product({{(on X in 1..7) f(X) : true }}) : true}})"),
			//
			parse("sum({{(on f in 1..2 x 1..2 -> 1..5) product({{(on X in 1..2) product({{(on Y in 1..2) f(X, Y) : true }}) : true }}) : true}})"),
			parse("sum({{(on f in 1..2 x 1..3 -> 1..5) product({{(on X in 1..2) product({{(on Y in 1..3) f(X, Y) : true }}) : true }}) : true}})"),
			parse("sum({{(on f in 1..3 x 1..3 -> 1..3) product({{(on X in 1..3) product({{(on Y in 1..3) f(X, Y) : true }}) : true }}) : true}})"),
			parse("sum({{(on f in 1..3 x 1..3 -> 1..4) product({{(on X in 1..3) product({{(on Y in 1..3) f(X, Y) : true }}) : true }}) : true}})")
	};
	
	private SumProductInterpreter[] interpreters = new SumProductInterpreter[] {
			new BruteForceSumProductInterpreter(),
			new InversionSumProductInterpreter(),
			new SamplingSumProductInterpreter(10),
			new SamplingSumProductInterpreter(100),
			new SamplingSumProductInterpreter(1000),
			new SamplingSumProductInterpreter(10000),
			new SamplingSumProductInterpreter(100000),
	};
	
	@Before
	public void setUp() {
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(false, false), 
						new TupleTheory()));
	}
	
	// Comment out when you want to run evaluation.
	@Ignore
	@Test
	public void evaluatePerformance() {
		// Compute sizes
		Rational[] sizes = new Rational[sumProducts.length];
		for (int i = 0; i < sumProducts.length; i++) {
			sizes[i] = computeSize(sumProducts[i]);			
		}
		
		// Warm up interpreters
		for (int i = 0; i < interpreters.length; i++) {
			evaluate(warmUpSumProduct, interpreters[i]);
		}	
		
		List<List<Pair<Long, Expression>>> results = new ArrayList<>();
		for (int i = 0; i < sumProducts.length; i++) {
			List<Pair<Long, Expression>> sumProductResults = new ArrayList<>();
			for (int j = 0; j < interpreters.length; j++) {
				Pair<Long, Expression> sumProductResult = evaluate(sumProducts[i], interpreters[j]);
				sumProductResults.add(sumProductResult);
			}
			results.add(sumProductResults);
		}
		
		//
		// Output Headers
		System.out.print("Size\t");
		for (int i = 0; i < interpreters.length; i++) {
			System.out.print(interpreters[i].getName());
			System.out.print("\t\t");
		}
		System.out.println("Sum Product");
		System.out.print("\t");
		for (int i = 0; i < interpreters.length; i++) {
			System.out.print("Duration(ms)\t");
			System.out.print("Computed\t");
		}
		System.out.println();
		//
		// Output Details
		for (int i = 0; i < sumProducts.length; i++) {
			System.out.print(Expressions.makeSymbol(sizes[i]));
			System.out.print("\t");
			
			for (int j = 0; j < interpreters.length; j++) {
				System.out.print(results.get(i).get(j).first);
				System.out.print("\t");
				System.out.print(results.get(i).get(j).second);
				System.out.print("\t");				
			}
						
			System.out.println(sumProducts[i]);
		}
		
		// 
		// Output details per sum-product
		for (int i = 0; i < sumProducts.length; i++) {
			System.out.println();
			System.out.print(sumProducts[i]);
			System.out.print("\tSize = \t");
			System.out.println(Expressions.makeSymbol(sizes[i]));
			System.out.println("Method\tDuration(ms)\tComputed");
			for (int j = 0; j < interpreters.length; j++) {
				System.out.print(interpreters[j].getName());
				System.out.print("\t");
				System.out.print(results.get(i).get(j).first);
				System.out.print("\t");
				System.out.print(results.get(i).get(j).second);
				System.out.println();				
			}
		}
	}
	
	private Pair<Long, Expression> evaluate(Expression sumProduct, SumProductInterpreter sumProductInterpreter) {
		long start = System.currentTimeMillis();
		
		Expression evalResult = sumProductInterpreter.apply(sumProduct, context);
		
		long duration = System.currentTimeMillis() - start;
		
		Pair<Long, Expression> result = new Pair<>(duration, evalResult);
		
		return result;
	}
	
	private Rational computeSize(Expression functionOnIntensionalSet) {
		Rational result = new Rational(1);
		
		result = computeSize(functionOnIntensionalSet, result, context);
		
		return result;
	}
	
	private Rational computeSize(Expression functionOnIntensionalSet, Rational resultSoFar, Context context) {
		IntensionalSet      intensionalSet      = (IntensionalSet)functionOnIntensionalSet.get(0);
		IndexExpressionsSet indexExpressionsSet = intensionalSet.getIndexExpressions();		
		List<Expression> indices                = IndexExpressions.getIndices(indexExpressionsSet);
		if (indices.size() != 1) {
			throw new UnsupportedOperationException("Currently only support singular indices");
		}
		Expression index              = indices.get(0);
		Context intensionalSetContext = (Context) GrinderUtil.extendRegistryWithIndexExpressions(indexExpressionsSet, context);
		Type type                     = GrinderUtil.getType(index, intensionalSetContext);
		
		Rational result = resultSoFar.multiply(type.cardinality().rationalValue());
		
		Expression head = intensionalSet.getHead();
		if (Expressions.isFunctionApplicationWithArguments(head) && Sets.isIntensionalSet(head.get(0))) {
			result = computeSize(head, result, intensionalSetContext);
		}
		return result;
	}
	
	private class BruteForceSumProductInterpreter implements SumProductInterpreter {
		private BruteForceCommonInterpreter bruteForceInterpreter = new BruteForceCommonInterpreter();
		
		@Override
		public String getName() {
			return "Brute Force";
		}
		
		@Override
		public Expression apply(Expression sumProduct, Context context) {
			Expression result = bruteForceInterpreter.apply(sumProduct, context);
			return result;
		}
	}
	
	private class InversionSumProductInterpreter implements SumProductInterpreter {
		private InversionSimplifier inversionSimplifier = new InversionSimplifier();
		
		@Override
		public String getName() {
			return "Inversion";
		}
		
		@Override
		public Expression apply(Expression sumProduct, Context context) {
			Expression possiblyInverted = inversionSimplifier.apply(sumProduct, context);
			Expression result           = context.getTheory().evaluate(possiblyInverted, context);
			return result;
		}
	}
	
	private class SamplingSumProductInterpreter implements SumProductInterpreter {
		private SampleCommonInterpreter samplingInterpreter;
		private int n;
		
		public SamplingSumProductInterpreter(int n) {
			this.n = n;
			samplingInterpreter = new SampleCommonInterpreter(n, false, new Random(1));
		}
		
		@Override
		public String getName() {
			return "Sampling[N="+n+"]";
		}
		
		@Override
		public Expression apply(Expression sumProduct, Context context) {
			Expression result = samplingInterpreter.apply(sumProduct, context);
			return result;
		}
	}
}
