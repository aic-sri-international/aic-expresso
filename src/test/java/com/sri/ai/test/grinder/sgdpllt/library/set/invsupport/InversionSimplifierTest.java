package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.InversionSimplifier;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

public class InversionSimplifierTest {
	private Context context;
	private InversionSimplifier simplifier;
	
	@Before
	public void setUp() {
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(false, false), 
						new TupleTheory()));
		simplifier = new InversionSimplifier();
	}
	
	@Ignore
	@Test
	public void testCase1() {
		Expression summation = parse("sum({{(on f in 1..10 -> 1..5) product({{(on X in 1..10) f(X) : true }}) : true}})");
	    Expression product  = parse("product({{(on X in 1..10) sum({{(on f in 1..10 -> 1..5) f(X) }}) : true}})");
		
	    IntensionalSet intensionalSet = (IntensionalSet) product.get(0);
	    Context intensionalSetContext  = (Context) GrinderUtil.extendRegistryWithIndexExpressions(intensionalSet.getIndexExpressions(), context);
	    Expression sum = intensionalSet.getHead();
	    intensionalSet = (IntensionalSet) sum.get(0);
	    intensionalSetContext = (Context) GrinderUtil.extendRegistryWithIndexExpressions(intensionalSet.getIndexExpressions(), intensionalSetContext);
	    
	    Type typeX = GrinderUtil.getType(parse("f"), intensionalSetContext);
System.out.println("typeX="+typeX);	    

Expression res = context.getTheory().evaluate(product, context);
System.out.println("res="+res);
	    Assert.assertEquals(
				product, 
				simplifier.apply(summation, context));
	}
}
