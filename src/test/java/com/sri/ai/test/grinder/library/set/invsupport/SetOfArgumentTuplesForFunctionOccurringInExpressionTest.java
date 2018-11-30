package com.sri.ai.test.grinder.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.invsupport.SetOfArgumentTuplesForFunctionOccurringInExpression;

public class SetOfArgumentTuplesForFunctionOccurringInExpressionTest {
	
	private Expression fName;
	private FunctionType fType;
	
	@Before
	public void setUp() {
		fName = parse("f");
		fType = new FunctionType(new IntegerInterval("1..5"), new IntegerInterval("1..10"), new IntegerInterval("11..20"));
	}
	
	@Test // if E does not contain &fnof;, oc<sub>&fnof;</sub>[E] is &empty;
	public void testCase1() {
		Expression e = parse("a");
		Assert.assertEquals(Sets.EMPTY_SET, getSetOfArgumentTuples(e));
		
		e = parse("if A = 1 then 4 else 5");
		Assert.assertEquals(Sets.EMPTY_SET, getSetOfArgumentTuples(e));
	}
	
	@Test // if E is &fnof;(t) for t a tuple, oc<sub>&fnof;</sub>[E] is {t}
	public void testCase2() {
		Expression e = parse("f(1,2)");
		Assert.assertEquals(parse("{(1,2)}"), getSetOfArgumentTuples(e));		
	}
	
	@Test // if E is &fnof;, oc<sub>&fnof;</sub>[E] is &Alpha;
	public void testCase3() {
		Expression e = parse("f");
		Assert.assertEquals(parse("{(1..10,11..20)}"), getSetOfArgumentTuples(e));
	}
	
	@Test // if E is g(E&prime;) for g a function symbol distinct from &fnof; and t a k-tuple of expressions
	public void testCase4() {
		// then oc<sub>&fnof;</sub>[E] is<br>
		// if C then oc<sub>&fnof;</sub>[E<sub>1</sub>] else oc<sub>&fnof;</sub>[E<sub>2</sub>]		
		Expression e = parse("if X = 2 then f(1,2) else f(3,4)");
		Assert.assertEquals(parse("if X = 2 then {(1,2)} else {(3,4)}"), getSetOfArgumentTuples(e));
		
		e = parse("if X = 2 then 4 else f(3,4)");
		Assert.assertEquals(parse("if X = 2 then {} else {(3,4)}"), getSetOfArgumentTuples(e));
		
		// otherwise, oc<sub>&fnof;</sub>[E] is 
		// oc<sub>&fnof;</sub>[t<sub>1</sub>] &cup; &hellip;	&cup; oc<sub>&fnof;</sub>[t<sub>k</sub>]
		e = parse("if f(1,2) = 3 then f(3,4) else f(5,6)");
		Assert.assertEquals(parse("{(1,2)} union {(3,4)} union {(5,6)}"), getSetOfArgumentTuples(e));
		
		e = parse("f(1,2) + f(3,4)");
		Assert.assertEquals(parse("{(1,2)} union {(3,4)}"), getSetOfArgumentTuples(e));
	}
	
	@Test
	// if E is Q<sub>x:C</sub>E&prime; for Q an arbitrary quantifier, x a variable, 
	// C a boolean formula not containing &fnof;, and E&prime; the
	// quantified expression,then oc<sub>&fnof;</sub>[E] is<br> 
	// &bigcup;<sub>x:C</sub>oc<sub>&fnof;</sub>[E&prime;]
	public void testCase5() {
		Expression e = parse("{ (on X in 1..10) f(X,2) : X != 4 }");
		Assert.assertEquals(parse("Union({{(on X in 1..10) {(X,2)} : X != 4}})"), getSetOfArgumentTuples(e));
		
		e = parse("{{ (on X in 1..10) f(X,2) : X != 4 }}");
		Assert.assertEquals(parse("Union({{(on X in 1..10) {(X,2)} : X != 4}})"), getSetOfArgumentTuples(e));
		
		e = parse("| X in 1..10 : X != 4 |");
		Assert.assertEquals(parse("{}"), getSetOfArgumentTuples(e));
		
		e = parse("for all X in 1..10 : f(X,2) = 3");
		Assert.assertEquals(parse("Union({{(on X in 1..10) {(X,2)} }})"), getSetOfArgumentTuples(e));
		
		e = parse("there exists X in 1..10 : f(X,2) = 3");
		Assert.assertEquals(parse("Union({{(on X in 1..10) {(X,2)} }})"), getSetOfArgumentTuples(e));
		
		e = parse("lambda X in 1..10 : f(X,2) = 3");
		Assert.assertEquals(parse("Union({{(on X in 1..10) {(X,2)} }})"), getSetOfArgumentTuples(e));
	}
	
	@Test
	// if E is Q<sub>x:C</sub>E&prime; for Q an arbitrary quantifier, x a variable, 
	// C a boolean formula containing &fnof;, and E&prime; the
	// quantified expression, then oc<sub>&fnof;</sub>[E] is<br> 
	// &bigcup;<sub>x</sub>(oc<sub>&fnof;</sub>[C] &cup; oc<sub>&fnof;</sub>[E&prime;])
	public void testCase6() {
		Expression e = parse("{ (on X in 1..10) f(X,2) : f(X,3) != 4 }");
		Assert.assertEquals(parse("Union({{(on X in 1..10) {(X,3)} union {(X,2)} }})"), getSetOfArgumentTuples(e));
		
		e = parse("{{ (on X in 1..10) f(X,2) : f(X,3) != 4 }}");
		Assert.assertEquals(parse("Union({{(on X in 1..10) {(X,3)} union {(X,2)} }})"), getSetOfArgumentTuples(e));
		
		e = parse("| X in 1..10 : f(X,3) != 4 |");
		Assert.assertEquals(parse("Union({{(on X in 1..10) {(X,3)} }})"), getSetOfArgumentTuples(e));
	}
	
	@Test
	public void testExampleFromIJCAII2017Paper() {
		Expression e = parse("f(X,Y) + sum({{ (on Z in 11..20) f(X,Z)*product({{(on W in 1..10) f(W,Z)}}) :  Z != 3 }})");
		
		Assert.assertEquals(
				parse("{ (X, Y) } union Union({{ ( on Z in 11..20 ) { (X, Z) } union Union({{ ( on W in 1..10 ) { (W, Z) } }}) : Z != 3 }})"), 
				getSetOfArgumentTuples(e));
	}
	
	private Expression getSetOfArgumentTuples(Expression e) {
		Expression result = SetOfArgumentTuplesForFunctionOccurringInExpression.compute(fName, fType, e);
		return result;
	}
}