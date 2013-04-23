package com.sri.ai.test.grinder.library.equality.cardinality.direct;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.equality.cardinality.FormulaToCNF;
import com.sri.ai.test.grinder.AbstractGrinderTest;

public class FormulaToCNFTest extends AbstractGrinderTest {
	
	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}
	
	@Test
	public void test_isCNFLiteral() {
		
		Assert.assertEquals(true, FormulaToCNF.isCNFLiteral(parse("X = a")));
		Assert.assertEquals(true, FormulaToCNF.isCNFLiteral(parse("a = X")));
		Assert.assertEquals(true, FormulaToCNF.isCNFLiteral(parse("X = Y")));
		Assert.assertEquals(true, FormulaToCNF.isCNFLiteral(parse("X = X")));
		
		Assert.assertEquals(true, FormulaToCNF.isCNFLiteral(parse("X != a")));
		Assert.assertEquals(true, FormulaToCNF.isCNFLiteral(parse("a != X")));
		Assert.assertEquals(true, FormulaToCNF.isCNFLiteral(parse("X != Y")));
		Assert.assertEquals(true, FormulaToCNF.isCNFLiteral(parse("X != X")));
		
		Assert.assertEquals(false, FormulaToCNF.isCNFLiteral(parse("X = Y = a")));
	}
	
	@Test
	public void test_isCNFClause() {
		Assert.assertEquals(true, FormulaToCNF.isCNFClause(parse("or(X = a)")));
		Assert.assertEquals(true, FormulaToCNF.isCNFClause(parse("or(X = a, Y = b)")));
		
		Assert.assertEquals(false, FormulaToCNF.isCNFClause(parse("or()")));
		Assert.assertEquals(false, FormulaToCNF.isCNFClause(parse("or(X = a, or(Y = b, Z = c))")));
		
		Assert.assertEquals(false, FormulaToCNF.isCNFClause(parse("and(X = a)")));
		Assert.assertEquals(false, FormulaToCNF.isCNFClause(parse("and(X = a, Y = b)")));				
	}
	
	@Test
	public void test_isInCNF() {
		Assert.assertEquals(true, FormulaToCNF.isInCNF(parse("or(X = a)")));		
		Assert.assertEquals(true, FormulaToCNF.isInCNF(parse("and(or(X = a))")));
		Assert.assertEquals(true, FormulaToCNF.isInCNF(parse("and(or(X = a, Y = b), or(Z = c, W = d))")));
		
		Assert.assertEquals(false, FormulaToCNF.isInCNF(parse("and(or(X = a, or(Z = c, W = d)))")));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void test_convertToCNF_notAFormula() {
		RewritingProcess process = newProcess();

		FormulaToCNF.convertToCNF(parse("or(f(a))"), process);
	}
	
	@Test
	public void test_convertToCNF_OrNormalization() {
		RewritingProcess process = newProcess();

		// or() -> false
		Assert.assertEquals(parse("false"), FormulaToCNF.convertToCNF(parse("or()"), process));
		
		//  or(..., true, ...) -> true
		Assert.assertEquals(parse("true"), FormulaToCNF.convertToCNF(parse("or(X = a, true, Y = b)"), process));
		
		// or(..., false, ...) -> or(..., ...)
		Assert.assertEquals(parse("or(X = a, Y = b)"), FormulaToCNF.convertToCNF(parse("or(X = a, false, Y = b)"), process));

	}
	
	@Test
	public void test_convertToCNF_AndNormalization() {
		RewritingProcess process = newProcess();
		// and() -> true
		Assert.assertEquals(parse("true"), FormulaToCNF.convertToCNF(parse("and()"), process));
		
		// and(..., true, ...) -> and(..., ...)
		Assert.assertEquals(parse("and(or(X = a), or(Y = b))"), FormulaToCNF.convertToCNF(parse("and(or(X = a), true, or(Y = b))"), process));
		
		// and(..., false, ...) -> false
		Assert.assertEquals(parse("false"), FormulaToCNF.convertToCNF(parse("and(or(X = a), false, or(Y = b))"), process));
	}
	
	@Test
	public void test_convertToCNF_EqualityNormalization() {
		RewritingProcess process = newProcess();

// TODO
// X = Y = Z = -> X = Y and Y = Z
		
		// a = X -> X = a
		Assert.assertEquals(parse("or(X = a)"), FormulaToCNF.convertToCNF(parse("or(a = X)"), process));
		
		// a != X -> X != a
		Assert.assertEquals(parse("or(X != a)"), FormulaToCNF.convertToCNF(parse("or(a != X)"), process));
		
		// X = X -> true
		Assert.assertEquals(parse("true"), FormulaToCNF.convertToCNF(parse("or(X = X)"), process));
		Assert.assertEquals(parse("true"), FormulaToCNF.convertToCNF(parse("or(a = a)"), process));
		
		// X != X -> false
		Assert.assertEquals(parse("false"), FormulaToCNF.convertToCNF(parse("or(X != X)"), process));
		Assert.assertEquals(parse("false"), FormulaToCNF.convertToCNF(parse("or(a != a)"), process));
		
		// a = b -> false
		Assert.assertEquals(parse("false"), FormulaToCNF.convertToCNF(parse("or(a = b)"), process));
		
		// a != b -> true
		Assert.assertEquals(parse("true"), FormulaToCNF.convertToCNF(parse("or(a != b)"), process));
		
	}
	
	@Test
	public void test_convertToCNF_AlreadyCNF() {
		
	}
	
	//
	// PRIVATE
	//
	private RewritingProcess newProcess() {
		return DirectCardinalityComputationFactory.newCardinalityProcess(Expressions.TRUE);
	}
}
