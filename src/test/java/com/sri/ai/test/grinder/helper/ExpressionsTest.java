package com.sri.ai.test.grinder.helper;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.util.Util;

public class ExpressionsTest extends AbstractGrinderTest {
	
	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}

	@Override
	public RewritingProcess makeRewritingProcess(Expression topExpression) {
		return new DefaultRewritingProcess(topExpression, new Basic());
	}
	
	@Ignore("NEED TO RESOLVE Issue 35: Defect in Expressions.freeSymbols logic")
	@Test
	public void testFreeSymbols() {
		RewritingProcess process = makeRewritingProcess(parse("true"));
				
		Expression e = parse("{(on X) X} and Y");
		Assert.assertEquals(Util.set(parse("and"), parse("Y")), Expressions.freeSymbols(e, process));
		
		e = parse("{(on X) X} and X");
		Assert.assertEquals(Util.set(parse("and"), parse("X")), Expressions.freeSymbols(e, process));
	}

}
