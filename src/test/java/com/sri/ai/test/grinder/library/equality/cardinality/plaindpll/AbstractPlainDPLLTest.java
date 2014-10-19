package com.sri.ai.test.grinder.library.equality.cardinality.plaindpll;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getAllVariables;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.makeIndexExpression;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.toArrayList;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;

public abstract class AbstractPlainDPLLTest {

	protected abstract Rewriter makeRewriter();

	protected abstract Expression makeProblem(Expression expression, List<Expression> indexExpressions);

	private static final Expression everythingType = makeSymbol("Everything");

	private static final Expression everythingCardinality = apply(CARDINALITY, everythingType);

	protected static class Parse implements Function<String, Expression> {
		@Override
		public Expression apply(String input) {
			return parse(input);
		}
	}

	protected void runTest(Expression expression, Collection<String> indicesStrings, Expression expected) {
		DefaultRewritingProcess process = new DefaultRewritingProcess(expression, null);
		
		Collection<Expression> indices;
		if (indicesStrings != null) {
			indices = mapIntoArrayList(indicesStrings, new Parse());
		}
		else {
			indices = getAllVariables(expression, process);
		}
		
		process.putGlobalObject(everythingCardinality, makeSymbol(10));
		
		List<Expression> indexExpressions =
				indices
				.stream()
				.map(index -> makeIndexExpression(index, everythingType))
				.collect(toArrayList(indices.size()));
		
		Rewriter rewriter = makeRewriter();
		Expression problem = makeProblem(expression, indexExpressions);
		System.out.println("Problem: " + problem);
		RewritingProcess subProcess = GrinderUtil.extendContextualSymbols(fromFreeSymbolsToEverything(problem, process), process);
		Expression actual = rewriter.rewrite(problem, subProcess);
		System.out.println("Solution: " + actual);
		System.out.println("Expected: " + expected + "\n");
		Assert.assertEquals(expected, actual);
	}

	private Map<Expression, Expression> fromFreeSymbolsToEverything(Expression expression, RewritingProcess process) {
		Map<Expression, Expression> result = new LinkedHashMap<Expression, Expression>();
		Collection<Expression> freeSymbols = Expressions.freeSymbols(expression, process);
		freeSymbols.forEach(freeSymbol -> result.put(freeSymbol, everythingType));
		return result;
	}
}