package com.sri.ai.grinder.core;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.IdentityWrapper;
import com.sri.ai.util.cache.DefaultCacheMap;

/**
 * Returns an equivalent expression by sorting arguments of commutative-associative functions,
 * thus order-normalizing them.
 * 
 * @author braz
 *
 */
public class OrderNormalize extends AbstractRewriter implements Comparator<Expression> {

	private static Map<IdentityWrapper, Expression> cache = new DefaultCacheMap<IdentityWrapper, Expression>(3000);

	private final static List<String> symmetricFunctorStrings = Util.list(
			FunctorConstants.AND,
			FunctorConstants.OR,
			FunctorConstants.PLUS,
			FunctorConstants.PRODUCT,
//			FunctorConstants.EQUALITY,
//			FunctorConstants.DISEQUALITY,
			FunctorConstants.EQUIVALENCE
			);

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = orderNormalize(expression, process);
		return result;
	}

	public static Expression orderNormalize(Expression expression, RewritingProcess process) {
		Expression functor = expression.getFunctor();
		if (isSymmetric(functor, process)) {
			
			IdentityWrapper originalCacheKey = new IdentityWrapper(expression);
			Expression cached = cache.get(originalCacheKey);
			if (cached == null) {

				List<Expression> arguments = expression.getArguments();

				List<Expression> newArguments;
				newArguments = Util.mapIntoArrayList(arguments, new OrderNormalizeFunction(process));
				Collections.sort(newArguments);
				
				// at this point, newArguments is a distinct instance from arguments because we needed to sort it,
				// but it may be identical otherwise.
				// If it is identical, its elements will not only be equal, but they will be the *same instances* as the original,
				// so we check for that, which is cheaper.
				if (! Util.sameInstancesInSameIterableOrder(newArguments, arguments)) {
					expression = Expressions.apply(functor, newArguments);
					// System.out.println("\nOrder-normalized:\n" + original +  "\n" + expression);
				}
				// else, no change in arguments and no need to create a new instance
				
				cache.put(originalCacheKey, expression);
			}
			else {
				expression = cached;
			}
		}
		return expression;
	}
	
	private static boolean isSymmetric(Expression functor, RewritingProcess process) {
		boolean result = false;
		
		if (functor != null) {
			result = symmetricFunctorStrings.contains(functor.toString());
		}

//		if (functor != null && process != null) {
//			SymmetricModule symmetricModule = (SymmetricModule) process.findModule(SymmetricModule.class);
//			if (symmetricModule != null) {
//				result = symmetricModule.isSymmetric(functor, process);
//			}
//		}

		return result;
	}

	private static class OrderNormalizeFunction implements Function<Expression,Expression> {
		private RewritingProcess process;
	
		public OrderNormalizeFunction(RewritingProcess process) {
			super();
			this.process = process;
		}
	
		@Override
		public Expression apply(Expression input) {
			Expression result = orderNormalize(input, process);
			return result;
		}
	}

	@Override
	public int compare(Expression o1, Expression o2) {
		Expression normalizedO1 = orderNormalize(o1, Expressions.getProcess());
		Expression normalizedO2 = orderNormalize(o2, Expressions.getProcess());
		int result = normalizedO1.compareTo(normalizedO2);
		return result;
	}
}
