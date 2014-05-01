package com.sri.ai.grinder.core;

import java.util.Collections;
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
public class OrderNormalize extends AbstractRewriter {

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
			
			Expression cached = cache.get(new IdentityWrapper(expression));
			if (cached == null) {

				List<Expression> arguments = expression.getArguments();

				List<Expression> newArguments;
				newArguments = Util.mapIntoArrayList(arguments, new OrderNormalizeFunction(process));
				
				Collections.sort(newArguments, null); // FIXME: NEED TO ADD COMPARATOR BECAUSE EXPRESSION IS NO LONGER COMPARABLE
				
				Expression original = expression;
				if ( ! Util.equals(newArguments, arguments)) {
					expression = Expressions.apply(functor, newArguments);
					// System.out.println("\nOrder-normalized:\n" + original +  "\n" + expression);
				}
				cache.put(new IdentityWrapper(original), expression);
			}
			else {
				expression = cached;
			}
		}
		return expression;
	}
	
	public boolean equalsUpToOrderNormalization(Expression expression1, Expression expression2, RewritingProcess process) {
		boolean result;
		if (isSymmetric(expression1.getFunctor(), process) && isSymmetric(expression2.getFunctor(), process)) {
			Expression normalized1 = orderNormalize(expression1, process);
			Expression normalized2 = orderNormalize(expression2, process);
			result = normalized1.equals(normalized2);
		}
		else {
			result = expression1.equals(expression2);
		}
		return result;
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
}
