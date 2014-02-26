package com.sri.ai.grinder.core;

import java.util.Arrays;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.function.SymmetricModule;
import com.sri.ai.util.Util;

/**
 * Returns an equivalent expression by sorting arguments of commutative-associative functions,
 * thus order-normalizing them.
 * 
 * @author braz
 *
 */
public class OrderNormalize extends AbstractRewriter {

//	private final static List<String> symmetricFunctorStrings = Util.list(
//			FunctorConstants.AND,
//			FunctorConstants.OR,
//			FunctorConstants.PLUS,
//			FunctorConstants.PRODUCT,
////			FunctorConstants.EQUALITY,
////			FunctorConstants.DISEQUALITY,
//			FunctorConstants.EQUIVALENCE
//			);

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = orderNormalize(expression, process);
		return result;
	}

	public static Expression orderNormalize(Expression expression, RewritingProcess process) {
		Expression functor = expression.getFunctor();
		if (isSymmetric(functor, process)) {
			List<Expression> arguments = expression.getArguments();
			Object[] newArguments = arguments.toArray(); // no need for recursion, sorting of arguments will normalize recursively.
//			Expression[] newArguments = Util.mapIntoArray(arguments, orderNormalizeFunction, new Expression[numberOfArguments]);
//			int numberOfArguments = expression.numberOfArguments();
			Arrays.sort(newArguments);
			if ( ! Util.equals(newArguments, arguments)) {
				// Expression original = expression;
				expression = Expressions.apply(functor, (Object[]) newArguments);
				// System.out.println("\nOrder-normalized:\n" + original +  "\n" + expression);
			}
		}
		return expression;
	}

//	private static Function<Expression,Expression> orderNormalizeFunction = new Function<Expression, Expression>() {
//		@Override
//		public Expression apply(Expression input) {
//			Expression result = orderNormalize(input);
//			return result;
//		}
//	};

	private static boolean isSymmetric(Expression functor, RewritingProcess process) {
		boolean result = false;
		if (functor != null && process != null) {
			SymmetricModule symmetricModule = (SymmetricModule) process.findModule(SymmetricModule.class);
			if (symmetricModule != null) {
				result = symmetricModule.isSymmetric(functor, process);
			}
		}
		return result;
	}
	
}
