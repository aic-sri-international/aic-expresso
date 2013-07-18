package com.sri.ai.grinder.library.equality;

import java.util.HashSet;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;

/**
 * This module concentrates the functionality for registering and using pieces
 * of knowledge that can tell whether or not two expressions are not equal to
 * each other cheaply (e.g. different length tuples are not equal).
 * 
 * @author oreilly
 *
 */
@Beta
public class CheapDisequalityModule extends AbstractRewriter implements NoOpRewriter {

	private HashSet<Provider> providers = new HashSet<Provider>();
	
	public static interface Provider {
		
		/**
		 * A providers determination if whether or not two expressions are
		 * guaranteed to not be equal to each other.
		 * 
		 * @param e1
		 * @param e2
		 * @param process
		 *            the current rewriting process.
		 * @return true if the two expressions are guaranteed to not be equal to
		 *         each other, false otherwise. Note: a return value of false
		 *         only means that it cannot be determined cheaply if the two
		 *         expressions are not equal to each other - they may or may not
		 *         be.
		 */
		boolean isCheapDisequality(Expression e1, Expression e2, RewritingProcess process);
	}
	
	public void register(Provider provider) {
		providers.add(provider);
	}
	
	@Override
	/* This will eventually be removed when we introduce mechanism to deal with modules. */
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression;
	}
	
	@Override
	public void rewritingProcessFinalized(RewritingProcess process) {
		providers.clear();
	}
	
	/**
	 * Determine (cheaply) if two expressions are guaranteed to not be equal to
	 * each other (i.e. e1 != e2).
	 * 
	 * @param e1
	 * @param e2
	 * @param process
	 *            the current rewriting process.
	 * @return true if the two expressions are guaranteed to not be equal to
	 *         each other, false otherwise. Note: a return value of false only
	 *         means that it cannot be determined cheaply if the two expressions
	 *         are not equal to each other - they may or may not be.
	 */
	public boolean isCheapDisequality(Expression e1, Expression e2, RewritingProcess process) {
		boolean result = false;
		for (Provider provider : providers) {
			result = provider.isCheapDisequality(e1, e2, process);
			if (result) {
				// The provider guarantees: e1 != e2
				break;
			}
		}
		return result;
	}
	
	/**
	 * Convenience method for determining cheap disequality.
	 * 
	 * @see #isCheapDisequality(Expression, Expression, RewritingProcess)
	 */
	public static boolean isACheapDisequality(Expression e1, Expression e2, RewritingProcess process) {
		boolean result = false;
		
		CheapDisequalityModule cheapDisequalityModule = (CheapDisequalityModule) process.findModule(CheapDisequalityModule.class);
		if (cheapDisequalityModule != null) {
			result = cheapDisequalityModule.isCheapDisequality(e1, e2, process);
		}
	
		return result;
	}
	
}
