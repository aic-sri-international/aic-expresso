package com.sri.ai.grinder.library.equality;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.AbstractModuleNoOpRewriter;
import com.sri.ai.grinder.api.Module;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * This module concentrates the functionality for registering and using pieces
 * of knowledge that can tell whether or not two expressions are not equal to
 * each other cheaply (e.g. different length tuples are not equal).
 * 
 * @author oreilly
 *
 */
@Beta
public class CheapDisequalityModule extends AbstractModuleNoOpRewriter {

	public static interface Provider extends Module.Provider {
		
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
	
	/**
	 * Registers a {@link Provider} in the {@link CheapDisequalityModule} module of the given process,
	 * or throw an error if there is not one.
	 */
	public static void register(Provider provider, RewritingProcess process) throws Error {
		register(CheapDisequalityModule.class, provider, process);
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
		for (Module.Provider moduleProvider : providers.keySet()) {
			Provider provider = (Provider) moduleProvider;
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
