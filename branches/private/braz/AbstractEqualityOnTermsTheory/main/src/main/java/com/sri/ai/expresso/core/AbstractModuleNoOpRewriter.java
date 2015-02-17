package com.sri.ai.expresso.core;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * An {@link AbstractModuleAndPossibleActiveRewriter} that also implements {@link NoOpRewriter} to indicate that it implements rewriters that perform no operation.
 *  
 * @author braz
 *
 */
public abstract class AbstractModuleNoOpRewriter extends AbstractModuleAndPossibleActiveRewriter implements NoOpRewriter  {

	@Override
	/* This will eventually be removed when we introduce mechanism to deal with modules that are not rewriters. */
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression;
	}
}
