package com.sri.ai.expresso.core;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;

/**
 * An abstract implementation of {@link ReplacementFunctionWithContextuallyUpdatedProcess} that defines {@link #apply(Expression)} to issue an exception about it not being supported (this makes sense because this interface is about replacement functions that take both an expression and a rewriting process as arguments).
 * 
 * @author braz
 * 
 */
abstract public class AbstractReplacementFunctionWithContextuallyUpdatedProcess implements ReplacementFunctionWithContextuallyUpdatedProcess {

	@Override
	public Expression apply(Expression expression) {
		throw new UnsupportedOperationException(getClass() + ".apply(Expression expression) is not defined and should not be invoked.");
	}

}
