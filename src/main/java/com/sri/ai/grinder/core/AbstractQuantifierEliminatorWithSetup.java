/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.grinder.core;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.QuantifierEliminator;
import com.sri.ai.grinder.api.QuantifierEliminatorWithSetup;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * A {@link QuantifierEliminator} offering methods for setup information
 * (symbol names and types, and type sizes).
 * <p>
 * Extensions to this class must routinely invoke {@link #checkInterrupted()}
 * during its operation so as to allow interruption ordered by client code.
 * 
 * @author braz
 *
 */
public abstract class AbstractQuantifierEliminatorWithSetup implements QuantifierEliminatorWithSetup {

	private boolean interrupted = false;
	private boolean debug = false;
	private RewritingProcess topLevelRewritingProcess;

	//// Abstract methods
 
	/**
	 * Returns a true constraint for a problem with given indices.
	 * @param indices
	 * @return
	 */
	protected abstract Constraint makeTrueConstraint(Collection<Expression> indices);
	
//	/**
//	 * Makes an appropriate rewriting process with the given data for SGDPLL2, which does not require a contextual constraint.
//	 * @param mapFromSymbolNameToTypeName
//	 * @param mapFromCategoricalTypeNameToSizeString
//	 * @param additionalTypes
//	 * @param isUniquelyNamedConstantPredicate
//	 * @return
//	 */
//	protected abstract RewritingProcess makeProcess(
//			Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString,
//			Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate);

	//// Implemented methods
	
	@Override
	public void interrupt() {
		interrupted = true;
		if (topLevelRewritingProcess != null) {
			topLevelRewritingProcess.interrupt();
		}
	}
	
	/**
	 * Extensions must periodically invoke this method, so algorithm stops if so ordered by user.
	 * @return 
	 */
	public void checkInterrupted() {
		if (interrupted) {
			throw new RuntimeException("Solver Interrupted");
		}
	}
	
	@Override
	public boolean getDebug() {
		return debug;
	}

	@Override
	public void setDebug(boolean newDebugValue) {
		debug = true;
	}

	@Override
	public Expression solve(Expression input, Collection<Expression> indices, RewritingProcess process) {
		Constraint constraint = makeTrueConstraint(indices);
		Expression result = solve(indices, constraint, input, process);
		return result;
	}
}