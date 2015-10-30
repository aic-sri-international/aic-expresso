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
package com.sri.ai.grinder.api;

import static com.sri.ai.util.Util.list;

import java.util.Collection;
import java.util.Map;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.core.PrologConstantPredicate;

/**
 * A {@link QuantifierEliminator} offering methods for setup information
 * (symbol names and types, and type sizes).
 * 
 * @author braz
 *
 */
public interface QuantifierEliminatorWithSetup extends QuantifierEliminator {

	// TODO: these should be moved to abstract implementation instead of left in the interface (see comments below)
	
	/**
	 * Returns a true constraint for a problem with given indices.
	 * @param indices
	 * @return
	 */
	Constraint makeTrueConstraint(Collection<Expression> indices);
	
	/**
	 * Local simplification of an expression according to the theory used by this solver.
	 * @param expression
	 * @param process
	 * @return
	 */
	Expression simplify(Expression expression, RewritingProcess process);
	
	/**
	 * Returns the additive identity element of the group used by this solver.
	 * @return
	 */
	Expression getAdditiveIdentityElement();
	
	/**
	 * Makes an appropriate rewriting process with the given data.
	 * @param constraint
	 * @param mapFromSymbolNameToTypeName
	 * @param mapFromTypeNameToSizeString
	 * @param isUniquelyNamedConstantPredicate
	 * @return
	 */
	RewritingProcess makeProcess(
			Constraint constraint,
			Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromTypeNameToSizeString,
			Predicate<Expression> isUniquelyNamedConstantPredicate);

	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices and a constraint on them
	 */
	@Override
	Expression solve(Collection<Expression> indices, Constraint constraint, Expression body, RewritingProcess process);

	////////// Convenience methods
	// TODO: These should be left as interface methods, not implemented, once we get rid of PlainDPLL package;
	// They should be in AbstractQuantifierEliminatorWithSetup class, but the classes in that package
	// would require a lot of work to use that because of the lack of multiple inheritance
	// (they are already inheriting from abstract hierarchical rewriters because they
	// are required to implement Rewriter as well.
	
	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices.
	 */
	default Expression solve(Expression input, Collection<Expression> indices, RewritingProcess process) {
		Constraint constraint = makeTrueConstraint(indices);
		Expression result = solve(indices, constraint, input, process);
		return result;
	}

	/**
	 * Convenience substitute for {@link #solve(Expression, Collection, RewritingProcess)} that takes care of constructing the RewritingProcess.
	 */
	default Expression solve(
			Expression expression, Collection<Expression> indices,
			Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromTypeNameToSizeString,
			Predicate<Expression> isUniquelyNamedConstantPredicate) {
		
		RewritingProcess topLevelRewritingProcess =
				makeProcess(makeTrueConstraint(list()),
						mapFromSymbolNameToTypeName, mapFromTypeNameToSizeString,
						isUniquelyNamedConstantPredicate);
		
		Expression result = solve(expression, indices, topLevelRewritingProcess);
		return result;
	}

	/**
	 * Convenience substitute for {@link #solve(Expression, Collection, RewritingProcess)} that takes care of constructing the RewritingProcess.
	 */
	default Expression solve(
			Expression expression, Collection<Expression> indices,
			Map<String, String> mapFromVariableNameToTypeName, Map<String, String> mapFromTypeNameToSizeString) {
		return solve(expression, indices, mapFromVariableNameToTypeName, mapFromTypeNameToSizeString, new PrologConstantPredicate());
	}
}