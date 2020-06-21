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
package com.sri.ai.grinder.interpreter;

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

public abstract class ContextAssignmentLookup {

	static final public String ASSIGNMENTS_GLOBAL_OBJECTS_KEY = "ASSIGNMENTS_GLOBAL_OBJECTS_KEY";
	
	/**
	 * Obtains the value assignment to a given expression in the binding mechanism stored in the context.
	 * @param expression
	 * @param context
	 * @return
	 */
	public static Expression getAssignedValue(Expression expression, Context context) {
		@SuppressWarnings("unchecked")
		Map<Expression, Expression> assignments = (Map<Expression, Expression>) context.getGlobalObject(ASSIGNMENTS_GLOBAL_OBJECTS_KEY);
		Expression result = assignments == null? null : assignments.get(expression);
		return result;
	}

	public static Context setAssignments(Context context, Map<Expression, Expression> extendedAssignments) {
		return context.putGlobalObject(ASSIGNMENTS_GLOBAL_OBJECTS_KEY, extendedAssignments);
	}

	public static Context setAssignment(Context context, Expression variable, Expression value) {
		@SuppressWarnings("unchecked")
		var assignments = (Map<Expression, Expression>) context.getGlobalObject(ASSIGNMENTS_GLOBAL_OBJECTS_KEY);
		if (assignments == null) {
			assignments = map();
			context = setAssignments(context, assignments);
		}
		assignments.put(variable, value);
		return context;
	}
}