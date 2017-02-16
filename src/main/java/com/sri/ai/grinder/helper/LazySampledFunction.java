/*
 * Copyright (c) 2017, SRI International
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
package com.sri.ai.grinder.helper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.interpreter.AbstractIterativeMultiIndexQuantifierElimination;

/**
 * 
 * @author oreilly
 *
 */
public class LazySampledFunction extends AbstractExpressionWrapper {
	private static final long serialVersionUID = 1L;
	
	private Expression innerExpression;
	private FunctionType functionType;
	private Random random;
	private Map<List<Expression>, Expression> applications = new HashMap<>();

	public LazySampledFunction(FunctionType functionType, Random random) {
		this.functionType = functionType;
		this.random       = random;
		
		this.innerExpression = Expressions.parse(functionType.toString());
	}
	
	public Expression sampleApplication(List<Expression> applicationArguments, Context context) {
		List<Expression> applicationArgumentAssignments = new ArrayList<>();
		for (Expression argument : applicationArguments) {
			Expression assignedValue = AbstractIterativeMultiIndexQuantifierElimination.getAssignedValue(argument, context);
			if (assignedValue == null) {
				applicationArgumentAssignments.add(argument);
			}
			else {
				applicationArgumentAssignments.add(assignedValue);
			}
		}
		
		Expression result = applications.get(applicationArgumentAssignments);
		
		if (result == null) {
			result = functionType.getCodomain().sampleUniquelyNamedConstant(random);
			applications.put(applicationArgumentAssignments, result);
		}
		
		// System.out.println("sampled application="+result+" for "+random+" with "+applicationArguments+" = " + applicationArgumentAssignments+ " in "+context);		
		
		return result;
	}
	
	@Override
	protected Expression computeInnerExpression() {
		return innerExpression;
	}	
}
