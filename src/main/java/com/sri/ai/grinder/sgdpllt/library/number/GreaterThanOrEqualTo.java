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
package com.sri.ai.grinder.sgdpllt.library.number;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.greaterThanOrEqualTo;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * @author braz
 */
@Beta
public class GreaterThanOrEqualTo  implements Simplifier {

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}
	
	/**
	 * Receives an application of {@link FunctorConstants.GREATER_THAN_OR_EQUAL_TO} and evaluates it if possible.
	 * @param greaterThanOrEqualToApplication
	 * @param context
	 * @return
	 */
	public static Expression simplify(Expression greaterThanOrEqualToApplication, Context context) {
		Expression result;
		if (greaterThanOrEqualToApplication.get(0).equals(greaterThanOrEqualToApplication.get(1))) {
			result = TRUE; // yes, it is greater than or equal to itself
		}
		else if (isNumber(greaterThanOrEqualToApplication.get(0)) && isNumber(greaterThanOrEqualToApplication.get(1))) {
			result = makeSymbol(greaterThanOrEqualTo(greaterThanOrEqualToApplication.get(0).rationalValue(), greaterThanOrEqualToApplication.get(1).rationalValue()));
		}
		else if ( greaterThanOrEqualToApplication.get(0).equals(MINUS_INFINITY) && !greaterThanOrEqualToApplication.get(1).equals(MINUS_INFINITY)) {
			result = FALSE;
		}
		else if ( greaterThanOrEqualToApplication.get(0).equals(INFINITY)       && !greaterThanOrEqualToApplication.get(1).equals(INFINITY)) {
			result = TRUE;
		}
		else if (!greaterThanOrEqualToApplication.get(0).equals(MINUS_INFINITY) &&  greaterThanOrEqualToApplication.get(1).equals(MINUS_INFINITY)) {
			result = TRUE;
		}
		else if (!greaterThanOrEqualToApplication.get(0).equals(INFINITY)       &&  greaterThanOrEqualToApplication.get(1).equals(INFINITY)) {
			result = FALSE;
		}
		else {
			result = greaterThanOrEqualToApplication;
		}
		return result;
	}
}
