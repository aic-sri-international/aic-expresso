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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import java.util.Collection;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.util.Util;

/**
 * An example on how to use GSDPLL(T).
 * @author braz
 *
 */
public class HelloWorlds {

	public static void main(String[] args) {

		/*
		 * The theory of equality on symbols (includes a model counter for formulas in it.
		 */
		Theory theory = new EqualityOnSymbolsTheory();

		/* The problem type, a sum. Could be Max or other commutative associative operators. */
//		ProblemType problemType = new Sum();
		ProblemType problemType = new Max();

		/* The expression to be summed (or whatever other operation'ed) - can use equality on symbols (capitalized ones are variables, other uniquely named constants),
		 * boolean connectives, if then else expressions, and numbers.
		 */
		Expression expression  = Expressions.parse("if X = Y or Y = Z then 1 else if Y != a then 2 else 3");
		
		/* the variables to be summed out (or maxed out, etc, depending on the problem type). 
		 * Could be just one of them, or all of them, or none of them */
		Collection<Expression> indices = Util.list(Expressions.parse("X"), Expressions.parse("Y"));
//		Collection<Expression> indices = Util.list(Expressions.parse("X"));
//		Collection<Expression> indices = Util.list(); // result is a compilation of the original expression in this case
//		Collection<Expression> indices = Util.list(Expressions.parse("X"), Expressions.parse("Y"), Expressions.parse("Z")); // sum is just a number in this case

		/* The definitions of variables, types, and type sizes. */
		Map<String, String> mapFromTypeNameToSizeString   = Util.map("Everything", "10000");
		Map<String, String> mapFromVariableNameToTypeName = Util.map("X", "Everything", "Y", "Everything", "Z", "Everything");
		
		/* The solver for the parameters above. */
		DPLLGeneralizedAndSymbolic solver = new DPLLGeneralizedAndSymbolic(theory, problemType);
		
		/* Solve the problem. */
		Expression result = solver.solve(expression, indices, mapFromVariableNameToTypeName, mapFromTypeNameToSizeString);

		System.out.println(problemType.getClass().getSimpleName() + " of\n" + expression + "\nover assignments to variables\n" + Util.join(indices) + "\nis\n" + result);	
	}

}
