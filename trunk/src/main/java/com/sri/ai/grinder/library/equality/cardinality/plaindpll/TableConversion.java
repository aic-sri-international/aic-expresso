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
 * An example on how to use SGDPLL(T) to convert table representations to decision tree representations.
 * @author braz
 *
 */
public class TableConversion {

	public static void main(String[] args) {

		/*
		 * The theory of equality on symbols (includes a model counter for formulas in it).
		 */
		Theory theory = new EqualityTheory(new SymbolTermTheory());

		ProblemType problemType = new Max(); // the problem type actually does not matter, because we are not going to have any supportedIndices.

		Expression table = Expressions.parse(""
						+ "if X = a and Y = a and Z = a then 0.1 else "
						+ "if X = a and Y = a and Z = b then 0.1 else "
						+ "if X = a and Y = a and Z = c then 0.1 else "
						+ "if X = a and Y = b and Z = a then 0.1 else "
						+ "if X = a and Y = b and Z = b then 0.1 else "
						+ "if X = a and Y = b and Z = c then 0.1 else "
						+ "if X = a and Y = c and Z = a then 0.1 else "
						+ "if X = a and Y = c and Z = b then 0.1 else "
						+ "if X = a and Y = c and Z = c then 0.1 else "
						+ "if X = b and Y = a and Z = a then 0.2 else "
						+ "if X = b and Y = a and Z = b then 0.2 else "
						+ "if X = b and Y = a and Z = c then 0.2 else "
						+ "if X = b and Y = b and Z = a then 0.2 else "
						+ "if X = b and Y = b and Z = b then 0.2 else "
						+ "if X = b and Y = b and Z = c then 0.2 else "
						+ "if X = b and Y = c and Z = a then 0.2 else "
						+ "if X = b and Y = c and Z = b then 0.2 else "
						+ "if X = b and Y = c and Z = c then 0.2 else "
						+ "if X = c and Y = a and Z = a then 0.3 else "
						+ "if X = c and Y = a and Z = b then 0.3 else "
						+ "if X = c and Y = a and Z = c then 0.3 else "
						+ "if X = c and Y = b and Z = a then 0.3 else "
						+ "if X = c and Y = b and Z = b then 0.3 else "
						+ "if X = c and Y = b and Z = c then 0.3 else "
						+ "if X = c and Y = c and Z = a then 0.3 else "
						+ "if X = c and Y = c and Z = b then 0.3 else "
						+  /* X = c and Y = c and Z = c */  "0.3"); 

		Collection<Expression> indices = Util.list(); // no supportedIndices; we want to keep all variables

		/* The definitions of variables, types, and type sizes. */
		Map<String, String> mapFromTypeNameToSizeString   = Util.map("Everything", "3");
		Map<String, String> mapFromVariableNameToTypeName = Util.map("X", "Everything", "Y", "Everything", "Z", "Everything");
		
		/* The solver for the parameters above. */
		SGDPLLT solver = new SGDPLLT(theory, problemType);
		
		/* Solve the problem. */
		Expression result = solver.solve(table, indices, mapFromVariableNameToTypeName, mapFromTypeNameToSizeString);

		System.out.println("Decision tree for table is:\n" + result);	
	}

}
