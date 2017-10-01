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
package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import com.sri.ai.grinder.sgdpllt.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.CombiningTopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.help.CompleteRewriter;

public class SetDNFRewriter extends CompleteRewriter {
	public SetDNFRewriter() {
		super(createTopRewriter());
	}
	
	private static TopRewriter createTopRewriter() {
		// Original Rewriters		
		TopRewriter result = new CombiningTopRewriter(
				"SetDNF",
				new IntensionalUnionToUnionsOfIntensionalSetsOfBaseTypeTopRewriter(),
				new BooleanSimplifier(), // NOTE: added to simplify expressions like `if true then { (2, 2) } else {  }', which are common in this setup
				new UnionEmptySetTopRewriter(),
				new IntersectionEmptySetTopRewriter(),
				new IntensionalSetToConditionalTopRewriter(),
				new IntersectionIntensionalSetsTopRewriter(),
				new IntersectionExtensionalSetTopRewriter(),
				new DistributeIntersectionOverUnionTopRewriter());
		
		// Rewriters based on UAI 2017 Paper
//		TopRewriter topRewriter = new DefaultTopRewriter(
//			new TupleEqualityTopRewriter(),                               // Rule 1		
//			new ElementOfExtensionalSetTopRewriter(),                     // Rule 2
//			new ElementOfIntensionalUnionTopRewriter(),                   // Rule 3
//			new IntensionalUnionEqualToEmptySetTopRewriter(),             // Rule 4
//			new IntersectionEmptySetTopRewriter(),                        // Rule 5			
//			new UnionEmptySetTopRewriter(),                               // Rule 6			
//			new SetIntersectExtensionalSetEqualToEmptySetTopRewriter(),   // Rule 7
//			new IntensionalUnionIntersectionEqualToEmptySetTopRewriter(), // Rule 8
//			new DistributeIntersectionOverUnionTopRewriter(),             // Rule 9
//			new UnionOfSetsEqualEmptySetTopRewriter(),                    // Rule 10
//			new ExtensionalSetEqualEmptySetTopRewriter(),                 // Rule 11
//			new BooleanSimplifier() // NOTE: added to simplify expressions like `if true then { (2, 2) } else {  }', which are common in this setup
//			);		
		
		return result;
	}
}
