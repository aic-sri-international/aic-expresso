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
package com.sri.ai.grinder.demo;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.demo.model.EnableItem;
import com.sri.ai.grinder.demo.model.ExampleRewrite;
import com.sri.ai.grinder.demo.model.GroupEnableItem;
import com.sri.ai.grinder.demo.model.LeafEnableItem;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;

@Beta
public class EqualityRewritePanel extends AbstractRewritePanel {
	private static final long serialVersionUID = 1L;

	@Override
	protected ExampleRewrite[] getExampleRewrites() {
		return new ExampleRewrite[] {
			// Equality
			new ExampleRewrite("Equality: 1 = 1", "1 = 1"),
			new ExampleRewrite("Equality: 1 = 2", "1 = 2"),
			new ExampleRewrite("Equality: A = A", "A = A"),
			new ExampleRewrite("Equality: A = B", "A = B"),
			new ExampleRewrite("Equality: a = b", "a = b"),
			// Disequality
			new ExampleRewrite("Disequality: 1 != 1", "1 != 1"),
			new ExampleRewrite("Disequality: 1 != 2", "1 != 2"),
			new ExampleRewrite("Disequality: A != A", "A != A"),
			new ExampleRewrite("Disequality: A != B", "A != B"),
			new ExampleRewrite("Disequality: a != b", "a != b"),
		};
	}
	
	@Override
	protected EnableItem<Rewriter> getExampleRewriters() {

		List<EnableItem<Rewriter>> equalityRewriters = new ArrayList<EnableItem<Rewriter>>();
		equalityRewriters.add(new LeafEnableItem<Rewriter>("Equality",  new Equality()));
		equalityRewriters.add(new LeafEnableItem<Rewriter>("Disequality",  new Disequality()));
		GroupEnableItem<Rewriter> equalityGroup = new GroupEnableItem<Rewriter>("Equality", equalityRewriters);
		
		List<EnableItem<Rewriter>> groups = new ArrayList<EnableItem<Rewriter>>();
		groups.add(equalityGroup);
		GroupEnableItem<Rewriter> root = new GroupEnableItem<Rewriter>("Equality Rewriters", groups);
				
		return root; 
	}
}
