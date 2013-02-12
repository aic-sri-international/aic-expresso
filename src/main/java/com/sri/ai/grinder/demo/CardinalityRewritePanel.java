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
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityWrapper;
@Beta
public class CardinalityRewritePanel extends AbstractRewritePanel {
	private static final long serialVersionUID = 1L;

	@Override
	protected ExampleRewrite[] getExampleRewrites() {
		return new ExampleRewrite[] {
			// Basic
			new ExampleRewrite("#1: | X = a |_X", "| {(on X) tuple(X) | X = a } |"),
			new ExampleRewrite("#2: | X != a |_X", "| {(on X) tuple(X) | X != a } |"),
			new ExampleRewrite("#3: | Y = b |_X,Y", "| {(on X, Y) tuple(X, Y) | Y = b } |"),
			new ExampleRewrite("#4: | Y != b |_X,Y", "| {(on X, Y) tuple(X, Y) | Y != b } |"),
			new ExampleRewrite("#5: | true |_X,Y", "| {(on X, Y) tuple(X, Y) | true } |"),
			new ExampleRewrite("#6: | false |_X,Y", "| {(on X, Y) tuple(X, Y) | false } |"),
			new ExampleRewrite("#7: | Z = c |", "| {(on ) tuple() | Z = c } |"),
			new ExampleRewrite("#8: | X = a |", "| {(on ) tuple() | X = a } |"),
			new ExampleRewrite("#9: there exists", "| {(on Y) tuple(Y) | there exists X : X = Y and Z = a } |"),
		};
	}
	
	@Override
	protected EnableItem<Rewriter> getExampleRewriters() {

		List<EnableItem<Rewriter>> cardinalityRewriters = new ArrayList<EnableItem<Rewriter>>();
		cardinalityRewriters.add(new LeafEnableItem<Rewriter>("Cardinality",  new CardinalityWrapper()));

		GroupEnableItem<Rewriter> root = new GroupEnableItem<Rewriter>("Cardinality Rewriters", cardinalityRewriters);
				
		return root; 
	}
}
