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
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;

@Beta
public class ArithmeticRewritePanel extends AbstractRewritePanel {
	private static final long serialVersionUID = 1L;

	@Override
	protected ExampleRewrite[] getExampleRewrites() {
		return new ExampleRewrite[] {
			// Plus
			new ExampleRewrite("Addition: 1 + 2", "1 + 2"),
			new ExampleRewrite("Addition: 1 + x + 2 + y", "1 + x + 2 + y"),
			new ExampleRewrite("Addition: +(x, +(1, y), 11)", "+(x, +(1, y), 11)"),
			// Minus
			new ExampleRewrite("Minus: 3 - 1", "3 - 1"),
			new ExampleRewrite("Minus: 1 - 3", "1 - 3"),
			new ExampleRewrite("Minus: 0 - X", "0 - X"),
			// Times
			new ExampleRewrite("Times: 2 * 2", "2 * 2"),
			new ExampleRewrite("Times: x * 2 * 1 * 2 * 1", "x * 2 * 1 * 2 * 1"),
			// Division
			new ExampleRewrite("Division: 4/2", "4/2"),
			new ExampleRewrite("Division: 2.1/4.2", "2.1/4.2"),
			// Exponentiation
			new ExampleRewrite("Exponentiation: 3^2", "3^2"),
			new ExampleRewrite("Exponentiation: x^0", "x^0"),
			new ExampleRewrite("Exponentiation: 1^4", "1^4"),
			// Mixed.
			new ExampleRewrite("Mixed 1", "2.1/4.2 + 3/4"),
			new ExampleRewrite("Mixed 2", "1 + 2 - 2 + 5^2 + 49 / 7 + 8 * 4 "),
		};
	}
	
	@Override
	protected EnableItem<Rewriter> getExampleRewriters() {

		List<EnableItem<Rewriter>> arithmeticRewriters = new ArrayList<EnableItem<Rewriter>>();
		arithmeticRewriters.add(new LeafEnableItem<Rewriter>("Plus",  new Plus()));
		arithmeticRewriters.add(new LeafEnableItem<Rewriter>("Minus", new Minus()));
		arithmeticRewriters.add(new LeafEnableItem<Rewriter>("Times", new Times()));
		arithmeticRewriters.add(new LeafEnableItem<Rewriter>("Division", new Division()));
		arithmeticRewriters.add(new LeafEnableItem<Rewriter>("Exponentiation",  new Exponentiation()));
		GroupEnableItem<Rewriter> basicGroup = new GroupEnableItem<Rewriter>("Arithmetic", arithmeticRewriters);
		
		List<EnableItem<Rewriter>> groups = new ArrayList<EnableItem<Rewriter>>();
		groups.add(basicGroup);
		GroupEnableItem<Rewriter> root = new GroupEnableItem<Rewriter>("Arithmetic Rewriters", groups);
				
		return root; 
	}
}
