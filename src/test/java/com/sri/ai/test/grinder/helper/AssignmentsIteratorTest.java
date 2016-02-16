/*
 * Copyright (c) 2014, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-4-Clause
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
package com.sri.ai.test.grinder.helper;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.AssignmentsIterator;


public class AssignmentsIteratorTest  {
	
	@Test
	public void test1() {
		RewritingProcess process = new DefaultRewritingProcess();
		Type myType = new Categorical("People", 4, arrayList(makeSymbol("oscar"), makeSymbol("mary")));
		Symbol x = makeSymbol("X");
		Symbol y = makeSymbol("Y");

		String expected =
				"{X=oscar, Y=oscar}\n" + 
				"{X=mary, Y=oscar}\n" + 
				"{X=people3, Y=oscar}\n" + 
				"{X=people4, Y=oscar}\n" + 
				"{X=oscar, Y=mary}\n" + 
				"{X=mary, Y=mary}\n" + 
				"{X=people3, Y=mary}\n" + 
				"{X=people4, Y=mary}\n" + 
				"{X=oscar, Y=people3}\n" + 
				"{X=mary, Y=people3}\n" + 
				"{X=people3, Y=people3}\n" + 
				"{X=people4, Y=people3}\n" + 
				"{X=oscar, Y=people4}\n" + 
				"{X=mary, Y=people4}\n" + 
				"{X=people3, Y=people4}\n" + 
				"{X=people4, Y=people4}";

		Symbol myTypeExpression = makeSymbol(myType.getName());
		process = process.add(myType);
		process = process.registerIndicesAndTypes(map(x, myTypeExpression, y, myTypeExpression));
		AssignmentsIterator assignmentsIterator = new AssignmentsIterator(list(x, y), process);
		String actual = join("\n", assignmentsIterator);
		
		// System.out.println(actual);	
		
		assertEquals(expected, actual);
	}

	@Test
	public void test2() {
		RewritingProcess process = new DefaultRewritingProcess();
		Type myType = new Categorical("People", 2, arrayList(makeSymbol("oscar"), makeSymbol("mary")));
		Symbol x = makeSymbol("X");
		Symbol y = makeSymbol("Y");

		String expected = 
				"{X=oscar, Y=oscar}\n" + 
				"{X=mary, Y=oscar}\n" + 
				"{X=oscar, Y=mary}\n" + 
				"{X=mary, Y=mary}";

		Symbol myTypeExpression = makeSymbol(myType.getName());
		process = process.add(myType);
		process = process.registerIndicesAndTypes(map(x, myTypeExpression, y, myTypeExpression));
		AssignmentsIterator assignmentsIterator = new AssignmentsIterator(list(x, y), process);
		String actual = join("\n", assignmentsIterator);
		
		// System.out.println(actual);	
		
		assertEquals(expected, actual);
	}

	@Test
	public void test3() {
		RewritingProcess process = new DefaultRewritingProcess();
		Type peopleType = new Categorical("People", 4, arrayList(makeSymbol("oscar"), makeSymbol("mary")));
		Type petsType = new Categorical("Pets", 3, arrayList(makeSymbol("fido"), makeSymbol("purrs")));
		Symbol x = makeSymbol("X");
		Symbol y = makeSymbol("Y");

		String expected =
				"{X=oscar, Y=fido}\n" + 
				"{X=mary, Y=fido}\n" + 
				"{X=people3, Y=fido}\n" + 
				"{X=people4, Y=fido}\n" + 
				"{X=oscar, Y=purrs}\n" + 
				"{X=mary, Y=purrs}\n" + 
				"{X=people3, Y=purrs}\n" + 
				"{X=people4, Y=purrs}\n" + 
				"{X=oscar, Y=pets3}\n" + 
				"{X=mary, Y=pets3}\n" + 
				"{X=people3, Y=pets3}\n" + 
				"{X=people4, Y=pets3}";

		Symbol myPeopleTypeExpression = makeSymbol(peopleType.getName());
		Symbol myPetsTypeExpression = makeSymbol(petsType.getName());
		process = process.add(peopleType);
		process = process.add(petsType);
		process = process.registerIndicesAndTypes(map(x, myPeopleTypeExpression, y, myPetsTypeExpression));
		AssignmentsIterator assignmentsIterator = new AssignmentsIterator(list(x, y), process);
		String actual = join("\n", assignmentsIterator);
		
		// System.out.println(actual);	

		assertEquals(expected, actual);
	}
}
