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
package com.sri.ai.test.brewer;


import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;


import com.google.common.collect.Lists;
import com.sri.ai.brewer.core.TokenIterator;
import com.sri.ai.util.Util;

public class TokenIteratorTest {

	@Before
	public void setUp() throws Exception {
	}
	
	@Test
	public void testTokenizerWithWhitespaces() throws IOException {
		String input;
		List<String> expected;
		
		input = "";
		expected = Collections.emptyList();
		testWithWhitespaces(input, expected);
		
		input = "Hello world!";
		expected = Util.list("Hello", " ", "world", "!");
		testWithWhitespaces(input, expected);
		
		input = "Hello \"stringfied\" \"very beautiful\" 'multiply \\'delimited\\'' world!";
		// internally, this string is: <Hello "stringfied" "very beautiful" 'multiply \'delimited\'' world!>
		// so the single quotes around "delimited" are quoted and therefore part of the produced token.
		expected = Util.list("Hello", " ", "stringfied", " ", "very beautiful", " ",  "multiply 'delimited'", " ", "world", "!");
		testWithWhitespaces(input, expected);
		
		input = "<=>";
		expected = Util.list("<", "=", ">");
		testWithWhitespaces(input, expected);
		
		input = "ananas <=> banana";
		expected = Util.list("ananas", " ", "<", "=", ">", " ", "banana");
		testWithWhitespaces(input, expected);
		
		input = "123 <=> banana";
		expected = Util.list("123", " ", "<", "=", ">", " ", "banana");
		testWithWhitespaces(input, expected);
		
		input = "123ananas <=> banana";
		expected = Util.list("123", "ananas", " ", "<", "=", ">", " ", "banana");
		testWithWhitespaces(input, expected);
		
		input = "ananas123 <=> banana";
		expected = Util.list("ananas123", " ", "<", "=", ">", " ", "banana");
		testWithWhitespaces(input, expected);
		
		input = "ananas123    \n\n\n\n    <=> banana";
		expected = Util.list("ananas123", " ", "\n", " ", "<", "=", ">", " ", "banana");
		testWithWhitespaces(input, expected);
	}

	@Test
	public void testTokenizerWithWhitespacesAndSpecialTokens() throws IOException {
		String input;
		List<String> expected;
		
		input = "";
		expected = Collections.emptyList();
		testWithWhitespaces(input, expected);
		
		input = "<=>";
		expected = Util.list("<=>");
		testWithWhitespaces(input, expected, Lists.newArrayList("<=>"));
		
		input = "<=";
		expected = Util.list("<" , "=");
		testWithWhitespaces(input, expected, Lists.newArrayList("<=>"));
		
		input = "<=";
		expected = Util.list("<=");
		testWithWhitespaces(input, expected, Lists.newArrayList("<=>", "<="));
		
		input = "i + j++";
		expected = Util.list("i", " ", "+", " ", "j", "++");
		testWithWhitespaces(input, expected, Lists.newArrayList("++"));
		
		input = "i + j++ + +++k";
		expected = Util.list("i", " ", "+", " ", "j", "++", " ", "+", " ", "+++", "k");
		testWithWhitespaces(input, expected, Lists.newArrayList("++", "+++"));
		
		input = "i + j++ + +++k";
		expected = Util.list("i", " ", "+", " ", "j", "+", "+", " ", "+", " ", "+++", "k");
		testWithWhitespaces(input, expected, Lists.newArrayList("+++"));
		
		input = "ananas <=> banana";
		expected = Util.list("ananas", " ", "<=>", " ", "banana");
		testWithWhitespaces(input, expected, Lists.newArrayList("<=>"));
	}

	@Test
	public void testTokenizerWithoutWhitespacesAndSpecialTokens() throws IOException {
		String input;
		List<String> expected;
		
		input = "";
		expected = Collections.emptyList();
		testWithoutWhitespaces(input, expected);
		
		input = "<=>";
		expected = Util.list("<=>");
		testWithoutWhitespaces(input, expected, Lists.newArrayList("<=>"));
		
		input = "<=";
		expected = Util.list("<" , "=");
		testWithoutWhitespaces(input, expected, Lists.newArrayList("<=>"));
		
		input = "<=";
		expected = Util.list("<=");
		testWithoutWhitespaces(input, expected, Lists.newArrayList("<=>", "<="));
		
		input = "i + j++";
		expected = Util.list("i", "+", "j", "++");
		testWithoutWhitespaces(input, expected, Lists.newArrayList("++"));
		
		input = "i + j++ + +++k";
		expected = Util.list("i", "+", "j", "++", "+", "+++", "k");
		testWithoutWhitespaces(input, expected, Lists.newArrayList("++", "+++"));
		
		input = "i + j++ + +++k";
		expected = Util.list("i", "+", "j", "+", "+", "+", "+++", "k");
		testWithoutWhitespaces(input, expected, Lists.newArrayList("+++"));
		
		input = "ananas <=> banana";
		expected = Util.list("ananas", "<=>", "banana");
		testWithoutWhitespaces(input, expected, Lists.newArrayList("<=>"));
	}

	@Test
	public void testTokenizerWithoutWhitespaces() throws IOException {
		String input;
		List<String> expected;
		
		input = "";
		expected = Collections.emptyList();
		testWithoutWhitespaces(input, expected);
		
		input = "Hello world!";
		expected = Util.list("Hello", "world", "!");
		testWithoutWhitespaces(input, expected);
		
		input = "Hello \"stringfied\" \"very beautiful\" 'multiply \\'delimited\\'' world!";
		// internally, this string is: <Hello "stringfied" "very beautiful" 'multiply \'delimited\'' world!>
		// so the single quotes around "delimited" are quoted and therefore part of the produced token.
		expected = Util.list("Hello", "stringfied", "very beautiful", "multiply 'delimited'", "world", "!");
		testWithoutWhitespaces(input, expected);
		
		input = "<=>";
		expected = Util.list("<", "=", ">");
		testWithoutWhitespaces(input, expected);
		
		input = "ananas <=> banana";
		expected = Util.list("ananas", "<", "=", ">", "banana");
		testWithoutWhitespaces(input, expected);
		
		input = "123 <=> banana";
		expected = Util.list("123", "<", "=", ">", "banana");
		testWithoutWhitespaces(input, expected);
		
		input = "123ananas <=> banana";
		expected = Util.list("123", "ananas", "<", "=", ">", "banana");
		testWithoutWhitespaces(input, expected);
		
		input = "ananas123 <=> banana";
		expected = Util.list("ananas123", "<", "=", ">", "banana");
		testWithoutWhitespaces(input, expected);
		
		input = "ananas123    \n\n\n\n    <=> banana";
		expected = Util.list("ananas123", "<", "=", ">", "banana");
		testWithoutWhitespaces(input, expected);
	}

	@Test
	public void testTokenizerOnNumbersWithoutWhitespaces() throws IOException {
		String input;
		List<String> expected;
		
		input = "1 2 3";
		expected = Util.list("1", "2", "3");
		testWithoutWhitespaces(input, expected);
		
		input =              "1       .2       0.33    ..           .123.          blah       12..33           12.3.44        1.0E34    1.0E-34    1.0E+34";
		expected = Util.list("1",    ".2",    "0.33", ".", ".",    ".123", ".",   "blah",    "12.", ".33",    "12.3", ".44", "1.0E34", "1.0E-34", "1.0E+34");
		testWithoutWhitespaces(input, expected);
	}
	
	@Test
	public void testRandomAccess() throws IOException {
		String input;
		TokenIterator t;
		
		input = "a b c d e f g h i j k l";
		t = new TokenIterator(input);
		String third = t.peekAtIthToken(3);
		assertEquals("d", third);
		String first = t.nextToken();
		assertEquals("a", first);
		String eleventh = t.peekAtIthToken(11);
		assertEquals("l", eleventh);
		String second = t.nextToken();
		assertEquals("b", second);
	}
	
	private void testWithWhitespaces(String input, List<String> expected) throws IOException {
		test(input, expected, true);
	}

	private void testWithWhitespaces(String input, List<String> expected, List<String> specialTokens) throws IOException {
		test(input, expected, true, specialTokens);
	}

	private void testWithoutWhitespaces(String input, List<String> expected) throws IOException {
		testWithoutWhitespaces(input, expected, new LinkedList<String>());
	}

	private void testWithoutWhitespaces(String input, List<String> expected, List<String> specialTokens) throws IOException {
		test(input, expected, false, specialTokens);
	}

	private void test(String input, List<String> expected,
			final boolean whitespaces) throws IOException {
		test(input, expected, whitespaces, new LinkedList<String>());
	}

	private void test(String input, List<String> expected,
			final boolean whitespaces, List<String> specialTokens) throws IOException {
		TokenIterator t;
		t = new TokenIterator(new StringReader(input), whitespaces);
		
		for (String specialToken : specialTokens) {
			t.putSpecialToken(specialToken);
		}

		Iterator<String> expectedTokenIterator = expected.iterator();
		int expectedPosition = 0;
		while (t.hasNext()) {
			Assert.assertEquals(expectedPosition, t.getPosition());

			String token = t.next();
			//System.out.println(token);
			String expectedToken = expectedTokenIterator.next();
			Assert.assertEquals(expectedToken, token);

			expectedPosition++;
			Assert.assertEquals(expectedPosition, t.getPosition());
		}
	}
}
