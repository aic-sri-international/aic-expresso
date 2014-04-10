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
package com.sri.ai.brewer.core;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Stack;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.CharactersAsStringsIterator;
import com.sri.ai.util.collect.EZIterator;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.HashMapTree;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.collect.Tree;

// TODO: This could use cleaning up. There are several levels of processing,
// on code point level, character level, and token level.
// There are two mechanisms for putting stuff back into the iterator.
// It seems like it would be much cleaner to have these multiple levels of streaming in different classes.

/**
 * A tokenizer separating numbers, identifiers, and other symbols (which are
 * provided as tokens one at a time). By default, it ignores white spaces, but
 * special constructors requesting white spaces to be considered are provided.
 * Tabs, spaces and newlines are considered white spaces. If returning white
 * space tokens, it collapses sequences of them into a token of a single
 * character, and it distinguishes newlines from spaces and tabs (which are
 * returned as a single space).
 * 
 * @author braz
 */
@Beta
public class TokenIterator extends EZIterator<String> {
	private static final int UNDERSCORE_CODE_POINT = "_".codePointAt(0);
	private static final int PRIME_CODE_POINT = "'".codePointAt(0);
	private Reader reader;
	private boolean noWhitespaces = true;
	private int next;
	private int position;
	private Stack<String> putBackTokens = new Stack<String>();
	private Stack<Integer> putBackCodePoints = new Stack<Integer>();
	private Tree specialTokens = new HashMapTree();
	
	public TokenIterator(Reader reader) throws IOException {
		this.reader = reader;
		position = 0;
		next = read();
	}

	public TokenIterator(String string) throws IOException {
		this(new StringReader(string));
	}
	
	public TokenIterator(Reader reader, boolean whitespaces) throws IOException {
		this.reader = reader;
		this.noWhitespaces = ! whitespaces;
		position = 0;
		next = read();
	}

	public TokenIterator(String string, boolean whitespaces) throws IOException {
		this(new StringReader(string), whitespaces);
	}
	
	public boolean isBasedOnString() {
		return reader instanceof StringReader;
	}
	
	public void putSpecialToken(String token) {
//		System.out.println("Special token: " + token);
		specialTokens.put(new CharactersAsStringsIterator(token));
	}
	
	public void putSpecialTokenIfItIsOne(String token) {
		if (token.length() > 1 && !isIdentifier(token) && !isNumber(token)) {
			putSpecialToken(token);
		}
	}
	
	public void clearSpecialTokens() {
		specialTokens.clear();
	}
	
	public String nextToken() {
		return next();	
	}
	
	@Override
	protected String calculateNext() { // assumes that 'next' is already on next character
		try {
			if (noWhitespaces) {
				while (isWhitespace(next)) {
					next = read();
				}
			}

			if (next == -1) {
				return null;
			}

			if (next == ' ' || next == '\t') {
				while (next == ' ' || next == '\t') {
					next = read();
				}
				return " ";
			}

			if (next == '\n') {
				while (next == '\n') {
					next = read();
				}
				return "\n";
			}

			if (next == '"' || next == '\'') {
				int delimiter = next;
				StringBuffer buffer = new StringBuffer();
				while ((next = read()) != delimiter) {
					if (next == '\\') {
						next = read();
					}
					buffer.append(codePointToString(next));
				}
				next = read(); // skip ending delimiter
				return buffer.toString();
			}
			
			if (next == '.' || Character.isDigit(next)) {
				StringBuffer buffer = new StringBuffer();
				boolean gotPoint = false;
				boolean gotE = false; // indicates whether E or e as in 7.1E-13 has been taken already.
				boolean characterBeforeNextIsE = false; // indicates whether last character was E or e.
				
				do {
					buffer.append(codePointToString(next));
					
					gotPoint = gotPoint || next == '.';
					if (next == 'E' || next == 'e') {
						gotE = true;
						characterBeforeNextIsE = true;
						gotPoint = true; // not supposed to have a point in the exponent.
					}
					else {
						characterBeforeNextIsE = false;
					}
					
					next = read();
				}
				while (Character.isDigit(next) || (!gotPoint && next == '.') || (!gotE && (next == 'E' || next == 'e') || (characterBeforeNextIsE && (next == '+' || next == '-'))));
				
				if (buffer.equals(".")) {  // not a number: unread last character, restore next as it was, and let it go on.
					unread(next);
					next = '.';
				}
				else {
					return buffer.toString();
				}
			}

			if (isIdentifierStart(next)) {
				StringBuffer buffer = new StringBuffer();
				do {
					buffer.append(codePointToString(next));
				}
				while (isIdentifierPart(next = read()));
				return buffer.toString();
			}

			Tree.GetResult getSpecialTokenResult = specialTokens.get(iteratorOverCharactersAsStringsFromNextToRead());
			if (getSpecialTokenResult.isValid()) {
				final String specialToken = Util.join("", getSpecialTokenResult.getValidPath());
				if (getSpecialTokenResult.getConsumedElements().size() > getSpecialTokenResult.getValidPath().size()) {
					final String string = (String)getSpecialTokenResult.getConsumedElements().get(getSpecialTokenResult.getConsumedElements().size()-1);
					int codePoint = string.codePointAt(0);
					unread(codePoint);
				}
				next = read(); // set next before returning
				return specialToken;
			}
			for (int i = getSpecialTokenResult.getConsumedElements().size() - 1; i > 0; i--) {
				int codePoint = ((String)getSpecialTokenResult.getConsumedElements().get(i)).codePointAt(0);
				unread(codePoint); // unread everything, but 'next'.
			}

			String result = codePointToString(next); // make token out of 'next'
			next = read(); // set 'next' before returning
			return result;

		} catch (IOException e) {
			Util.fatalError("TokenIterator exception", e);
		}
		return null;
	}

	public static boolean isIdentifier(String token) {
		if (! isIdentifierStart(token.charAt(0))) {
			return false;
		}
		for (int i = 1; i != token.length(); i++) {
			if (!isIdentifierPart(token.charAt(i))) {
				return false;
			}
		}
		return true;
	}
	
	public static boolean isNumber(String token) {
		for (int i = 0; i != token.length(); i++) {
			if (!Character.isDigit(token.charAt(i))) {
				return false;
			}
		}
		return true;
	}
	
	private FunctionIterator iteratorOverCharactersAsStringsFromNextToRead() {
		List<Object> base = Util.list((Object)Util.list(next), readIterator);
		return new FunctionIterator<Integer, String>(
				codePointToString,
				new NestedIterator<Integer>(base)
		);
		// It seems pretty inefficient to process characters as strings.
		// Perhaps this can be improved.
	}

	////////////////////////////////////////// READ AND POSITION
	
	private int read() throws IOException {
		if (putBackCodePoints.isEmpty()) {
			int result = reader.read();
			return result;
		}
		return putBackCodePoints.pop();
		// in case you're wondering why 'position' isn't updated by this method,
		// this is because 'read' reads characters, not tokens, and 'position' refers to tokens.
	}
	
	private void unread(int codePoint) {
		putBackCodePoints.push(codePoint);
	}
	
	/** An iterator using {@link #read()}. */
	private Iterator<Integer> readIterator = new EZIterator<Integer>() {
		@Override
		protected Integer calculateNext() {
			try {
				int c = read();
				if (c != -1) {
					return c;
				}
			} catch (IOException e) {
				Util.fatalError("Error in tokenizer", e);
			}
			return null;
		}
	};
	
	/** Indicates the position of the next token to be read. */
	public int getPosition() {
		return position;
	}
	
	////////////////////////////////////////// PUT BACK MECHANISM
	
	public void putBack(String token) {
		putBackTokens.push(token);
		position--;
	}
	
	/** Puts tokens from a list back in the token iterator, starting from the END of the list. */
	public void putBack(List<String> token) {
		ListIterator<String> iterator = token.listIterator(token.size());
		while (iterator.hasPrevious()) {
			putBack(iterator.previous());
		}
	}
	
	/**
	 * Indicates whether token iterator has already read token that have been put back in it.
	 */
	public boolean hasPutBackTokens() {
//		System.out.println("Put back tokens: " + putBackTokens);
		boolean result = ! putBackTokens.isEmpty();
		return result;
	}
	
	public boolean hasNext() {
		boolean hasNextTokenInPutBackTokens = hasPutBackTokens();
		boolean hasNextTokenFromIterator = super.hasNext();
		boolean result = hasNextTokenInPutBackTokens || hasNextTokenFromIterator;
		return result;
	}
	
	public String next() {
		String token;
//		System.out.println("Position: " + getPosition());
		if (putBackTokens.isEmpty()) {
			token = super.next();
		}
		else {
			token = putBackTokens.pop();
		}
//		System.out.println("Token: " + token);
		position++;
		return token;
	}
	
	/** Returns whether the base iterator has a next token; must be used for debugging purposes only. */
	public boolean baseHasNext() {
		return super.hasNext();
	}
	
	/**
	 * Makes sure that the i-th token is in the random access (put-back stack)
	 * portion of the token iterator.
	 * Assumes i >= current position.
	 * If there is no i-th token (due to iterator being exhausted), leaves all tokens in put-back stack.
	 */
	public void makeSureIthPositionIsInRandomAccess(int i) {
		assert(i >= getPosition()) : i + "-th token must be equal or ahead of next token";
		if (i - getPosition() > putBackTokens.size()) {
			List<String> tokens = new LinkedList<String>();
			while (hasNext() && getPosition() <= i) {
				tokens.add(nextToken());
			}
			putBack(tokens);
			// note that the above reads the tokens in the stack just to place them back again.
			// we could be hackier and go straight to the underlying iterator, skipping the stack,
			// and then adding whatever was read into the stack's bottom, but that would violate
			// the stack interface, and be more complex in general.
		}
	}

	/**
	 * Returns the i-th token in iterator, in constant time if this token has already been read at some point
	 * and linear time otherwise.
	 */
	public String peekAtIthToken(int i) {
		makeSureIthPositionIsInRandomAccess(i);
		return peekAtIthTokenAfterMakingSureItIsInRandomAccess(i);
	}

	/**
	 * A version of {@link #peekAtIthToken(int)} that does not try to ensure that the i-th
	 * token is already in random access.
	 * This is useful when a number of random accesses will done together and a single call to
	 * {@link #makeSureIthPositionIsInRandomAccess(int)} on the largest index is enough to
	 * ensure random access to them all.
	 */
	public String peekAtIthTokenAfterMakingSureItIsInRandomAccess(int i) {
		int indexOfLastElementInStack = putBackTokens.size() - 1;
		int stackDepthOfIthToken = i - getPosition();
		int indexInStack = indexOfLastElementInStack - stackDepthOfIthToken; // stack is in reverse order
		String result = putBackTokens.elementAt(indexInStack);
		return result;
	}
	
	////////////////////////////////////////// END OF PUT BACK MECHANISM

	private static String codePointToString(int codePoint) {
		return new String(new int[]{codePoint}, 0, 1);
	}
	
	////////////////////////////////////////// READ AND POSITION
	
	private static class CodePointToString implements Function<Integer, String> {
		public String apply(Integer codePoint) {
			return codePointToString(codePoint.intValue());
		}
	}
	
	private static final Function<Integer, String> codePointToString = new CodePointToString();

	public void advanceNTokens(int size) {
		while (size-- != 0) {
			next();
		}
	}
	
	public boolean isWhitespace(int c) {
		return c == ' ' || c == '\t' || c == '\n';
	}
	
	public static boolean isIdentifierStart(char ch) {
		return Character.isUnicodeIdentifierStart(ch) && ch != '_';
	}
	
	public static boolean isIdentifierPart(char ch) {
		return (
				Character.isUnicodeIdentifierPart(ch) ||
				ch == '\''
		) && ch != '_';
	}

	public static boolean isIdentifierStart(int codePoint) {
		return Character.isUnicodeIdentifierStart(codePoint) && codePoint != UNDERSCORE_CODE_POINT;
	}
	
	public static boolean isIdentifierPart(int codePoint) {
		return (
				Character.isUnicodeIdentifierPart(codePoint) ||
				codePoint == PRIME_CODE_POINT
		) && codePoint != UNDERSCORE_CODE_POINT;
	}
}
