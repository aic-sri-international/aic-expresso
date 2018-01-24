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
package com.sri.ai.expresso.core;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.lang3.text.translate.AggregateTranslator;
import org.apache.commons.lang3.text.translate.CharSequenceTranslator;
import org.apache.commons.lang3.text.translate.EntityArrays;
import org.apache.commons.lang3.text.translate.JavaUnicodeEscaper;
import org.apache.commons.lang3.text.translate.LookupTranslator;
import org.apache.commons.lang3.text.translate.OctalUnescaper;
import org.apache.commons.lang3.text.translate.UnicodeUnescaper;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SyntaxLeaf;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.AICUtilConfiguration;
import com.sri.ai.util.base.BinaryProcedure;
import com.sri.ai.util.math.Rational;

/**
 * A syntax tree with a single node representing a symbol. Symbols can stand for
 * variables or values; however, in this library, variables are
 * indistinguishable from string values at the expression representation level.
 * Strings are regarded as variable names at the semantic, rather than
 * syntactic, level. For example, in an expression <code>sum(i, f(i))</code>,
 * <code>i</code> might be considered a variable because it is the index of
 * function <code>sum</code>, while <code>f</code> is considered a constant.
 * This decision fundamentally depends on the semantics of <code>sum</code>,
 * which "knows" that the first argument is the index. The semantic level may
 * choose any criteria whatever to decide what is a variable. For example, it
 * could always consider <code>x</code>, <code>y</code>, and <code>z</code> as
 * variables, or capitalized strings (as in Prolog). This expression
 * representation library does not commit to any set of such criteria.
 * 
 * Numbers are represented with {@link Rational} and therefore have arbitrary
 * precision. {@link #intValue()}, {@link #intValueExact()} and
 * {@link #doubleValue()} are available but may decrease precision, so they
 * should not be used in contexts in which arbitrary precision is expected,
 * which is probably true if the results are going to be stored again in a
 * Symbol.
 * 
 * @author braz
 */
@Beta
public class DefaultSyntaxLeaf extends AbstractSyntaxTree implements SyntaxLeaf  {
	private static final long serialVersionUID = 1L;
	
	// ([a-zA-Z] | [0-9] | '_') ([a-zA-Z] | [0-9] | '_')* ('\'')*
	public static Pattern UNQUOTED_SYMBOLIC_NAME = Pattern.compile("[[a-z][A-Z][0-9]_]+[']*");
	private static final Set<String> _specialFunctorSymbols;
	static
	{
		_specialFunctorSymbols = new HashSet<>();
		_specialFunctorSymbols.add(FunctorConstants.PLUS);
		_specialFunctorSymbols.add(FunctorConstants.MINUS);
		_specialFunctorSymbols.add(FunctorConstants.TIMES);
		_specialFunctorSymbols.add(FunctorConstants.DIVISION);
		_specialFunctorSymbols.add(FunctorConstants.EXPONENTIATION);
		_specialFunctorSymbols.add(FunctorConstants.EQUIVALENCE);
		_specialFunctorSymbols.add(FunctorConstants.IMPLICATION);
		_specialFunctorSymbols.add(FunctorConstants.IMPLIED);
		_specialFunctorSymbols.add(FunctorConstants.EQUAL);
		_specialFunctorSymbols.add(FunctorConstants.DISEQUALITY);
		_specialFunctorSymbols.add(FunctorConstants.GREATER_THAN);
		_specialFunctorSymbols.add(FunctorConstants.LESS_THAN);
		_specialFunctorSymbols.add(FunctorConstants.LESS_THAN_OR_EQUAL_TO);
		_specialFunctorSymbols.add(FunctorConstants.GREATER_THAN_OR_EQUAL_TO);
		_specialFunctorSymbols.add(FunctorConstants.FUNCTION_TYPE);
	}
	private static int     displayNumericPrecision                = ExpressoConfiguration.getDisplayNumericPrecisionForSymbols();
	private static boolean displayNumericsExactly                 = ExpressoConfiguration.isDisplayNumericsExactlyForSymbols();
	private static int     displayScientificGreaterNIntegerPlaces = ExpressoConfiguration.getDisplayScientificGreaterNIntegerPlaces();
	private static int     displayScientificAfterNDecimalPlaces   = ExpressoConfiguration.getDisplayScientificAfterNDecimalPlaces();
	
	public static final CharSequenceTranslator UNESCAPE_STRING_VALUE = 
		        new AggregateTranslator(
		            new OctalUnescaper(),     // .between('\1', '\377'),
		            new UnicodeUnescaper(),
		            new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_UNESCAPE()),
		            new LookupTranslator(
		                      new String[][] { 
		                            {"\\\\", "\\"},
		                            {"\\\"", "\""},
		                            {"\\'", "'"},
		                            {"\\", ""}
		                      })
		        );
	
    public static final CharSequenceTranslator ESCAPE_STRING_VALUE = 
            new LookupTranslator(
              new String[][] { 
            	{"'", "\\'"},
                {"\"", "\\\""},
                {"\\", "\\\\"},
            }).with(
              new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_ESCAPE())
            ).with(
              JavaUnicodeEscaper.outsideOf(32, 0x7f) 
          );
	
	//
    private boolean isStringLiteral = false;
	private int hashCode = -1; // lazy init and re-use the calculated hashCode.
	
	@Override
	public Object getValue() {
		return valueOrRootSyntaxTree;
	}
	
	@Override
	public boolean isStringLiteral() {
		return isStringLiteral;
	}

	@Override
	public SyntaxTree getRootTree() {
		return null;
	}

	@Override
	public Object getLabel() {
		return valueOrRootSyntaxTree;
	}

	/**
	 * Set the numeric display precision for numeric valued symbols.
	 * 
	 * @param precision
	 *        the decimal display precision.
	 *        
	 * @return the old numeric display precision;
	 */
	public static int setNumericDisplayPrecision(int precision) {
		int oldPrecision = displayNumericPrecision;
		
		displayNumericPrecision = precision;
		
		return oldPrecision;
	}
	
	/**
	 * Set whether numerics should be displayed exactly or not.
	 * 
	 * @param displayExactly
	 *        whether to display numerics exactly or not.
	 *        
	 * @return the old display exactly setting;
	 */
	public static boolean setDisplayNumericsExactly(boolean displayNumericsExactly) {
		boolean oldDisplayNumericsExactly = DefaultSyntaxLeaf.displayNumericsExactly;
		
		DefaultSyntaxLeaf.displayNumericsExactly = displayNumericsExactly;
		
		return oldDisplayNumericsExactly;
	}
	
	/**
	 * Set the number of integer places a number is to have before it is
	 * displayed in scientific notation.
	 * 
	 * @param numIntegerPlaces
	 * @return the value previously used before being set here.
	 */
	public static int setDisplayScientificGreaterNIntegerPlaces(int numIntegerPlaces) {
		int oldValue = displayScientificGreaterNIntegerPlaces;
		
		displayScientificGreaterNIntegerPlaces = numIntegerPlaces;
				
		return oldValue;
	}
	
	/**
	 * Set the number of decimal places a number is to have before it is
	 * displayed in scientific notation.
	 * 
	 * @param numDecimalPlaces
	 * @return the value previously used before being set here.
	 */
	public static int setDisplayScientificAfterNDecimalPlaces(int numDecimalPlaces) {
		int oldValue = displayScientificAfterNDecimalPlaces;
		
		displayScientificAfterNDecimalPlaces = numDecimalPlaces;
				
		return oldValue;
	}
	
	public static SyntaxLeaf createSyntaxLeaf(Object inputValue, boolean isStringLiteral) {	
		Object     value  = inputValue;
		SyntaxLeaf result = null;
		// If global symbol table to be used and the symbol's value is not
		// an expression - i.e. quoted expressions of the form:
		// <X>
		// as these need to be instantiated each time in order to be
		// parsed correctly.
		boolean useGlobalSymbolTable = _useGlobalSymbolTable && !(inputValue instanceof Expression);
		if (useGlobalSymbolTable) {
			if (isStringLiteral) {
				result = _globalStringLiteralTable.getIfPresent(inputValue);
			}
			else {
				result = _globalSymbolTable.getIfPresent(inputValue);
			}
		} 
		
		if (result == null) {			
			if (value instanceof Number && !(value instanceof Rational)) {
				value = new Rational(((Number)value).doubleValue());
			} 
			else if (value.equals("true")) {
				value = Boolean.TRUE;
			} 
			else if (value.equals("false")) {
				value = Boolean.FALSE;
			} 
			else if (value instanceof String) {				
				value = UNESCAPE_STRING_VALUE.translate((String) value);
				
				if (!isStringLiteral) {
					// attempt to implicitly convert to a Rational value if possible.
					try {
						value = new Rational((String)value);
					}
					catch (NumberFormatException e) {
						// ignore
					}
				}
			}
			
			result = new DefaultSyntaxLeaf(value, isStringLiteral);
			if (useGlobalSymbolTable && !(!_cacheNumericSymbols && result.getValue() instanceof Number)) {
				if (isStringLiteral) {
					_globalStringLiteralTable.put(inputValue, result);
				}
				else {
					_globalSymbolTable.put(inputValue, result);
				}
			}
		}
		
		return result;
	}
	


	@Override
	public Iterator<SyntaxTree> getImmediateSubTreesIncludingRootOneIterator() {
		List<SyntaxTree> emptyList = Collections.emptyList();
		return emptyList.iterator();
	}

	@Override
	public int numberOfImmediateSubTreesIncludingRootOneIterator() {
		return 0;
	}

	@Override
	public SyntaxTree setImmediateSubTree(int i, Object newIthArgument) {
		throw new Error("Attempt at changing " + i + "-th sub-syntax tree of Symbol: " + this + " with " + newIthArgument + ", but Symbols have no sub-trees");
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesFirstOccurrence(replacementFunction, prunePredicate, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		if (prunePredicate != null && prunePredicate.apply(this)) {
			return this;
		}
		SyntaxTree replacement = replacementFunction.apply(this);
		if (replacement == null) {
			replacement = this;
		}
		if (listener != null) {
			listener.apply(this, replacement);
		}
		return replacement;
	}
	
	@Override
	public SyntaxTree replaceSubTrees(Function<SyntaxTree, SyntaxTree> replacementFunction,
			boolean onlyTheFirstOne, Predicate<SyntaxTree> prunePredicate,
			BinaryProcedure<SyntaxTree, SyntaxTree> listener, boolean ignoreTopExpression) {
		if (ignoreTopExpression) {
			return this;
		}
		return replaceSubTreesFirstOccurrence(replacementFunction, prunePredicate, listener);
	}

	@Override
	public int hashCode() {
		if (hashCode == -1) {
			hashCode = getValue().hashCode() + (isStringLiteral() ? 11 : 0);
		}
		return hashCode;
	}
	
	@Override
	public boolean equals(Object another) {
		if (this == another) {
			return true;
		}
		
		if (another instanceof CompoundSyntaxTree) {
			return false;
		}
		
		SyntaxLeaf anotherSymbol = null;
		if (another instanceof SyntaxLeaf) {
			anotherSymbol = (SyntaxLeaf) another;
		} 
		else {
			if (isStringLiteral()) {
				// If this is a string literal the other object will not be
				// coerced into one so they are not equal.
				return false;
			}
			anotherSymbol = createSyntaxLeaf(another, false);
			// Test again, as may have had self returned from the symbol table.
			if (this == anotherSymbol) {
				return true;
			}
		}
		
		if (hashCode() == anotherSymbol.hashCode()) {
			return isStringLiteral() == anotherSymbol.isStringLiteral() && getValue().equals(anotherSymbol.getValue());
		}
		return false;
	}
	
	@Override
	/**
	 * Compares this Symbol to other syntax trees, placing it before {@link CompoundSyntaxTree}s and comparing
	 * it to other Symbols by comparing their values if they are in the same type (as returned by {@link #getValueType()},
	 * or their types if they are of different types.
	 */
	public int compareTo(Object another) {
		if (this == another) {
			return 0;
		}
		
		if (another instanceof CompoundSyntaxTree) {
			return -1; // Symbols come first
		}
		
		SyntaxLeaf anotherSymbol = null;
		if (another instanceof SyntaxLeaf) {
			anotherSymbol = (SyntaxLeaf) another;
		} 
		else {
			anotherSymbol = createSyntaxLeaf(another, false);
			// Test again, as may have had self returned from the symbol table.
			if (this == anotherSymbol) {
				return 0;
			}
		}
		
		try {
			@SuppressWarnings("unchecked")
			Comparable<Object> value        = (Comparable<Object>) getValue();
			@SuppressWarnings("unchecked")
			Comparable<Object> anotherValue = (Comparable<Object>) anotherSymbol.getValue();
			
			int result;
			String valueType = getValueType();
			String anotherValueType = anotherSymbol.getValueType();
			if (valueType.equals(anotherValueType)) {
				result = value.compareTo(anotherValue);
			}
			else {
				result = valueType.compareTo(anotherValueType);
			}
			return result;
		}
		catch (ClassCastException e) {
			throw new Error("Using DefaultSymbol.compareTo method with non-comparable values.", e);
		}
	}

	/**
	 * Returns "Number" if the Symbol's value is a number and a simple class name for other types.
	 */
	@Override
	public String getValueType() {
		if (getValue() instanceof Number) {
			return "Number";
		}
		else {
			return getValue().getClass().getSimpleName();
		}
	}
	
	@Override
	public String toStringWithoutCaching() {
		String result = "";
		if (valueOrRootSyntaxTree instanceof String) {
			result = (String) valueOrRootSyntaxTree;
			if (isStringLiteral()) {
				result = ESCAPE_STRING_VALUE.translate((String)valueOrRootSyntaxTree);
				// Ensure is output as a String again
				result = "\"" + result + "\"";
			}
			else if (isSymbolValueQuotingRequired(result)) {
				result = ESCAPE_STRING_VALUE.translate((String)valueOrRootSyntaxTree);
				result = "'" + result + "'";
			}
		}
		else if (valueOrRootSyntaxTree instanceof Expression) {
			result = "<" + valueOrRootSyntaxTree + ">";
		}
		else if (valueOrRootSyntaxTree instanceof Number && displayNumericPrecision != 0) {
			Rational rationalValue = ((Rational) valueOrRootSyntaxTree);
			RationalWithPrecisionInformation rationalWithPrecisionInformation = new RationalWithPrecisionInformation(rationalValue);
			result = rationalWithPrecisionInformation.getRepresentation();
		}
		else {
			result = valueOrRootSyntaxTree.toString();
		}
		
		return result;
	}
	
	private static class RationalWithPrecisionInformation {
		public Rational rationalValue;
		public Rational absValue;
		public Rational[] integerAndFractionalPart;
		public Rational integerPart;
		public Rational fractionalPart;
		public Rational reducedIntegerPart;
		public Rational increasedFractionalPart;
		public boolean decimalRepresentationLosesPrecision;

		public RationalWithPrecisionInformation(Rational rationalValue) {
			this.rationalValue            = rationalValue;
			this.absValue                 = rationalValue.abs();
			this.integerAndFractionalPart = absValue.integerAndFractionalPart();
			this.integerPart              = integerAndFractionalPart[0];
			this.fractionalPart           = integerAndFractionalPart[1];

			decimalRepresentationLosesPrecision = decimalRepresentationLosesPrecision();
		}

		private boolean decimalRepresentationLosesPrecision() {

			this.reducedIntegerPart      = integerPart;
			this.increasedFractionalPart = fractionalPart;

			for (int i = 0; i < displayNumericPrecision; i++) {				
				boolean eliminatedAllIntegerPlaces = reducedIntegerPart.compareTo(1) < 0;
				if (eliminatedAllIntegerPlaces) {
					boolean eliminatedAllDecimalPlaces = increasedFractionalPart.isInteger();
					if (eliminatedAllDecimalPlaces) {							
						// eliminated all places in rational value in less steps than displayNumericPrecision.
						// Therefore, converting to decimal representation does not lose precision.
						break;
					}
					else {
						// eliminate decimal place
						increasedFractionalPart = increasedFractionalPart.multiply(10);
					}
				}
				else {
					// eliminate integer place
					reducedIntegerPart = reducedIntegerPart.divide(10);						
				}
			}
			boolean result = !(reducedIntegerPart.compareTo(1) < 0 && increasedFractionalPart.isInteger());
			return result;
		}

		public String getRepresentation() {
			String result;
			boolean cannotUseDecimalRepresentation = displayNumericsExactly && decimalRepresentationLosesPrecision;
			if (cannotUseDecimalRepresentation) {		
				result = getExactRepresentation();
			}
			else {		
				result = getDecimalRepresentation();
			}
			return result;
		}

		private String getExactRepresentation() {
			String result;
			if (rationalValue.isInteger()) {
				result = rationalValue.getNumerator().toString();
			}
			else {
				// Output as an exact ratio
				result = rationalValue.getNumerator().toString() + "/" + rationalValue.getDenominator().toString(); 
			}
			return result;
		}

		private String getDecimalRepresentation() {
			int approximatePrecision = decideApproximatePrecision();
			String result = getDecimalRepresentationGivenApproximatePrecision(approximatePrecision);
			return result;
		}

		private int decideApproximatePrecision() {
			int approximationPrecision = displayNumericPrecision;
			boolean decimalRepresentationLosesIntegerPartPrecision = reducedIntegerPart.compareTo(1) >= 0;
			if (decimalRepresentationLosesIntegerPartPrecision) {
				approximationPrecision = displayScientificGreaterNIntegerPlaces;
			}
			return approximationPrecision;
		}

		private String getDecimalRepresentationGivenApproximatePrecision(int approximationPrecision) {
			String result;
			if (requiresScientificFormat()) {					
				result = rationalValue.toStringExponent(approximationPrecision);
			}
			else {
				result = rationalValue.toStringDotRelative(approximationPrecision);
			}
			
			result = removeTrailingZerosToRight(result);
			return result;
		}
		
		boolean numberOfIntegerPlacesIsGreaterThan(Rational rational, int integerPlacesBound) {
			int numberOfCountedIntegerPlaces = 0;
			while ((numberOfCountedIntegerPlaces <= integerPlacesBound) && rational.compareTo(1) >= 0) {
				numberOfCountedIntegerPlaces++;
				rational = rational.divide(10);
			}
			boolean result = numberOfCountedIntegerPlaces > integerPlacesBound;
			return result;
		}

		boolean numberOfDecimalPlacesIsGreaterThan(Rational rational, int decimalPlacesBound) {
			int numberOfCountedDecimalPlaces = 0;
			while ((numberOfCountedDecimalPlaces <= decimalPlacesBound) && ! rational.isInteger()) {
				numberOfCountedDecimalPlaces++;
				rational = rational.multiply(10);
			}
			boolean result = numberOfCountedDecimalPlaces > decimalPlacesBound;
			return result;
		}

		private boolean requiresScientificFormat() {
			
			boolean isDisplayIntegerPartScientific = numberOfIntegerPlacesIsGreaterThan(integerPart, displayScientificGreaterNIntegerPlaces);
			
			boolean isDisplayFractPartScientific = false;
			if (!isDisplayIntegerPartScientific) {
				isDisplayFractPartScientific = numberOfDecimalPlacesIsGreaterThan(fractionalPart, displayScientificAfterNDecimalPlaces);
			}
			
			boolean result = isDisplayIntegerPartScientific || isDisplayFractPartScientific;
			
			return result;
		}
	}

	@Override
	public SyntaxTree replaceSubTrees(Function<SyntaxTree, SyntaxTree> replacementFunction) {
		return this;
	}

	@Override
	public SyntaxTree clone() {
		return new DefaultSyntaxLeaf(getValue(), isStringLiteral());
	}

	private static boolean dontAcceptSymbolValueToBeExpression = true;
	public static boolean setDontAcceptSymbolValueToBeExpression(boolean newValue) {
		boolean oldValue = dontAcceptSymbolValueToBeExpression;
		dontAcceptSymbolValueToBeExpression = newValue;
		return oldValue;
	}
	
	//
	// PRIVATE METHODS
	//
	// Note: Can only instantiate Symbols via the factory method.
	private DefaultSyntaxLeaf(Object value, boolean isStringLiteral) {
		if (value instanceof Expression) {
			if (dontAcceptSymbolValueToBeExpression) {
				throw new Error("DefaultSyntaxLeaf received an expression: " + value);
			}	
		}
		
		this.valueOrRootSyntaxTree = value;
		this.isStringLiteral       = isStringLiteral;
	}
	
	private static boolean isSymbolValueQuotingRequired(String symbolValue) {
		boolean result = false;
		
		if (!_specialFunctorSymbols.contains(symbolValue)) {
			result = !UNQUOTED_SYMBOLIC_NAME.matcher(symbolValue).matches();
		}
		return result;
	}
	
	private static String removeTrailingZerosToRight(String number) {
		String result = number;
		int dot = result.indexOf('.');
		int end = result.length();
		
		if (dot >= 0) {
			for (int i = end-1; i > dot; i--) {
				if (result.charAt(i) == '0') {
					end--;
				}
				else {
					break;
				}
			}
			result = result.substring(0, end);
		}
		// 11.0 would get converted to 11. 
		// This logic ensures the trailing dot is removed.
		if (result.endsWith(".")) {
			result = result.substring(0, result.length()-1);
		}
	
		return result;
	}
	
	public static void flushGlobalSymbolTable() {
		if (AICUtilConfiguration.isRecordCacheStatistics()) {
			System.out.println("Global Symbol Table Cache Stats="+_globalSymbolTable.stats());
		}
		// Causes relevant flags to be reset.
		_useGlobalSymbolTable = ExpressoConfiguration.isUseGlobalSymbolTable();
		_cacheNumericSymbols  = ExpressoConfiguration.isGlobalSymbolTableToCacheNumerics();
		
		if (_globalSymbolTable != null) {
			_globalSymbolTable.invalidateAll();
		}
		if (_globalStringLiteralTable != null) {
			_globalStringLiteralTable.invalidateAll();
		}
		
		_globalSymbolTable        = newSymbolTable();
		_globalStringLiteralTable = newSymbolTable();
		// Add well known symbols to the table
		// The Booleans.
		_globalSymbolTable.put(true,          SYMBOL_TRUE);
		_globalSymbolTable.put(Boolean.TRUE,  SYMBOL_TRUE);
		_globalSymbolTable.put("true",        SYMBOL_TRUE);
		_globalSymbolTable.put(false,         SYMBOL_FALSE);
		_globalSymbolTable.put(Boolean.FALSE, SYMBOL_FALSE);
		_globalSymbolTable.put("false",       SYMBOL_FALSE);
		// Common Numbers
		_globalSymbolTable.put("0",             SYMBOL_0);
		_globalSymbolTable.put(new Integer(0),  SYMBOL_0);
		_globalSymbolTable.put(new Rational(0), SYMBOL_0);
		_globalSymbolTable.put("1",             SYMBOL_1);
		_globalSymbolTable.put(new Integer(1),  SYMBOL_1);
		_globalSymbolTable.put(new Rational(1), SYMBOL_1);
		_globalSymbolTable.put("2",             SYMBOL_2);
		_globalSymbolTable.put(new Integer(2),  SYMBOL_2);
		_globalSymbolTable.put(new Rational(2), SYMBOL_2);
		_globalSymbolTable.put("3",             SYMBOL_3);
		_globalSymbolTable.put(new Integer(3),  SYMBOL_3);
		_globalSymbolTable.put(new Rational(3), SYMBOL_3);
		_globalSymbolTable.put("4",             SYMBOL_4);
		_globalSymbolTable.put(new Integer(4),  SYMBOL_4);
		_globalSymbolTable.put(new Rational(4), SYMBOL_4);
		_globalSymbolTable.put("5",             SYMBOL_5);
		_globalSymbolTable.put(new Integer(5),  SYMBOL_5);
		_globalSymbolTable.put(new Rational(5), SYMBOL_5);
		_globalSymbolTable.put("6",             SYMBOL_6);
		_globalSymbolTable.put(new Integer(6),  SYMBOL_6);
		_globalSymbolTable.put(new Rational(6), SYMBOL_6);
		_globalSymbolTable.put("7",             SYMBOL_7);
		_globalSymbolTable.put(new Integer(7),  SYMBOL_7);
		_globalSymbolTable.put(new Rational(7), SYMBOL_7);
		_globalSymbolTable.put("8",             SYMBOL_8);
		_globalSymbolTable.put(new Integer(8),  SYMBOL_8);
		_globalSymbolTable.put(new Rational(8), SYMBOL_8);
		_globalSymbolTable.put("9",             SYMBOL_9);
		_globalSymbolTable.put(new Integer(9),  SYMBOL_9);
		_globalSymbolTable.put(new Rational(9), SYMBOL_9);
	}

	private static Cache<Object, SyntaxLeaf> newSymbolTable() {
		CacheBuilder<Object, Object> cb = CacheBuilder.newBuilder();
		
		long maximumSize = ExpressoConfiguration.getGlobalSymbolTableMaximumSize();
		// Note: a maximumSize of 
		// < 0 means no size restrictions
		// = 0 means no cache
		// > 0 means maximum size of cache
		if (maximumSize >= 0L) {			
			cb.maximumSize(maximumSize);
		}
		if (AICUtilConfiguration.isRecordCacheStatistics()) {
			cb.recordStats();
		}
		
		Cache<Object, SyntaxLeaf> result = cb.build();
		
		return result;
	}

	//
	// Well known static Symbols
	private static final SyntaxLeaf SYMBOL_TRUE  = SyntaxTrees.makeSyntaxLeaf(true);
	private static final SyntaxLeaf SYMBOL_FALSE = SyntaxTrees.makeSyntaxLeaf(false);
	private static final SyntaxLeaf SYMBOL_0     = SyntaxTrees.makeSyntaxLeaf(new Rational(0));
	private static final SyntaxLeaf SYMBOL_1     = SyntaxTrees.makeSyntaxLeaf(new Rational(1));
	private static final SyntaxLeaf SYMBOL_2     = SyntaxTrees.makeSyntaxLeaf(new Rational(2));
	private static final SyntaxLeaf SYMBOL_3     = SyntaxTrees.makeSyntaxLeaf(new Rational(3));
	private static final SyntaxLeaf SYMBOL_4     = SyntaxTrees.makeSyntaxLeaf(new Rational(4));
	private static final SyntaxLeaf SYMBOL_5     = SyntaxTrees.makeSyntaxLeaf(new Rational(5));
	private static final SyntaxLeaf SYMBOL_6     = SyntaxTrees.makeSyntaxLeaf(new Rational(6));
	private static final SyntaxLeaf SYMBOL_7     = SyntaxTrees.makeSyntaxLeaf(new Rational(7));
	private static final SyntaxLeaf SYMBOL_8     = SyntaxTrees.makeSyntaxLeaf(new Rational(8));
	private static final SyntaxLeaf SYMBOL_9     = SyntaxTrees.makeSyntaxLeaf(new Rational(9));
	//
	private static boolean                      _useGlobalSymbolTable     = ExpressoConfiguration.isUseGlobalSymbolTable();
	private static boolean                      _cacheNumericSymbols      = ExpressoConfiguration.isGlobalSymbolTableToCacheNumerics();
	private static Cache<Object, SyntaxLeaf>    _globalSymbolTable        = newSymbolTable();
	private static Cache<Object, SyntaxLeaf>    _globalStringLiteralTable = newSymbolTable();

	static {
		flushGlobalSymbolTable();
	}
}
