/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.test.grinder.helper;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.grinder.helper.GrinderUtil.INTEGER_TYPE;
import static com.sri.ai.grinder.helper.GrinderUtil.REAL_TYPE;
import static com.sri.ai.grinder.helper.GrinderUtil.isTypeSubtypeOf;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.map;

import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.core.DefaultRegistry;
import com.sri.ai.grinder.helper.GrinderUtil;

public class GrinderUtilTest {
	Registry registry;
	Type peopleType = new Categorical("People", 4, arrayList(makeSymbol("oscar"), makeSymbol("mary")));
	Type petsType = new Categorical("Pets", 3, arrayList(makeSymbol("fido"), makeSymbol("purrs")));
	
	@Before
	public void setUp() {
		registry = new DefaultRegistry();
		Symbol x = makeSymbol("X");
		Symbol y = makeSymbol("Y");
		Symbol myPeopleTypeExpression = makeSymbol(peopleType.getName());
		Symbol myPetsTypeExpression = makeSymbol(petsType.getName());
		registry = registry.makeCloneWithAddedType(peopleType);
		registry = registry.makeCloneWithAddedType(petsType);
		registry = registry.makeCloneWithAdditionalRegisteredSymbolsAndTypes(map(x, myPeopleTypeExpression, y, myPetsTypeExpression));
	}
	
	@Test
	public void testGetTypeForTupleType() {
		Assert.assertEquals(parse("(People x Pets)"), GrinderUtil.getTypeExpressionOfExpression(parse("(X, Y)"), registry));		
	}
	
	@Test
	public void testFromTypeExpressionToItsIntrinsicMeaningForTupleType() {
		Assert.assertEquals(new TupleType(peopleType, petsType), 
				GrinderUtil.fromTypeExpressionToItsIntrinsicMeaning(parse("(People x Pets)"), registry));
	}
	
	@Test
	public void testIsBooleanTypeSubtypeOf() {
		Assert.assertTrue(isTypeSubtypeOf(BOOLEAN_TYPE, BOOLEAN_TYPE));		
		Assert.assertFalse(isTypeSubtypeOf(BOOLEAN_TYPE, INTEGER_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(BOOLEAN_TYPE, REAL_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(BOOLEAN_TYPE, new IntegerInterval("Integer")));
		Assert.assertFalse(isTypeSubtypeOf(BOOLEAN_TYPE, new IntegerInterval("0..4")));
		Assert.assertFalse(isTypeSubtypeOf(BOOLEAN_TYPE, new RealInterval("Real")));
		Assert.assertFalse(isTypeSubtypeOf(BOOLEAN_TYPE, new RealInterval("[-0.5;4.5]")));
		Assert.assertFalse(isTypeSubtypeOf(BOOLEAN_TYPE, new FunctionType(BOOLEAN_TYPE)));
		Assert.assertFalse(isTypeSubtypeOf(BOOLEAN_TYPE, new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE)));
	}
	
	@Test
	public void testIsIntegerTypeSubtypeOf() {
		Assert.assertFalse(isTypeSubtypeOf(INTEGER_TYPE, BOOLEAN_TYPE));		
		Assert.assertTrue(isTypeSubtypeOf(INTEGER_TYPE, INTEGER_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(INTEGER_TYPE, REAL_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(INTEGER_TYPE, new IntegerInterval("Integer")));
		Assert.assertFalse(isTypeSubtypeOf(INTEGER_TYPE, new IntegerInterval("0..4")));
		Assert.assertTrue(isTypeSubtypeOf(INTEGER_TYPE, new IntegerInterval("-infinity..infinity")));
		Assert.assertTrue(isTypeSubtypeOf(INTEGER_TYPE, new RealInterval("Real")));
		Assert.assertTrue(isTypeSubtypeOf(INTEGER_TYPE, new RealInterval("[-infinity;infinity]")));
		Assert.assertTrue(isTypeSubtypeOf(INTEGER_TYPE, new RealInterval("]-infinity;infinity]")));
		Assert.assertTrue(isTypeSubtypeOf(INTEGER_TYPE, new RealInterval("[-infinity;infinity[")));
		Assert.assertTrue(isTypeSubtypeOf(INTEGER_TYPE, new RealInterval("]-infinity;infinity[")));
		Assert.assertFalse(isTypeSubtypeOf(INTEGER_TYPE, new RealInterval("[-0.5;4.5]")));
		Assert.assertFalse(isTypeSubtypeOf(INTEGER_TYPE, new FunctionType(INTEGER_TYPE)));
		Assert.assertFalse(isTypeSubtypeOf(INTEGER_TYPE, new FunctionType(INTEGER_TYPE, INTEGER_TYPE)));
	}
	
	@Test
	public void testIsRealTypeSubtypeOf() {
		Assert.assertFalse(isTypeSubtypeOf(REAL_TYPE, BOOLEAN_TYPE));		
		Assert.assertFalse(isTypeSubtypeOf(REAL_TYPE, INTEGER_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(REAL_TYPE, REAL_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(REAL_TYPE, new IntegerInterval("Integer")));
		Assert.assertFalse(isTypeSubtypeOf(REAL_TYPE, new IntegerInterval("0..4")));
		Assert.assertTrue(isTypeSubtypeOf(REAL_TYPE, new RealInterval("Real")));
		Assert.assertTrue(isTypeSubtypeOf(REAL_TYPE, new RealInterval("[-infinity;infinity]")));
		Assert.assertTrue(isTypeSubtypeOf(REAL_TYPE, new RealInterval("]-infinity;infinity]")));
		Assert.assertTrue(isTypeSubtypeOf(REAL_TYPE, new RealInterval("[-infinity;infinity[")));
		Assert.assertTrue(isTypeSubtypeOf(REAL_TYPE, new RealInterval("]-infinity;infinity[")));
		Assert.assertFalse(isTypeSubtypeOf(REAL_TYPE, new RealInterval("[-0.5;4.5]")));
		Assert.assertFalse(isTypeSubtypeOf(REAL_TYPE, new FunctionType(REAL_TYPE)));
		Assert.assertFalse(isTypeSubtypeOf(REAL_TYPE, new FunctionType(REAL_TYPE, REAL_TYPE)));
	}
	
	@Test
	public void testIsCategoricalTypeSubtypeOf() {
		Categorical categoricalType = new Categorical("TestCategorical1", 10, parse("a"), parse("b"), parse("c"));
		Assert.assertFalse(isTypeSubtypeOf(categoricalType, BOOLEAN_TYPE));		
		Assert.assertFalse(isTypeSubtypeOf(categoricalType, INTEGER_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(categoricalType, REAL_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(categoricalType, new IntegerInterval("Integer")));
		Assert.assertFalse(isTypeSubtypeOf(categoricalType, new IntegerInterval("0..4")));
		Assert.assertFalse(isTypeSubtypeOf(categoricalType, new RealInterval("Real")));
		Assert.assertFalse(isTypeSubtypeOf(categoricalType, new RealInterval("[-0.5;4.5]")));
		Assert.assertFalse(isTypeSubtypeOf(categoricalType, new FunctionType(categoricalType)));
		Assert.assertFalse(isTypeSubtypeOf(categoricalType, new FunctionType(categoricalType, categoricalType)));
		
		// NOTE: Categorical types equality is based on name.
		Assert.assertTrue(isTypeSubtypeOf(categoricalType,  new Categorical("TestCategorical1", 10, parse("a"), parse("b"), parse("c"))));
		Assert.assertTrue(isTypeSubtypeOf(categoricalType,  new Categorical("TestCategorical1", 5, parse("x"), parse("y"), parse("z"))));
		
		Assert.assertFalse(isTypeSubtypeOf(categoricalType,  new Categorical("TestCategorical2", 10, parse("a"), parse("b"), parse("c"))));
	}
	
	@Test
	public void testIsIntegerIntervalSubtypeOf() {
		IntegerInterval intInterval = new IntegerInterval("Integer");
		Assert.assertFalse(isTypeSubtypeOf(intInterval, BOOLEAN_TYPE));		
		Assert.assertTrue(isTypeSubtypeOf(intInterval, INTEGER_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, REAL_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new IntegerInterval("Integer")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("0..4")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("Real")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[-0.5;4.5]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new FunctionType(intInterval)));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new FunctionType(intInterval, intInterval)));
		
		intInterval = new IntegerInterval("0..4");
		Assert.assertTrue(isTypeSubtypeOf(intInterval, INTEGER_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new IntegerInterval("0..4")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("1..4")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("0..3")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, REAL_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("Real")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("[0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("]0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[0;4[")));
		
		intInterval = new IntegerInterval("-infinity..4");
		Assert.assertTrue(isTypeSubtypeOf(intInterval, INTEGER_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("0..4")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("1..4")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("0..3")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new IntegerInterval("-infinity..5")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, REAL_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("Real")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("]0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[0;4[")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("[-infinity;4]")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("]-infinity;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[-infinity;4[")));
		
		intInterval = new IntegerInterval("0..infinity");
		Assert.assertTrue(isTypeSubtypeOf(intInterval, INTEGER_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("0..4")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("1..4")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("0..3")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new IntegerInterval("-1..infinity")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, REAL_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("Real")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("]0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[0;4[")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("[0;infinity]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("]0;infinity]")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("[0;infinity[")));
		
		intInterval = new IntegerInterval("-infinity..infinity");
		Assert.assertTrue(isTypeSubtypeOf(intInterval, INTEGER_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("0..4")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("1..4")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new IntegerInterval("0..3")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, REAL_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("Real")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("]0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[0;4[")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[-infinity;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("]-infinity;4]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[-infinity;4[")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[0;infinity]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("]0;infinity]")));
		Assert.assertFalse(isTypeSubtypeOf(intInterval, new RealInterval("[0;infinity[")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("[-infinity;infinity]")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("]-infinity;infinity]")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("[-infinity;infinity[")));
		Assert.assertTrue(isTypeSubtypeOf(intInterval, new RealInterval("]-infinity;infinity[")));
	}
	
	@Test
	public void testIsRealIntervalSubtypeOf() {
		RealInterval realInterval = new RealInterval("Real");
		Assert.assertFalse(isTypeSubtypeOf(realInterval, BOOLEAN_TYPE));		
		Assert.assertFalse(isTypeSubtypeOf(realInterval, INTEGER_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, REAL_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("Integer")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("0..4")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("Real")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[-0.5;4.5]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new FunctionType(realInterval)));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new FunctionType(realInterval, realInterval)));
		
		realInterval = new RealInterval("[0;4]");
		Assert.assertFalse(isTypeSubtypeOf(realInterval, INTEGER_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("0..4")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("1..4")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("0..3")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, REAL_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("Real")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("[0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("]0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[0;4[")));
		
		realInterval = new RealInterval("[-infinity;4]");
		Assert.assertFalse(isTypeSubtypeOf(realInterval, INTEGER_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("0..4")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("1..4")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("0..3")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("-infinity..5")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, REAL_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("Real")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("]0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[0;4[")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("[-infinity;4]")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("]-infinity;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[-infinity;4[")));
		
		realInterval = new RealInterval("[0;infinity]");
		Assert.assertFalse(isTypeSubtypeOf(realInterval, INTEGER_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("0..4")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("1..4")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("0..3")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("-1..infinity")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, REAL_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("Real")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("]0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[0;4[")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("[0;infinity]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("]0;infinity]")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("[0;infinity[")));
		
		realInterval = new RealInterval("[-infinity;infinity]");
		Assert.assertFalse(isTypeSubtypeOf(realInterval, INTEGER_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("0..4")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("1..4")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new IntegerInterval("0..3")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, REAL_TYPE));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("Real")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("]0;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[0;4[")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[-infinity;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("]-infinity;4]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[-infinity;4[")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[0;infinity]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("]0;infinity]")));
		Assert.assertFalse(isTypeSubtypeOf(realInterval, new RealInterval("[0;infinity[")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("[-infinity;infinity]")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("]-infinity;infinity]")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("[-infinity;infinity[")));
		Assert.assertTrue(isTypeSubtypeOf(realInterval, new RealInterval("]-infinity;infinity[")));
	}
	
	@Test
	public void testIsFunctionTypeSubtypeOf() {
		Assert.assertTrue(isTypeSubtypeOf(new FunctionType(BOOLEAN_TYPE), new FunctionType(BOOLEAN_TYPE)));
		Assert.assertTrue(isTypeSubtypeOf(new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE), new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE)));

		Assert.assertFalse(isTypeSubtypeOf(new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE), BOOLEAN_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(new FunctionType(BOOLEAN_TYPE), new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE)));
		Assert.assertFalse(isTypeSubtypeOf(new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE), new FunctionType(BOOLEAN_TYPE)));

		Assert.assertTrue(isTypeSubtypeOf(new FunctionType(INTEGER_TYPE), new FunctionType(REAL_TYPE)));
		Assert.assertFalse(isTypeSubtypeOf(new FunctionType(REAL_TYPE), new FunctionType(INTEGER_TYPE)));
				
		Assert.assertTrue(isTypeSubtypeOf(new FunctionType(INTEGER_TYPE, REAL_TYPE), new FunctionType(REAL_TYPE, INTEGER_TYPE)));
		Assert.assertFalse(isTypeSubtypeOf(new FunctionType(INTEGER_TYPE, INTEGER_TYPE), new FunctionType(REAL_TYPE, REAL_TYPE)));
	}
	
	@Test
	public void testIsTupleTypeSubtypeOf() {
		Assert.assertTrue(isTypeSubtypeOf(new TupleType(BOOLEAN_TYPE), new TupleType(BOOLEAN_TYPE)));
		Assert.assertTrue(isTypeSubtypeOf(new TupleType(BOOLEAN_TYPE, BOOLEAN_TYPE), new TupleType(BOOLEAN_TYPE, BOOLEAN_TYPE)));

		Assert.assertFalse(isTypeSubtypeOf(new TupleType(BOOLEAN_TYPE, BOOLEAN_TYPE), BOOLEAN_TYPE));
		Assert.assertFalse(isTypeSubtypeOf(new TupleType(BOOLEAN_TYPE), new TupleType(BOOLEAN_TYPE, BOOLEAN_TYPE)));
		Assert.assertFalse(isTypeSubtypeOf(new TupleType(BOOLEAN_TYPE, BOOLEAN_TYPE), new TupleType(BOOLEAN_TYPE)));

		Assert.assertTrue(isTypeSubtypeOf(new TupleType(INTEGER_TYPE), new TupleType(REAL_TYPE)));
		Assert.assertFalse(isTypeSubtypeOf(new TupleType(REAL_TYPE), new TupleType(INTEGER_TYPE)));
				
		Assert.assertFalse(isTypeSubtypeOf(new TupleType(INTEGER_TYPE, REAL_TYPE), new TupleType(REAL_TYPE, INTEGER_TYPE)));
		Assert.assertTrue(isTypeSubtypeOf(new TupleType(INTEGER_TYPE, INTEGER_TYPE), new TupleType(REAL_TYPE, REAL_TYPE)));
	}
}
