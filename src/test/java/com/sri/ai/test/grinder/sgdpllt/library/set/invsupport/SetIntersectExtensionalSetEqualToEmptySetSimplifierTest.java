package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.SetIntersectExtensionalSetEqualToEmptySetSimplifier;

public class SetIntersectExtensionalSetEqualToEmptySetSimplifierTest {
	private SetIntersectExtensionalSetEqualToEmptySetSimplifier simplifier = new SetIntersectExtensionalSetEqualToEmptySetSimplifier();

private Context context = new TrueContext();
	
	@Test
	public void test1() {
		Assert.assertEquals(
			parse("false"), 
			simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection {}) = {}"), context));
	}

	@Test
	public void test2() {
		Assert.assertEquals(
				parse("not(A in Union({{(on I in 1..10) {I} }})) and ((Union({{(on I in 1..10) {I} }}) intersection {B, C}) = {})"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection {A, B, C}) = {}"), context));
	}	
	
	@Test
	public void test3() {
		Assert.assertEquals(
				parse("not(A in Union({{(on I in 1..10) {I} }})) and (Union({{(on I in 1..10) {I} }}) = {})"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection {A}) = {}"), context));
	}
	
	@Test
	public void test4() {
		Assert.assertEquals(
				parse("not(A in Union({{(on I in 1..10) {I} }})) and not(A in Union({{(on J in 11..20) {J} }})) and ((Union({{(on I in 1..10) {I} }}) intersection Union({{(on J in 11..20) {J} }}) intersection {B, C}) = {})"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection Union({{(on J in 11..20) {J} }}) intersection {A, B, C}) = {}"), context));
	}
	
	@Test
	public void test5() {
		Assert.assertEquals(
				parse("not(A in Union({{(on I in 1..10) {I} }})) and not(A in Union({{(on J in 11..20) {J} }})) and ((Union({{(on I in 1..10) {I} }}) intersection Union({{(on J in 11..20) {J} }})) = {})"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection Union({{(on J in 11..20) {J} }}) intersection {A}) = {}"), context));
	}
}
