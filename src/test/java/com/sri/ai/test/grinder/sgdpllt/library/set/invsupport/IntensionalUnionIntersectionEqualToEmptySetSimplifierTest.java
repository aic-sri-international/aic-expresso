package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.IntensionalUnionIntersectionEqualToEmptySetSimplifier;

public class IntensionalUnionIntersectionEqualToEmptySetSimplifierTest {
	private IntensionalUnionIntersectionEqualToEmptySetSimplifier simplifier = new IntensionalUnionIntersectionEqualToEmptySetSimplifier();
	
private Context context = new TrueContext();
	
	@Test
	public void test1() {
		Assert.assertEquals(
				parse("for all I in 1..10 : true => for all J in 11..20 : true => ({ I } intersection { J } = {  })"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection Union({{(on J in 11..20) {J} }})) = {}"), context));
	}
	
	@Test
	public void test2() {
		Assert.assertEquals(
				parse("for all I in 1..10 : I > 4 => for all J in 11..20 : J > 14 => ({ I } intersection { J } = {  })"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} : I > 4 }}) intersection Union({{(on J in 11..20) {J} : J > 14}})) = {}"), context));
	}
	
	@Test
	public void test3() {
		Assert.assertEquals(
				parse("for all I in 1..10 : true => for all J in 11..20 : true => for all K in 21..30 : true => ({ I } intersection { J } intersection {K} = {  })"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection Union({{(on J in 11..20) {J} }}) intersection Union({{(on K in 21..30) {K} }})) = {}"), context));
	}
	
	@Test
	public void test4() {
		Assert.assertEquals(
				parse("for all I in 1..10 : I > 4 => for all J in 11..20 : J > 14 => for all K in 21..30 : K > 24 => ({ I } intersection { J } intersection {K} = {  })"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} : I > 4}}) intersection Union({{(on J in 11..20) {J} : J > 14}}) intersection Union({{(on K in 21..30) {K} : K > 24}})) = {}"), context));
	}
	
	@Test
	public void test5() {
		Assert.assertEquals(
				parse("for all I in 1..10 : true => (for all J in 11..20 : true => for all K in 21..30 : true => ({ I } intersection { J, K } = {  }))"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection Union({{(on J in 11..20, K in 21..30) {J, K} }})) = {}"), context));
	}
	
	@Test
	public void test6() {
		Assert.assertEquals(
				parse("for all I in 1..10 : I > 4 => for all J in 11..20 : true => for all K in 21..30 : J > 14 and K > 24 => ({ I } intersection { J, K } = {  })"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} : I > 4}}) intersection Union({{(on J in 11..20, K in 21..30) {J, K} : J > 14 and K > 24}})) = {}"), context));
	}
	
	@Test
	public void test7() {
		Assert.assertEquals(
				parse("for all I in 1..10 : true => for all I' in 11..20 : true => ({ I } intersection { I' } = {  })"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection Union({{(on I in 11..20) {I} }})) = {}"), context));
	}
	
	@Test
	public void test8() {
		Assert.assertEquals(
				parse("for all I in 1..10 : I > 4 => for all I' in 11..20 : I' > 14 => ({ I } intersection { I' } = {  })"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} : I > 4 }}) intersection Union({{(on I in 11..20) {I} : I > 14}})) = {}"), context));
	}
	
	@Test
	public void test9() {
		Assert.assertEquals(
				parse("for all I in 1..10 : true => (for all I' in 11..20 : true => for all J in 21..30 : true => ({ I } intersection { I', J } = {  }))"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection Union({{(on I in 11..20, J in 21..30) {I, J} }})) = {}"), context));
	}
	
	@Test
	public void test10() {
		Assert.assertEquals(
				parse("for all I in 1..10 : I > 4 => for all I' in 11..20 : true => for all J in 21..30 : I' > 14 and J > 24 => ({ I } intersection { I', J } = {  })"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} : I > 4}}) intersection Union({{(on I in 11..20, J in 21..30) {I, J} : I > 14 and J > 24}})) = {}"), context));
	}
}
