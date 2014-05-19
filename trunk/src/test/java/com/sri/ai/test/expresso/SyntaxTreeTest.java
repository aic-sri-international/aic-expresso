package com.sri.ai.test.expresso;

import static org.junit.Assert.assertEquals;

import java.util.Collections;
import java.util.List;

import org.junit.Test;

import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.util.Util;

public class SyntaxTreeTest {
	@SuppressWarnings("unchecked")
	@Test
	public void testOrdering() {
		SyntaxTree a;
		SyntaxTree b;
		SyntaxTree c;
		List<SyntaxTree> list;
		
		a = SyntaxTrees.parse("a");
		b = SyntaxTrees.parse("b");
		c = SyntaxTrees.parse("c");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(a, b, c), list);
		
		a = SyntaxTrees.parse("f()");
		b = SyntaxTrees.parse("f");
		c = SyntaxTrees.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(b, a, c), list);
		
		a = SyntaxTrees.parse("f(a, b, c)");
		b = SyntaxTrees.parse("f(c, b, a)");
		c = SyntaxTrees.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(a, b, c), list);
		
		a = SyntaxTrees.parse("f(a, b, c)");
		b = SyntaxTrees.parse("f(a)");
		c = SyntaxTrees.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(b, a, c), list);
		
		a = SyntaxTrees.parse("f(a, b, c)");
		b = SyntaxTrees.parse("f(c)");
		c = SyntaxTrees.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(a, b, c), list);
		
		a = SyntaxTrees.parse("f(a(c,b), b, c)");
		b = SyntaxTrees.parse("f(a(b,c), b, c)");
		c = SyntaxTrees.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(b, a, c), list);
		
		a = SyntaxTrees.parse("f(a(b,c), f(a(b,a)), c)");
		b = SyntaxTrees.parse("f(a(b,c), f(a(a,b)), c)");
		c = SyntaxTrees.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(b, a, c), list);
		
		// Symbol labels f and g come first
		a = SyntaxTrees.parse("f   (a(b,c), f(a(a,b)), c)");
		b = SyntaxTrees.parse("f(x)(a(a,a), f(a(a,b)), c)");
		c = SyntaxTrees.parse("g()");
		list = Util.list(a, b, c);
		Collections.sort(list);
		assertEquals(Util.list(a, c, b), list);
	}
}
