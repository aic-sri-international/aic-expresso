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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import com.google.common.annotations.Beta;
import com.google.common.collect.ImmutableList;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;

/**
 * A default implementation of a {@link IndexExpressionsSet}.
 * 
 * @author braz
 */
@Beta
public class DefaultIndexExpressionsSet implements IndexExpressionsSet {

	private ImmutableList<Expression> list;
	
	public DefaultIndexExpressionsSet(List<Expression> indexExpressions) {
		super();
		list = new ImmutableList.Builder<Expression>().addAll(indexExpressions).build();
	}

	@Override
	public boolean add(Expression e) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void add(int index, Expression element) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean addAll(Collection<? extends Expression> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean addAll(int index, Collection<? extends Expression> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void clear() {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean contains(Object o) {
		return list.contains(o);
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		return list.containsAll(c);
	}

	@Override
	public Expression get(int index) {
		return list.get(index);
	}

	@Override
	public int indexOf(Object o) {
		return list.indexOf(o);
	}

	@Override
	public boolean isEmpty() {
		return list.isEmpty();
	}

	@Override
	public Iterator<Expression> iterator() {
		return list.iterator();
	}

	@Override
	public int lastIndexOf(Object o) {
		return list.lastIndexOf(o);
	}

	@Override
	public ListIterator<Expression> listIterator() {
		return list.listIterator();
	}

	@Override
	public ListIterator<Expression> listIterator(int index) {
		return list.listIterator(index);
	}

	@Override
	public boolean remove(Object o) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Expression remove(int index) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Expression set(int index, Expression element) {
		throw new UnsupportedOperationException();
	}

	@Override
	public int size() {
		return list.size();
	}

	@Override
	public List<Expression> subList(int fromIndex, int toIndex) {
		return list.subList(fromIndex, toIndex);
	}

	@Override
	public Object[] toArray() {
		return list.toArray();
	}

	@Override
	public <T> T[] toArray(T[] a) {
		return list.toArray(a);
	}
}
