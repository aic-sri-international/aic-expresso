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
package com.sri.ai.grinder.library;

import java.util.Collection;
import java.util.LinkedHashSet;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.ThereExists;

/**
 * 
 * @author braz
 *
 */
@Beta
public class FunctorConstants {
	public final static String PRODUCT = "product";
	public final static String SUM = "sum";
	public final static String THERE_EXISTS  = ThereExists.LABEL;
	public final static String FOR_ALL  = ForAll.LABEL;
	public final static String PLUS  = "+";
	public static final String MINUS = "-";
	public final static String TIMES  = "*";
	public static final String DIVISION = "/";
	public final static String EXPONENTIATION = "^";
	public final static String EQUIVALENCE  = "<=>";
	public final static String IMPLICATION  = "=>";
	public final static String EQUAL  = "=";
	public final static String INEQUALITY = "!=";
	public final static String GREATER_THAN = ">";
	//public final static String SET  = "{ . . . }";
	//public final static String MULTISET  = "{{ . . . }}";
	public final static String UNION = "union";
	public final static String PARTITION = "partition";
	public final static String SET_DIFFERENCE = "\\";
	public final static String INTERSECTION = "intersection";
	public final static String CARDINALITY = "| . |";
	public final static String ON  = "( on . )";
	public final static String VALUE_OF  = "value of";
	public final static String LEFT_DOT_RIGHT  = "[ . ]";
	public final static String KLEENE_LIST  = "kleene list";

	public final static String AND  = "and";
	public final static String OR  = "or";
	public final static String NOT = "not";
	public final static String TRUE  = "true";
	public final static String FALSE  = "false";
	public static final Collection<String> BOOLEAN_FUNCTORS;
	static {
		BOOLEAN_FUNCTORS = new LinkedHashSet<String>();
		BOOLEAN_FUNCTORS.add(AND);
		BOOLEAN_FUNCTORS.add(OR);
		BOOLEAN_FUNCTORS.add(IMPLICATION);
		BOOLEAN_FUNCTORS.add(EQUIVALENCE);
		BOOLEAN_FUNCTORS.add(NOT);
	}
}
