package com.sri.ai.grinder.plaindpll.theory;

import java.util.Collection;
import java.util.LinkedHashMap;

import com.sri.ai.expresso.api.Expression;

/** A convenience implementation of {@link NonEqualitiesForSingleTerm}. */
@SuppressWarnings("serial")
public class LinkedHashNonEqualitiesForSingleVariable
extends LinkedHashMap<String, Collection<Expression>> implements NonEqualitiesForSingleTerm {
}