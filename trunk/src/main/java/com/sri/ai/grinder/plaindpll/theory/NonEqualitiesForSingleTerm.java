package com.sri.ai.grinder.plaindpll.theory;

import java.util.Collection;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;

/**
 * A convenient name for Map<String, Collection<Expression>>,
 * meant to represent terms constrained against a single term (not represented),
 * where each entry (functorString, termCollection)
 * indicates that the terms in termCollection are constrainted against the single
 * term by the constraint operation with functorString.
 */
 public interface NonEqualitiesForSingleTerm extends Map<String, Collection<Expression>> {}
