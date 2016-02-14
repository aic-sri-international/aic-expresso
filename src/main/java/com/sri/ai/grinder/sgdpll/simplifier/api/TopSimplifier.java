package com.sri.ai.grinder.sgdpll.simplifier.api;


/**
 * A type of {@link Simplifier} that only simplifies the top expression
 * (that is, does not recurse into sub-expressions).
 * <p>
 * This property is important if the simplifier is being used by
 * code that is taking care of the recursion itself.
 *  
 * @author braz
 *
 */
public interface TopSimplifier extends Simplifier {
}