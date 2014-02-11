package com.sri.ai.grinder.core;

import com.sri.ai.grinder.api.Rewriter;

/**
 * An interface to be implemented by classes that can provide rewriters
 * based on name to the RewritingProcess for use by its rewrite() methods.
 * 
 * @author oreilly
 * 
 */
public interface RewriterLookup {
	/**
	 * 
	 * @param rewriterName
	 *            the name of the rewriter to be looked up.
	 * @return the rewriter corresponding to the rewriterName provided.
	 */
	Rewriter getRewriterFor(String rewriterName);
}