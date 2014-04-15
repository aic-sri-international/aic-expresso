package com.sri.ai.grinder.api;

/**
 * A module is an entity concentrating user-defined <i>providers<i> of some sort of knowledge about {@link Expression}s.
 * Providers must be <i>registered<i> with the provider.
 * Later, code may ask the module questions about a specific topic, and the module consults the multiple providers registered with it about it.
 * This is a way of extending functionality of a library while adding user code only, without changing code in the library itself.
 * 
 * @author braz
 *
 */
public interface Module {

	public interface Provider { }

	public void register(Provider provider);
}
