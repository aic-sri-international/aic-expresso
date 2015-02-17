package com.sri.ai.expresso.core;

import java.util.concurrent.ConcurrentHashMap;

import com.sri.ai.grinder.api.Module;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;

/**
 * A abstract module implements basic functionality of a module for keeping a list of providers,
 * registering them and clearing them when a {@link RewritingProcess} is finalized.
 * Because only {@link Rewriter}s receive notification of finalization, this extends {@link AbstractRewriter}.
 * In the future, we should modify {@link RewritingProcess} to notify {@link Module}s as well to dispense with the need for modules to be rewriters.
 * 
 * Most modules are {@link NoOpRewriter}s, but some are not. This class does not commit to being a {@link NoOpRewriter}.
 * Its extension {@link AbstractModuleNoOpRewriter} does, and is the one most often used for creating modules.
 * 
 * @author braz
 *
 */
public abstract class AbstractModuleAndPossibleActiveRewriter extends AbstractRewriter implements Module {

	protected ConcurrentHashMap<Module.Provider, Module.Provider> providers = new ConcurrentHashMap<Module.Provider, Module.Provider>();

	public AbstractModuleAndPossibleActiveRewriter() {
		super();
	}

	/**
	 * Registers a {@link Provider} in the module which is an instance of moduleClass in the given process.
	 * This is typically used for extending classes, not user code.
	 * User code usually uses a method in the extending class that dispenses the need for providing moduleClass.
	 * For example, if FooModule is an extending class, it will typically provide a method
	 * <code>public static void register(Provider provider, RewritingProcess process)</code>.
	 */
	public static void register(Class moduleClass, Provider provider, RewritingProcess process) throws Error {
		Module module = (Module) process.findModule(moduleClass);
		if (module != null) {
			module.register(provider);
		}
//		else {
//			throw new Error(provider.getClass() + " wants to register as provider to " + moduleClass + " but cannot find one in current process");
//		}
// this test is too strict as some providers are useful for other things and may be used when the respective module is not present.		
	}

	@Override
	public void register(Module.Provider provider) {
		if ( ! providers.contains(provider)) {
			providers.put(provider, provider);
		}
	}

	@Override
	public void rewritingProcessFinalized(RewritingProcess process) {
		providers.clear();
	}
}