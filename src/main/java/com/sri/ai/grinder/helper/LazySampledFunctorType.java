package com.sri.ai.grinder.helper;

import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.FunctionType;

/**
 * 
 * @author oreilly
 *
 */
public class LazySampledFunctorType extends FunctionType {
	private static final long serialVersionUID = 1L;

	public LazySampledFunctorType(Type codomain, Type... argumentTypes) {
		super(codomain, argumentTypes);
	}
	
	@Override
	public boolean isSampleUniquelyNamedConstantSupported() {
		return true;
	}

	@Override
	public Expression sampleUniquelyNamedConstant(Random random) {
		return new LazySampledFunctor(this, new Random(random.nextInt()));
	}
}
