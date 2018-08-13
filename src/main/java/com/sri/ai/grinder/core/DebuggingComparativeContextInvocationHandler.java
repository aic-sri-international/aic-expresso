package com.sri.ai.grinder.core;

import static com.sri.ai.util.Util.println;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.lang.reflect.UndeclaredThrowableException;

import com.sri.ai.grinder.api.Context;

public class DebuggingComparativeContextInvocationHandler implements InvocationHandler {

	private Context context1;
	private Context context2;
	
	public DebuggingComparativeContextInvocationHandler(Context context1, Context context2) {
		this.context1 = context1;
		this.context2 = context2;
	}
	
	@Override
	public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
		
		Object result1 = null;
		Object result2 = null;
		
		try {
			result1 = method.invoke(context1, args);
			result2 = method.invoke(context2, args);
		}
		catch (UndeclaredThrowableException e) {
			println(e.getCause());
			System.exit(-1);
		}
		
		if (method.getName().startsWith("conjoin") && ((Context) result1).isContradiction() != ((Context) result2).isContradiction()) {
			println("Divergence in debugging comparative contexts");
			println(method);
			println("context1: " + context1);
			println("context2: " + context2);
			println("result1: " + result1);
			println("result2: " + result2);
		}
		
		if (result1 instanceof Context) {
			result1 = makeComparativeContext((Context) result1, (Context) result2);
		}
		
		return result1;
	}

	public static Context makeComparativeContext(Context context1, Context context2) {
		return (Context) Proxy.newProxyInstance(Context.class.getClassLoader(),
	            new Class<?>[] { Context.class },
	            new DebuggingComparativeContextInvocationHandler(context1, context2));
	}
}
