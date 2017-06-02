package com.sri.ai.grinder.sgdpllt.anytime;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.util.Util.println;

import java.util.HashSet;
import java.util.Set;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.expresso.helper.Expressions.parse;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;

public class Examples {

	public static VariableComponent TriangleModel() {
		Expression func = DefaultSymbol.createSymbol("f");
		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression q = DefaultSymbol.createSymbol("Q");
		
		Expression f1 = apply(IF_THEN_ELSE, a, q, 5);
		Expression f2 = apply(IF_THEN_ELSE, b, q, 5);
		Expression f3 = apply(IF_THEN_ELSE, a, b, 5);
		

		Expression res = apply(func, q);
		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(res);

		Model m = new Model(Factor);

		VariableComponent ComponentResultat = new VariableComponent(q, res, m, new HashSet<Expression>());
		return ComponentResultat;
	}
	
	public static VariableComponent DiamondModel() {
		Expression func = DefaultSymbol.createSymbol("f");
		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression trueValue = DefaultSymbol.createSymbol(true);
		Expression f1 = apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 95, 5),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 5, 95));
		Expression f2 = apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 5, 95),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 95, 5));
		Expression f3 = apply(IF_THEN_ELSE, apply(EQUAL, c, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue), 60, 40),
				apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue), 40, 60));
		Expression f4 = apply(IF_THEN_ELSE, apply(EQUAL, c, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 50, 50),
				apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 50, 50));
		Expression f5 = apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue),
				1,0);

		Expression res = apply(func, q);
		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(f4);
		Factor.add(f5);
		Factor.add(res);

		Model m = new Model(Factor);

		VariableComponent ComponentResultat = new VariableComponent(q, res, m, new HashSet<Expression>());
		return ComponentResultat;
	}

	public static VariableComponent BasicBayesianLoopyModel() {
		Expression func = DefaultSymbol.createSymbol("f");
		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression q = DefaultSymbol.createSymbol("Q");
		
		Expression trueValue = DefaultSymbol.createSymbol(true);
		
		Expression f1 = IfThenElse.make(apply(EQUAL, q, trueValue), IfThenElse.make(a, parse("0.1"), parse("0.9")), IfThenElse.make(a, parse("0.9"), parse("0.1")));
		Expression f2 = apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 5, 95),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 95, 5));
		Expression f3 = apply(IF_THEN_ELSE, apply(EQUAL, c, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue), 60, 40),
				apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue), 40, 60));
		Expression f4 = apply(IF_THEN_ELSE, apply(EQUAL, c, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 50, 50),
				apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 50, 50));
		Expression f5 = apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue),
				1,0);

		Expression res = apply(func, q);
		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(f4);
		Factor.add(f5);
		Factor.add(res);

		Model m = new Model(Factor);

		VariableComponent ComponentResultat = new VariableComponent(q, res, m, new HashSet<Expression>());
		return ComponentResultat;
	}
	
	public static VariableComponent TreeModel() {
		Expression func = DefaultSymbol.createSymbol("f");
		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression trueValue = DefaultSymbol.createSymbol(true);
		Expression f1 = apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 95, 5),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 5, 95));
		Expression f2 = apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 5, 95),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 95, 5));
		Expression f3 = apply(IF_THEN_ELSE, apply(EQUAL, c, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue), 60, 40),
				apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue), 40, 60));
		Expression f4 = apply(IF_THEN_ELSE, apply(EQUAL, c, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 50, 50),
				apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 50, 50));
		Expression f5 = apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue),
				1,0);
		

		Expression res = apply(func, q);
		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		//Factor.add(f4);
		//Factor.add(f5);
		Factor.add(res);

		Model m = new Model(Factor);

		VariableComponent ComponentResultat = new VariableComponent(q, res, m, new HashSet<Expression>());
		return ComponentResultat;
	}
	
	public static VariableComponent DoubleDiamondModel() {
		Expression func = DefaultSymbol.createSymbol("f");

		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression d = DefaultSymbol.createSymbol("D");
		Expression e = DefaultSymbol.createSymbol("E");
		Expression f = DefaultSymbol.createSymbol("F");
		Expression g = DefaultSymbol.createSymbol("G");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression f1 = apply(IF_THEN_ELSE, a, q, 5);
		Expression f2 = apply(IF_THEN_ELSE, e, q, 5);
		Expression f3 = apply(IF_THEN_ELSE, g, q, 5);
		Expression f4 = apply(IF_THEN_ELSE, e, f, 5);
		Expression f5 = apply(IF_THEN_ELSE, g, f, 5);
		Expression f6 = apply(IF_THEN_ELSE, b, d, 5);
		Expression f7 = apply(IF_THEN_ELSE, c, d, 5);
		Expression f8 = apply(IF_THEN_ELSE, b, a, 5);
		Expression f9 = apply(IF_THEN_ELSE, c, a, 5);
		Expression res = apply(func, q);

		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(f4);
		Factor.add(f5);
		Factor.add(f6);
		Factor.add(f7);
		Factor.add(f8);
		Factor.add(f9);
		Factor.add(res);

		Model m = new Model(Factor);

		VariableComponent ComponentResultat = new VariableComponent(q, res, m, new HashSet<Expression>());
		return ComponentResultat;

	}

	public static VariableComponent Model1() {
		Expression func = DefaultSymbol.createSymbol("f");

		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression d = DefaultSymbol.createSymbol("D");
		Expression e = DefaultSymbol.createSymbol("E");
		Expression f = DefaultSymbol.createSymbol("F");
		Expression g = DefaultSymbol.createSymbol("G");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression f1 = apply(IF_THEN_ELSE, a, q, d);
		Expression f2 = apply(IF_THEN_ELSE, b, q, g);
		Expression f3 = apply(IF_THEN_ELSE, b, f, c);
		Expression f4 = apply(IF_THEN_ELSE, c, a, e);
		Expression res = apply(func, q);

		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(f4);
		Factor.add(res);

		Model m = new Model(Factor);

		VariableComponent ComponentResultat = new VariableComponent(q, res, m, new HashSet<Expression>());
		return ComponentResultat;

	}

	public static VariableComponent Model2() {

		Expression func = DefaultSymbol.createSymbol("f");

		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression d = DefaultSymbol.createSymbol("D");
		Expression e = DefaultSymbol.createSymbol("E");
		Expression f = DefaultSymbol.createSymbol("F");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression f1 = apply(IF_THEN_ELSE, a, q, b);
		Expression f2 = apply(IF_THEN_ELSE, b, a, e);
		Expression f3 = apply(IF_THEN_ELSE, a, d, c);
		Expression f4 = apply(IF_THEN_ELSE, d, e, f);
		Expression res = apply(func, q);

		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(f4);
		Factor.add(res);

		Model m = new Model(Factor);

		VariableComponent ComponentResultat = new VariableComponent(q, res, m, new HashSet<Expression>());
		return ComponentResultat;
	}

	public static VariableComponent Model3() {

		Expression func = DefaultSymbol.createSymbol("f");

		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression d = DefaultSymbol.createSymbol("D");
		Expression e = DefaultSymbol.createSymbol("E");
		Expression f = DefaultSymbol.createSymbol("F");
		Expression g = DefaultSymbol.createSymbol("G");
		Expression h = DefaultSymbol.createSymbol("H");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression f1 = apply(IF_THEN_ELSE, q, a, b);
		Expression f2 = apply(IF_THEN_ELSE, a, b, c);
		Expression f3 = apply(IF_THEN_ELSE, c, d, e);
		Expression f4 = apply(IF_THEN_ELSE, e, f, g);
		Expression f5 = apply(IF_THEN_ELSE, f, g, h);
		Expression res = apply(func, q);

		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(f4);
		Factor.add(f5);
		Factor.add(res);

		Model m = new Model(Factor);

		VariableComponent ComponentResultat = new VariableComponent(q, res, m, new HashSet<Expression>());
		return ComponentResultat;
	}

	public static void main(String[] args) {
			
		VariableComponent ComponentResultat = TreeModel();
		int nbIter = 0;
		ComponentResultat.model.context = ComponentResultat.model.context.extendWithSymbolsAndTypes("Q", "Boolean");
		while(!ComponentResultat.entirelyDiscover) {
			ComponentResultat.update(new HashSet<Expression>());
			nbIter ++;
		}
		
		System.out.println("Iteration necessary : " + nbIter);

		ComponentResultat.print(0);
		

		Expression naiveResult = ComponentResultat.naiveCalcul();
		System.out.println(" ");
		System.out.println(" ");
		System.out.println("Naive Result : " + naiveResult);
		Expression unnormalizedMessage = ComponentResultat.calculate();
		String string = "(" + unnormalizedMessage + ")/sum({{ (on "  + ComponentResultat.variable + " in Boolean) " + unnormalizedMessage + " }})";
		Expression normalizedMessage = ComponentResultat.model.theory.evaluate(parse(string), ComponentResultat.model.context);
		System.out.println(" ");
		System.out.println(" ");
		System.out.println("Our computation : " + normalizedMessage);
	}

}
