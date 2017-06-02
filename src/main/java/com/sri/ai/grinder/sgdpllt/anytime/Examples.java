package com.sri.ai.grinder.sgdpllt.anytime;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.util.Util.println;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.expresso.helper.Expressions.parse;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Equals;

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
		Expression f5 = apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 1, 0);

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

		Expression f1 = IfThenElse.make(apply(EQUAL, q, trueValue), IfThenElse.make(a, parse("0.1"), parse("0.9")),
				IfThenElse.make(a, parse("0.9"), parse("0.1")));
		Expression f2 = apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 5, 95),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 95, 5));
		Expression f3 = apply(IF_THEN_ELSE, apply(EQUAL, c, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue), 60, 40),
				apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue), 40, 60));
		Expression f4 = apply(IF_THEN_ELSE, apply(EQUAL, c, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 50, 50),
				apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 50, 50));
		Expression f5 = apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 1, 0);

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

		
		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);

		Model m = new Model(Factor);
		m.setType(a,  "Boolean");
		m.setType(b,  "Boolean");
		m.setType(c,  "Boolean");
		m.setType(q,  "Boolean");
		
		m.setValues(a,  "Boolean");
		m.setValues(b,  "Boolean");
		m.setValues(c,  "Boolean");
		m.setValues(q,  "Boolean");

		Expression f4 = apply(IF_THEN_ELSE, apply(EQUAL, c, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 50, 50),
				apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 50, 50));
		Expression f5 = apply(IF_THEN_ELSE, apply(EQUAL, a, trueValue), 1, 0);

		VariableComponent ComponentResultat = new VariableComponent(q, null, m, new HashSet<Expression>());
		return ComponentResultat;
	}
	
	public static VariableComponent TreeModelWithInteger() {
		Expression a = DefaultSymbol.createSymbol("A");
		Expression b = DefaultSymbol.createSymbol("B");
		Expression c = DefaultSymbol.createSymbol("C");
		Expression q = DefaultSymbol.createSymbol("Q");

		Expression trueValue = DefaultSymbol.createSymbol(true);
		Expression f1 = apply(IF_THEN_ELSE, apply(EQUAL, a, parse("1")),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 95, 5),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 5, 95));
		Expression f2 = apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 5, 95),
				apply(IF_THEN_ELSE, apply(EQUAL, q, trueValue), 95, 5));
		Expression f3 = apply(IF_THEN_ELSE, apply(EQUAL, c, trueValue),
				apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue), 60, 40),
				apply(IF_THEN_ELSE, apply(EQUAL, b, trueValue), 40, 60));
		
		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);

		Model m = new Model(Factor);
		m.setType(a,  "Integer");
		m.setType(b,  "Boolean");
		m.setType(c,  "Boolean");
		m.setType(q,  "Boolean");
		
		m.setValues(a,  "0..4");
		m.setValues(b,  "Boolean");
		m.setValues(c,  "Boolean");
		m.setValues(q,  "Boolean");

		VariableComponent ComponentResultat = new VariableComponent(q, null, m, new HashSet<Expression>());
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

		Expression f1 = IfThenElse.make(a, IfThenElse.make(q, parse("3"), parse("9")),
				IfThenElse.make(q, parse("5"), parse("7")));
		Expression f2 = IfThenElse.make(e, IfThenElse.make(q, parse("2"), parse("8")),
				IfThenElse.make(q, parse("1"), parse("0")));
		Expression f3 = IfThenElse.make(g, IfThenElse.make(q, parse("34"), parse("9")),
				IfThenElse.make(q, parse("5"), parse("7")));
		Expression f4 = IfThenElse.make(e, IfThenElse.make(f, parse("5"), parse("2")),
				IfThenElse.make(f, parse("1"), parse("1")));
		Expression f5 = IfThenElse.make(g, IfThenElse.make(f, parse("6"), parse("0")),
				IfThenElse.make(f, parse("0"), parse("3")));
		Expression f6 = IfThenElse.make(b, IfThenElse.make(d, parse("9"), parse("3")),
				IfThenElse.make(d, parse("2"), parse("0")));
		Expression f7 = IfThenElse.make(c, IfThenElse.make(d, parse("1"), parse("7")),
				IfThenElse.make(d, parse("6"), parse("6")));
		Expression f8 = IfThenElse.make(b, IfThenElse.make(a, parse("2"), parse("1")),
				IfThenElse.make(a, parse("4"), parse("10")));
		Expression f9 = IfThenElse.make(c, IfThenElse.make(a, parse("6"), parse("4")),
				IfThenElse.make(a, parse("8"), parse("4")));
		Expression f10 = IfThenElse.make(c, parse("1"), parse("0"));
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
		Factor.add(f10);
		Factor.add(res);

		Model m = new Model(Factor);

		VariableComponent ComponentResultat = new VariableComponent(q, res, m, new HashSet<Expression>());
		return ComponentResultat;

	}

	public static VariableComponent RealCancerModel(Set<Expression> condition) {

		// https://qph.ec.quoracdn.net/main-qimg-17c53810d49f6e917af93e576a9ec8da

		Expression visitToAsia = DefaultSymbol.createSymbol("VisitToAsia");
		Expression tuberculosis = DefaultSymbol.createSymbol("Tuberculosis");
		Expression lungCancer = DefaultSymbol.createSymbol("LungCancer");
		Expression lungCancerOrTuberculosis = DefaultSymbol.createSymbol("LungCancerOrTuberculosis");
		Expression positiveXRay = DefaultSymbol.createSymbol("PositiveXRay");
		Expression dispnea = DefaultSymbol.createSymbol("Dispnea");
		Expression bronchitis = DefaultSymbol.createSymbol("Bronchitis");
		Expression smoker = DefaultSymbol.createSymbol("Smoker");

		Expression probabilityVisitToAsia = IfThenElse.make(visitToAsia, parse("0.01"), parse("0.99"));
		Expression probabilitySmoker = IfThenElse.make(smoker, parse("0.5"), parse("0.5"));

		Expression f1 = IfThenElse.make(visitToAsia, IfThenElse.make(tuberculosis, parse("0.05"), parse("0.95")),
				IfThenElse.make(tuberculosis, parse("0.01"), parse("0.99")));
		Expression f2 = IfThenElse.make(smoker, IfThenElse.make(lungCancer, parse("0.1"), parse("0.9")),
				IfThenElse.make(lungCancer, parse("0.01"), parse("0.99")));
		Expression f3 = IfThenElse.make(smoker, IfThenElse.make(bronchitis, parse("0.6"), parse("0.4")),
				IfThenElse.make(bronchitis, parse("0.3"), parse("0.7")));
		Expression f4 = IfThenElse.make(lungCancerOrTuberculosis,
				IfThenElse.make(positiveXRay, parse("0.98"), parse("0.02")),
				IfThenElse.make(positiveXRay, parse("0.05"), parse("0.95")));

		Expression f5 = IfThenElse.make(lungCancerOrTuberculosis,
				IfThenElse.make(lungCancer, parse("1"), IfThenElse.make(tuberculosis, parse("1"), parse("0"))),
				IfThenElse.make(lungCancer, parse("0"), IfThenElse.make(tuberculosis, parse("0"), parse("1"))));

		Expression f6 = IfThenElse.make(lungCancerOrTuberculosis,
				IfThenElse.make(bronchitis, IfThenElse.make(dispnea, parse("0.9"), parse("0.1")),
						IfThenElse.make(dispnea, parse("0.7"), parse("0.3"))),
				IfThenElse.make(bronchitis, IfThenElse.make(dispnea, parse("0.8"), parse("0.2")),
						IfThenElse.make(dispnea, parse("0.1"), parse("0.9"))));

		Set<Expression> Factor = new HashSet<Expression>();
		Factor.add(f1);
		Factor.add(f2);
		Factor.add(f3);
		Factor.add(f4);
		Factor.add(f5);
		Factor.add(f6);
		Factor.add(probabilityVisitToAsia);
		Factor.add(probabilitySmoker);

		Model m = new Model(Factor);
		m.addConditions(condition);

		

		VariableComponent ComponentResultat = new VariableComponent(lungCancer, DefaultSymbol.createSymbol(""), m,
				new HashSet<Expression>());
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

		VariableComponent ComponentResult = TreeModelWithInteger();
		Set<Expression> condition = new HashSet<Expression>();
		condition.add(parse("A = 1"));
		ComponentResult.model.addConditions(condition);
		runningTest(ComponentResult);
	}

	private static void runningTest(VariableComponent ComponentResult) {

		long startTime, endTime, totalTime;
		ComponentResult.model.context = ComponentResult.model.context.extendWithSymbolsAndTypes("Q", "Boolean");
		while(!ComponentResult.entirelyDiscover) {
			ComponentResult.update(new HashSet<Expression>());
		}
		
		
		ComponentResult.print(0);
		
		startTime = System.currentTimeMillis();
		Expression naiveResult = ComponentResult.naiveCalcul();
		endTime   = System.currentTimeMillis();
		totalTime = endTime - startTime;
		
		println("\n\nNaive Result : " + naiveResult);
		println("totalTime: " + totalTime);
		
		startTime = System.currentTimeMillis();
		Expression unnormalizedMessage = ComponentResult.calculate();
		String string = "(" + unnormalizedMessage + ")/sum({{ (on "  + ComponentResult.variable + " in Boolean) " + unnormalizedMessage + " }})";
		Expression normalizedMessage = ComponentResult.model.theory.evaluate(parse(string), ComponentResult.model.context);
		endTime   = System.currentTimeMillis();
		totalTime = endTime - startTime;
		
		System.out.println("\n\nOur computation : " + normalizedMessage);
		println("totalTime: " + totalTime);
	}

}
