package com.sri.ai.grinder.sgdpllt.anytime;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IF_THEN_ELSE;

import java.util.HashSet;
import java.util.Set;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.AND;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.NOT;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.OR;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PLUS;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PRODUCT;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.set;

import java.util.LinkedHashSet;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.helper.UniquelyNamedConstantAreAllSymbolsNotIn;
import com.sri.ai.grinder.helper.UniquelyNamedConstantIncludingBooleansAndNumbersPredicate;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
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
		

		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		Context context = new TrueContext(theory);			
		context = context.add(BOOLEAN_TYPE);		
		VariableComponent ComponentResultat = DiamondModel();
		int nbIter = 0;

		while(!ComponentResultat.entirelyDiscover) {
			ComponentResultat.update(new HashSet<Expression>());
			nbIter ++;
		}
		
		System.out.println("Iteration necessary : " + nbIter);

		ComponentResultat.print(0);
		System.out.println(" ");
		System.out.println(" ");
		System.out.println(" ");
		System.out.println(ComponentResultat.calculate());
	}

}
