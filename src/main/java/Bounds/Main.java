package Bounds;


import com.sri.ai.expresso.api.Expression;
import static com.sri.ai.expresso.helper.Expressions.parse;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

//import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.util.Util.println;
//import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;


public class Main {
	public static void main(String[] args) {
		
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		
		Context context = new TrueContext(theory);
		context = context.extendWithSymbols("A","Boolean");
		context = context.extendWithSymbols("B","Boolean");
		context = context.extendWithSymbols("X","Boolean");
		context = context.extendWithSymbols("Y","Boolean");
		
		//Set of numbers
		Expression one   = DefaultSymbol.createSymbol(1);
		Expression two   = DefaultSymbol.createSymbol(2);
		Expression three = DefaultSymbol.createSymbol(3);
		Expression setOFNumbers = ExtensionalSet.makeUniSet(one, two, three);

		//Set of functions
		Expression phi1 = parse("if X = true then 1 else if Y = true then 2 else 3");
		Expression phi2 = parse("if X = true then if Y = true then 4 else 5 else 6");
		Expression phi3 = parse("if A = true then 7 else if B = true then 8 else 9");
		Expression phi4 = parse("if A = true then 10 else if B = true then 11 else 12");
		Expression setOfFactors = ExtensionalSet.makeUniSet(phi1, phi2, phi3, phi4);
		println("setOfFactors: " + setOfFactors);
		
		//Testing @applyFunctionToBound
		Expression phi = DefaultSymbol.createSymbol("phi");
		Expression f = parse("13*phi");
	//	Expression f2 = parse("sum(A true or false, phi)");
		println("f: " + f);
		
		println("f(setNum (1,2,3)): " + Bounds.applyFunctionToBound(f, phi, setOFNumbers, theory, context));
		println("f(setfac): " + Bounds.applyFunctionToBound(f, phi, setOfFactors, theory, context)); // NOT the expected answer!
		
		//Testing @boundProduct
		println("setFac X setFac X setNum" + Bounds.boundProduct(theory, context, setOfFactors,setOfFactors,setOFNumbers));
		
		//Testing @normalize
		println("bound : " + setOfFactors);
		println("normal(setFac) : " + Bounds.normalize(setOfFactors, theory, context));
		println("normal(setNum) : " + Bounds.normalize(setOFNumbers, theory, context));
		
		Expression phinormalized = parse("(if X = true then 1 else if Y = true then 2 else 3) / sum({{ ( on A in Boolean, B in Boolean ) if A = true then 1 else if B = true then 2 else 3 }})");
		Expression eval = theory.evaluate(phinormalized, context);
		println(eval);
	}
}
