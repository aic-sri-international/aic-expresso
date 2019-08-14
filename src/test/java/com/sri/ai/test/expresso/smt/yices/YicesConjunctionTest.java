package com.sri.ai.test.expresso.smt.yices;

import static com.sri.ai.util.Util.println;
import static com.sri.ai.expresso.helper.Expressions.parse;
import java.util.Random;
import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.smt.core.yices.YicesConjunction;

public class YicesConjunctionTest {
	
	final String tab = "\t";
	final String stab_big = "        ";
	final String stab = "    ";

	public Random makeRandom() {
		return new Random();
	}
	
	
	
	
	@Test
	public void blankTest() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		
		
		println();
		println();
		println();
	}
	
	
	
	
	@Test
	public void simpleTest() throws Exception {
		println("////////////////////////////////////////////////////////");
		println( new Object() {}.getClass().getEnclosingMethod().getName() );
		println("////////////////////////////////////////////////////////");
		println();
		
		Theory theory = new CommonTheory();
		
		Context context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes("X", "Integer", "Y", "Real");
		
		String[] literalStrings = new String[] {
				"X < 1",
				"Y > X",
				"Y < 0.5",
				"X > -1",
				"Y = 0"
		};
		
		YicesConjunction conjunction = new YicesConjunction();
		
		for (String literalString : literalStrings) {
			println("and(" + literalString + "):");
			Expression literal = parse(literalString);
			conjunction.and(literal, context);
			println(stab + "isSatisfiable: " + conjunction.isSatisfiable());
			println(stab + "getModel: " + conjunction.getModel());
			println();
		}
		
		println("backtracking to undo (Y = 0)...");
		conjunction.backtrack();
		println("and(Y = 0.3):");
		Expression literal = parse("Y = 0.3");
		conjunction.and(literal, context);
		println(stab + "isSatisfiable: " + conjunction.isSatisfiable());
		println(stab + "getModel: " + conjunction.getModel());
		println();
		
		
		println();
		println();
		println();
	}
	
}




		

