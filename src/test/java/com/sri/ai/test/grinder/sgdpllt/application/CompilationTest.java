package com.sri.ai.test.grinder.sgdpllt.application;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.sgdpllt.application.Compilation.compile;
import static com.sri.ai.util.Util.list;
import static org.junit.Assert.assertEquals;

import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.util.Util;

public class CompilationTest {

	@Test
	public void test() {
		
		Expression input; 
		Expression expected;
		Theory theory = 
				new CompoundTheory(
						new EqualityTheory(true, true),
						new PropositionalTheory());
		Map<String, String> mapFromCategoricalTypeNameToSizeString;
		Map<String, String> mapFromVariableNameToTypeName;
		Map<String, String> mapFromUniquelyNamedConstantToTypeName;

		input = Expressions.parse("if X = a then if X = b then 1 else 2 else 3"); 
		expected = parse("if X = a then 2 else 3");
		mapFromCategoricalTypeNameToSizeString   = Util.map("Everything", "2");
		mapFromVariableNameToTypeName = Util.map("X", "Everything");
		mapFromUniquelyNamedConstantToTypeName = Util.map("a", "Everything", "b", "Everything");
		runTest(input, expected, theory, mapFromCategoricalTypeNameToSizeString, mapFromVariableNameToTypeName, mapFromUniquelyNamedConstantToTypeName);
		

		input = Expressions.parse(""
						+ "if X = a and Y = a then 0.1 else "
						+ "if X = a and Y = b then 0.1 else "
						+ "if X = b and Y = a then 0.2 else "
						// + "if X = b and Y = b then 0.2" // no need to test because it is the last case
						+ "0.2"); 
		expected = parse("if X = a then 0.1 else 0.2");
		mapFromCategoricalTypeNameToSizeString   = Util.map("Everything", "2");
		mapFromVariableNameToTypeName = 
				Util.map("X", "Everything", "Y", "Everything");
		mapFromUniquelyNamedConstantToTypeName = 
				Util.map("a", "Everything", "b", "Everything");
		runTest(input, expected, theory, mapFromCategoricalTypeNameToSizeString, mapFromVariableNameToTypeName, mapFromUniquelyNamedConstantToTypeName);
		

		input = Expressions.parse(""
						+ "if X = a and Y = a and Z = a then 0.1 else "
						+ "if X = a and Y = a and Z = b then 0.1 else "
						+ "if X = a and Y = a and Z = c then 0.1 else "
						+ "if X = a and Y = b and Z = a then 0.1 else "
						+ "if X = a and Y = b and Z = b then 0.1 else "
						+ "if X = a and Y = b and Z = c then 0.1 else "
						+ "if X = a and Y = c and Z = a then 0.1 else "
						+ "if X = a and Y = c and Z = b then 0.1 else "
						+ "if X = a and Y = c and Z = c then 0.1 else "
						+ "if X = b and Y = a and Z = a then 0.2 else "
						+ "if X = b and Y = a and Z = b then 0.2 else "
						+ "if X = b and Y = a and Z = c then 0.2 else "
						+ "if X = b and Y = b and Z = a then 0.2 else "
						+ "if X = b and Y = b and Z = b then 0.2 else "
						+ "if X = b and Y = b and Z = c then 0.2 else "
						+ "if X = b and Y = c and Z = a then 0.2 else "
						+ "if X = b and Y = c and Z = b then 0.2 else "
						+ "if X = b and Y = c and Z = c then 0.2 else "
						+ "if X = c and Y = a and Z = a then 0.3 else "
						+ "if X = c and Y = a and Z = b then 0.3 else "
						+ "if X = c and Y = a and Z = c then 0.3 else "
						+ "if X = c and Y = b and Z = a then 0.3 else "
						+ "if X = c and Y = b and Z = b then 0.3 else "
						+ "if X = c and Y = b and Z = c then 0.3 else "
						+ "if X = c and Y = c and Z = a then 0.3 else "
						+ "if X = c and Y = c and Z = b then 0.3 else "
						+  /* X = c and Y = c and Z = c ; no need as it is implied by domain definition */  "0.3"); 
		expected = parse("if X = a then 0.1 else if X = b then 0.2 else 0.3");
		mapFromCategoricalTypeNameToSizeString   = Util.map("Everything", "3");
		mapFromVariableNameToTypeName = 
				Util.map("X", "Everything", "Y", "Everything", "Z", "Everything");
		mapFromUniquelyNamedConstantToTypeName = 
				Util.map("a", "Everything", "b", "Everything", "c", "Everything");
		runTest(input, expected, theory, mapFromCategoricalTypeNameToSizeString, mapFromVariableNameToTypeName, mapFromUniquelyNamedConstantToTypeName);
		
		// Same thing, but with non-capitalized variables that should still be recognized as variables
		input = Expressions.parse(""
						+ "if x = a and y = a and z = a then 0.1 else "
						+ "if x = a and y = a and z = b then 0.1 else "
						+ "if x = a and y = a and z = c then 0.1 else "
						+ "if x = a and y = b and z = a then 0.1 else "
						+ "if x = a and y = b and z = b then 0.1 else "
						+ "if x = a and y = b and z = c then 0.1 else "
						+ "if x = a and y = c and z = a then 0.1 else "
						+ "if x = a and y = c and z = b then 0.1 else "
						+ "if x = a and y = c and z = c then 0.1 else "
						+ "if x = b and y = a and z = a then 0.2 else "
						+ "if x = b and y = a and z = b then 0.2 else "
						+ "if x = b and y = a and z = c then 0.2 else "
						+ "if x = b and y = b and z = a then 0.2 else "
						+ "if x = b and y = b and z = b then 0.2 else "
						+ "if x = b and y = b and z = c then 0.2 else "
						+ "if x = b and y = c and z = a then 0.2 else "
						+ "if x = b and y = c and z = b then 0.2 else "
						+ "if x = b and y = c and z = c then 0.2 else "
						+ "if x = c and y = a and z = a then 0.3 else "
						+ "if x = c and y = a and z = b then 0.3 else "
						+ "if x = c and y = a and z = c then 0.3 else "
						+ "if x = c and y = b and z = a then 0.3 else "
						+ "if x = c and y = b and z = b then 0.3 else "
						+ "if x = c and y = b and z = c then 0.3 else "
						+ "if x = c and y = c and z = a then 0.3 else "
						+ "if x = c and y = c and z = b then 0.3 else "
						+  /* x = c and y = c and z = c ; no need as it is implied by domain definition */  "0.3"); 
		expected = parse("if x = a then 0.1 else if x = b then 0.2 else 0.3");
		mapFromCategoricalTypeNameToSizeString   = Util.map("Everything", "3");
		mapFromVariableNameToTypeName = 
				Util.map("x", "Everything", "y", "Everything", "z", "Everything");
		mapFromUniquelyNamedConstantToTypeName = 
				Util.map("a", "Everything", "b", "Everything", "c", "Everything");
		runTest(input, expected, theory, mapFromCategoricalTypeNameToSizeString, mapFromVariableNameToTypeName, mapFromUniquelyNamedConstantToTypeName);

		input = Expressions.parse(""
						+ "if not g0 and (g1 = consg1_0)\r\n" + 
						"then 0.0001\r\n" + 
						"else if not g0 and (g1 = consg1_1)\r\n" + 
						"     then 1\r\n" + 
						"     else if not g0 and (g1 = consg1_2)\r\n" + 
						"          then 0.0001\r\n" + 
						"          else if not g0 and (g1 = consg1_3)\r\n" + 
						"               then 1\r\n" + 
						"               else if g0 and (g1 = consg1_0)\r\n" + 
						"                    then 1\r\n" + 
						"                    else if g0 and (g1 = consg1_1)\r\n" + 
						"                         then 1\r\n" + 
						"                         else if g0 and (g1 = consg1_2)\r\n" + 
						"                              then 1\r\n" + 
						"                              else 1\r\n" + 
				""); 
		expected = parse("if not g0 then if g1 = consg1_0 then 0.0001 else if g1 = consg1_1 then 1 else if g1 = consg1_2 then 0.0001 else 1 else 1");
		mapFromCategoricalTypeNameToSizeString   = Util.map("G1Type", "4", "Boolean", "2");
		mapFromVariableNameToTypeName = 
				Util.map("g0", "Boolean", "g1", "G1Type");
		mapFromUniquelyNamedConstantToTypeName = 
				Util.map("consg1_0", "G1Type", "consg1_1", "G1Type", "consg1_2", "G1Type", "consg1_3", "G1Type");
		runTest(input, expected, theory, mapFromCategoricalTypeNameToSizeString, mapFromVariableNameToTypeName, mapFromUniquelyNamedConstantToTypeName);

		input = Expressions.parse("if not g0 then 1 else 1"); 
		expected = parse("1");
		mapFromCategoricalTypeNameToSizeString   = Util.map("G1Type", "4", "Boolean", "2");
		mapFromVariableNameToTypeName = Util.map("g0", "Boolean", "g1", "G1Type");
		mapFromUniquelyNamedConstantToTypeName = Util.map();
		runTest(input, expected, theory, mapFromCategoricalTypeNameToSizeString, mapFromVariableNameToTypeName, mapFromUniquelyNamedConstantToTypeName);
	}

	private void runTest(Expression input, Expression expected, Theory theory, Map<String, String> mapFromCategoricalTypeNameToSizeString, Map<String, String> mapFromVariableNameToTypeName, Map<String, String> mapFromUniquelyNamedConstantToTypeName) {
		Expression actual = compile(input, theory, mapFromCategoricalTypeNameToSizeString, list(), mapFromVariableNameToTypeName, mapFromUniquelyNamedConstantToTypeName);
		assertEquals(expected, actual);
	}
}
