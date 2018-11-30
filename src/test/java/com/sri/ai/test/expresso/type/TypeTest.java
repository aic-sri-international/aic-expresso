package com.sri.ai.test.expresso.type;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.FUNCTION_TYPE;
import static com.sri.ai.grinder.library.FunctorConstants.TUPLE_TYPE;

import java.util.Arrays;
import java.util.StringJoiner;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.helper.GrinderUtil;

public class TypeTest {
	
	@Test
	public void testCardinality() {
		// i.e. 2^3
		Assert.assertEquals(parse("8"),
				new FunctionType(GrinderUtil.BOOLEAN_TYPE, new IntegerInterval(0, 2)).cardinality());
		// i.e. 3^(4*2)
		Assert.assertEquals(parse("6561"),
				new FunctionType(new IntegerInterval(1, 3), new IntegerInterval(1, 4), new IntegerInterval(1, 2))
						.cardinality());
		
		// i.e. 1 - the empty tuple
		Assert.assertEquals(parse("1"), 
				new TupleType()
						.cardinality());
		// i.e. 2x3x4
		Assert.assertEquals(parse("24"), 
				new TupleType(GrinderUtil.BOOLEAN_TYPE, new IntegerInterval(1, 3), new IntegerInterval(1, 4))
						.cardinality());	
	}
	
	@Test
	public void testIterator() {
		FunctionType fType;
		 // i.e. 2
		fType = new FunctionType(new IntegerInterval(1, 2));
		checkTypeIteration(fType, 
				"lambda : 1",
				"lambda : 2"
				);
		
		// i.e. 2^2
		fType = new FunctionType(new IntegerInterval(1, 2), new Categorical("Car", 2));
		checkTypeIteration(fType, 
				"lambda A1 in Car : if A1 = car1 then 1 else 1",
				"lambda A1 in Car : if A1 = car1 then 2 else 1",
				"lambda A1 in Car : if A1 = car1 then 1 else 2",
				"lambda A1 in Car : if A1 = car1 then 2 else 2"
				);
		
		// i.e. 2^(2*2)
		fType = new FunctionType(new IntegerInterval(1, 2), new Categorical("Car", 2), new Categorical("Bike", 2));
		checkTypeIteration(fType, 
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 1 else if (A1 = car2) and (A2 = bike1) then 1 else if (A1 = car1) and (A2 = bike2) then 1 else 1",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 2 else if (A1 = car2) and (A2 = bike1) then 1 else if (A1 = car1) and (A2 = bike2) then 1 else 1",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 1 else if (A1 = car2) and (A2 = bike1) then 2 else if (A1 = car1) and (A2 = bike2) then 1 else 1",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 2 else if (A1 = car2) and (A2 = bike1) then 2 else if (A1 = car1) and (A2 = bike2) then 1 else 1",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 1 else if (A1 = car2) and (A2 = bike1) then 1 else if (A1 = car1) and (A2 = bike2) then 2 else 1",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 2 else if (A1 = car2) and (A2 = bike1) then 1 else if (A1 = car1) and (A2 = bike2) then 2 else 1",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 1 else if (A1 = car2) and (A2 = bike1) then 2 else if (A1 = car1) and (A2 = bike2) then 2 else 1",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 2 else if (A1 = car2) and (A2 = bike1) then 2 else if (A1 = car1) and (A2 = bike2) then 2 else 1",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 1 else if (A1 = car2) and (A2 = bike1) then 1 else if (A1 = car1) and (A2 = bike2) then 1 else 2",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 2 else if (A1 = car2) and (A2 = bike1) then 1 else if (A1 = car1) and (A2 = bike2) then 1 else 2",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 1 else if (A1 = car2) and (A2 = bike1) then 2 else if (A1 = car1) and (A2 = bike2) then 1 else 2",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 2 else if (A1 = car2) and (A2 = bike1) then 2 else if (A1 = car1) and (A2 = bike2) then 1 else 2",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 1 else if (A1 = car2) and (A2 = bike1) then 1 else if (A1 = car1) and (A2 = bike2) then 2 else 2",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 2 else if (A1 = car2) and (A2 = bike1) then 1 else if (A1 = car1) and (A2 = bike2) then 2 else 2",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 1 else if (A1 = car2) and (A2 = bike1) then 2 else if (A1 = car1) and (A2 = bike2) then 2 else 2",
				"lambda A1 in Car, A2 in Bike : if (A1 = car1) and (A2 = bike1) then 2 else if (A1 = car2) and (A2 = bike1) then 2 else if (A1 = car1) and (A2 = bike2) then 2 else 2"
				);
		
		// i.e. 0
		TupleType tType = new TupleType(); 
		checkTypeIteration(tType,
				"()"
				);
		
		// i.e. 3
		tType = new TupleType(new IntegerInterval(1, 3)); 
		checkTypeIteration(tType,
				"tuple(1)",
				"tuple(2)",
				"tuple(3)"
				);
		
		// i.e. 2x3
		tType = new TupleType(GrinderUtil.BOOLEAN_TYPE, new IntegerInterval(1,3));
		checkTypeIteration(tType,
				"(true, 1)",
				"(false, 1)",
				"(true, 2)",
				"(false, 2)",
				"(true, 3)",
				"(false, 3)"
				);		
	}
	
	public void checkTypeIteration(Type type, String... expectedLines) {
		StringJoiner expected = new StringJoiner("\n", type.getName()+"\n", "");
		for (String expectedLine : expectedLines) {
			expected.add(expectedLine);
		}
		StringJoiner actual = new StringJoiner("\n", type.getName()+"\n", "");
		type.iterator().forEachRemaining(actualLine -> actual.add(actualLine.toString()));
		
		Assert.assertEquals(expected.toString(), actual.toString());
	}
	
	@Test
	public void testIsDiscrete() {
		//
		// Categorical type tests
		Assert.assertTrue(new Categorical("UnknownCardCatType", -1).isDiscrete());
		Assert.assertTrue(new Categorical("InfiniteCardCatType", -2).isDiscrete());
		Assert.assertTrue(new Categorical("CardCatType", 0).isDiscrete());
		Assert.assertTrue(new Categorical("CardCatType", 100).isDiscrete());
		
		//
		// Integer type tests
		Assert.assertTrue(new IntegerExpressoType().isDiscrete());
		
		//
		// Real type tests
		Assert.assertFalse(new RealExpressoType().isDiscrete());
		
		//
		// Integer Interval type tests
		Assert.assertTrue(new IntegerInterval("Integer").isDiscrete());
		Assert.assertTrue(new IntegerInterval("integer_Interval(-infinity, inifinity)").isDiscrete());
		Assert.assertTrue(new IntegerInterval("integer_Interval(-10, inifinity)").isDiscrete());
		Assert.assertTrue(new IntegerInterval("integer_Interval(-infinity, 10)").isDiscrete());
		Assert.assertTrue(new IntegerInterval("integer_Interval(-10, 10)").isDiscrete());
		
		//
		// Real Interval type tests
		Assert.assertFalse(new RealInterval("Real").isDiscrete());
		Assert.assertFalse(new RealInterval("[-infinity;infinity]").isDiscrete());
		Assert.assertFalse(new RealInterval("[-10;infinity]").isDiscrete());
		Assert.assertFalse(new RealInterval("[-infinity;10]").isDiscrete());
		Assert.assertFalse(new RealInterval("[0;1]").isDiscrete());
		
		//
		// Function Type
		Assert.assertTrue(new FunctionType(new IntegerExpressoType()).isDiscrete());
		Assert.assertFalse(new FunctionType(new RealExpressoType()).isDiscrete());
		Assert.assertTrue(new FunctionType(new Categorical("Cat", 10)).isDiscrete());
		Assert.assertTrue(new FunctionType(new IntegerInterval("Integer")).isDiscrete());
		Assert.assertFalse(new FunctionType(new RealInterval("Real")).isDiscrete());
		Assert.assertTrue(new FunctionType(new TupleType(new IntegerExpressoType())).isDiscrete());
		Assert.assertFalse(new FunctionType(new TupleType(new RealInterval("Real"))).isDiscrete());
		//
		Assert.assertTrue(new FunctionType(new IntegerExpressoType(), new Categorical("Cat", 10)).isDiscrete());
		Assert.assertFalse(new FunctionType(new IntegerExpressoType(), new RealExpressoType()).isDiscrete());
		Assert.assertFalse(new FunctionType(new RealExpressoType(), new IntegerExpressoType()).isDiscrete());
		Assert.assertTrue(new FunctionType(new Categorical("Cat", 10), new IntegerExpressoType()).isDiscrete());
		Assert.assertFalse(new FunctionType(new Categorical("Cat", 10), new RealExpressoType()).isDiscrete());
		Assert.assertTrue(new FunctionType(new IntegerInterval("Integer"), new IntegerExpressoType()).isDiscrete());
		Assert.assertFalse(new FunctionType(new IntegerInterval("Integer"), new RealExpressoType()).isDiscrete());
		Assert.assertFalse(new FunctionType(new RealInterval("Real")).isDiscrete());
		Assert.assertFalse(new FunctionType(new RealInterval("Real"), new IntegerExpressoType()).isDiscrete());
		
		//
		// Tuple Type
		Assert.assertTrue(new TupleType().isDiscrete());
		Assert.assertTrue(new TupleType(new IntegerExpressoType()).isDiscrete());
		Assert.assertFalse(new TupleType(new RealExpressoType()).isDiscrete());
		Assert.assertTrue(new TupleType(new Categorical("Cat", 10)).isDiscrete());
		Assert.assertTrue(new TupleType(new IntegerInterval("Integer")).isDiscrete());
		Assert.assertFalse(new TupleType(new RealInterval("Real")).isDiscrete());
		//
		Assert.assertTrue(new TupleType(new IntegerExpressoType(), new Categorical("Cat", 10)).isDiscrete());
		Assert.assertFalse(new TupleType(new IntegerExpressoType(), new RealExpressoType()).isDiscrete());
		Assert.assertFalse(new TupleType(new RealExpressoType(), new IntegerExpressoType()).isDiscrete());
		Assert.assertTrue(new TupleType(new Categorical("Cat", 10), new IntegerExpressoType()).isDiscrete());
		Assert.assertFalse(new TupleType(new Categorical("Cat", 10), new RealExpressoType()).isDiscrete());
		Assert.assertTrue(new TupleType(new IntegerInterval("Integer"), new IntegerExpressoType()).isDiscrete());
		Assert.assertFalse(new TupleType(new IntegerInterval("Integer"), new RealExpressoType()).isDiscrete());
		Assert.assertFalse(new TupleType(new RealInterval("Real")).isDiscrete());
		Assert.assertFalse(new TupleType(new RealInterval("Real"), new IntegerExpressoType()).isDiscrete());
	}
	
	@Test
	public void testIsFinite() {
		//
		// Categorical type tests
		Assert.assertFalse(new Categorical("UnknownCardCatType", -1).isFinite());
		Assert.assertFalse(new Categorical("InfiniteCardCatType", -2).isFinite());
		Assert.assertTrue(new Categorical("CardCatType", 0).isFinite());
		Assert.assertTrue(new Categorical("CardCatType", 100).isFinite());
		
		//
		// Integer type tests
		Assert.assertFalse(new IntegerExpressoType().isFinite());
		
		//
		// Real type tests
		Assert.assertFalse(new RealExpressoType().isFinite());
		
		//
		// Integer Interval type tests
		Assert.assertFalse(new IntegerInterval("Integer").isFinite());
		Assert.assertFalse(new IntegerInterval("integer_Interval(-infinity, inifinity)").isFinite());
		Assert.assertFalse(new IntegerInterval("integer_Interval(-10, inifinity)").isFinite());
		Assert.assertFalse(new IntegerInterval("integer_Interval(-infinity, 10)").isFinite());
		Assert.assertTrue(new IntegerInterval("integer_Interval(-10, 10)").isFinite());
		
		//
		// Real Interval type tests
		Assert.assertFalse(new RealInterval("Real").isFinite());
		Assert.assertFalse(new RealInterval("[-infinity;infinity]").isFinite());
		Assert.assertFalse(new RealInterval("[-10;infinity]").isFinite());
		Assert.assertFalse(new RealInterval("[-infinity;10]").isFinite());
		Assert.assertFalse(new RealInterval("[0;1]").isFinite());
		
		//
		// Function Type
		Assert.assertFalse(new FunctionType(new IntegerExpressoType()).isFinite());
		Assert.assertFalse(new FunctionType(new RealExpressoType()).isFinite());
		Assert.assertTrue(new FunctionType(new Categorical("Cat", 10)).isFinite());
		Assert.assertTrue(new FunctionType(new IntegerInterval(1, 3)).isFinite());
		Assert.assertFalse(new FunctionType(new IntegerInterval("Integer")).isFinite());
		Assert.assertFalse(new FunctionType(new RealInterval("Real")).isFinite());
		Assert.assertTrue(new FunctionType(new TupleType(new IntegerInterval(1, 3))).isFinite());
		Assert.assertFalse(new FunctionType(new TupleType(new RealInterval("Real"))).isFinite());
		//
		Assert.assertFalse(new FunctionType(new IntegerExpressoType(), new Categorical("Cat", 10)).isFinite());
		Assert.assertFalse(new FunctionType(new IntegerExpressoType(), new RealExpressoType()).isFinite());
		Assert.assertFalse(new FunctionType(new RealExpressoType(), new IntegerExpressoType()).isFinite());
		Assert.assertFalse(new FunctionType(new Categorical("Cat", 10), new IntegerExpressoType()).isFinite());
		Assert.assertFalse(new FunctionType(new Categorical("Cat", 10), new RealExpressoType()).isFinite());
		Assert.assertFalse(new FunctionType(new IntegerInterval("Integer"), new IntegerExpressoType()).isFinite());
		Assert.assertTrue(new FunctionType(new IntegerInterval(1, 2), new IntegerInterval(3, 5)).isFinite());
		Assert.assertFalse(new FunctionType(new IntegerInterval("Integer"), new RealExpressoType()).isFinite());
		Assert.assertFalse(new FunctionType(new RealInterval("Real")).isFinite());
		Assert.assertFalse(new FunctionType(new RealInterval("Real"), new IntegerExpressoType()).isFinite());
		
		//
		// Tuple Type
		Assert.assertFalse(new TupleType(new IntegerExpressoType()).isFinite());
		Assert.assertFalse(new TupleType(new RealExpressoType()).isFinite());
		Assert.assertTrue(new TupleType(new Categorical("Cat", 10)).isFinite());
		Assert.assertTrue(new TupleType(new IntegerInterval(1, 3)).isFinite());
		Assert.assertFalse(new TupleType(new IntegerInterval("Integer")).isFinite());
		Assert.assertFalse(new TupleType(new RealInterval("Real")).isFinite());
		//
		Assert.assertFalse(new TupleType(new IntegerExpressoType(), new Categorical("Cat", 10)).isFinite());
		Assert.assertFalse(new TupleType(new IntegerExpressoType(), new RealExpressoType()).isFinite());
		Assert.assertFalse(new TupleType(new RealExpressoType(), new IntegerExpressoType()).isFinite());
		Assert.assertFalse(new TupleType(new Categorical("Cat", 10), new IntegerExpressoType()).isFinite());
		Assert.assertFalse(new TupleType(new Categorical("Cat", 10), new RealExpressoType()).isFinite());
		Assert.assertFalse(new TupleType(new IntegerInterval("Integer"), new IntegerExpressoType()).isFinite());
		Assert.assertTrue(new TupleType(new IntegerInterval(1, 2), new IntegerInterval(3, 5)).isFinite());
		Assert.assertFalse(new TupleType(new IntegerInterval("Integer"), new RealExpressoType()).isFinite());
		Assert.assertFalse(new TupleType(new RealInterval("Real")).isFinite());
		Assert.assertFalse(new TupleType(new RealInterval("Real"), new IntegerExpressoType()).isFinite());
	}
	
	@Test
	public void testThreeFormsOfFunctionType() {
		Assert.assertEquals(apply(FUNCTION_TYPE, parse("Boolean")), parse("'->'(Boolean)"));
		Assert.assertEquals(apply(FUNCTION_TYPE, parse("Boolean"), parse("Boolean")), parse("'->'(Boolean, Boolean)"));
		Assert.assertEquals(apply(FUNCTION_TYPE, apply(TUPLE_TYPE, parse("Boolean")), parse("Boolean")), parse("'->'(x(Boolean), Boolean)"));
		Assert.assertEquals(apply(FUNCTION_TYPE, apply(TUPLE_TYPE, parse("Boolean"), parse("Boolean")), parse("Boolean")), parse("'->'(x(Boolean, Boolean), Boolean)"));
		
		Assert.assertEquals(0, FunctionType.getArgumentList(parse("'->'(Boolean)")).size());
		Assert.assertEquals(1, FunctionType.getArgumentList(parse("'->'(Boolean, Boolean)")).size());
		Assert.assertEquals(1, FunctionType.getArgumentList(parse("'->'(x(Boolean), Boolean)")).size());
		Assert.assertEquals(2, FunctionType.getArgumentList(parse("'->'(x(Boolean, Boolean), Boolean)")).size());
		
		Assert.assertEquals(Arrays.asList(parse("Boolean")), FunctionType.getArgumentList(parse("'->'(Boolean, Boolean)")));
		Assert.assertEquals(Arrays.asList(parse("Boolean")), FunctionType.getArgumentList(parse("'->'(x(Boolean), Boolean)")));
		Assert.assertEquals(Arrays.asList(parse("Boolean"), parse("Boolean")), FunctionType.getArgumentList(parse("'->'(x(Boolean, Boolean), Boolean)")));
	}

}
