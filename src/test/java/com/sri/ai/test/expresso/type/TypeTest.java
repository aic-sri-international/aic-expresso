package com.sri.ai.test.expresso.type;

import org.junit.Assert;
import org.junit.Test;

import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.StringJoiner;

import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
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
	}
	
	@Test
	public void testIterator() {
		FunctionType fType;
		 // i.e. 2
		fType = new FunctionType(new IntegerInterval(1, 2));
		checkFunctionIteration(fType, 
				"lambda : 1",
				"lambda : 2"
				);
		
		// i.e. 2^2
		fType = new FunctionType(new IntegerInterval(1, 2), new Categorical("Car", 2));
		checkFunctionIteration(fType, 
				"lambda A1 in Car : if A1 = car1 then 1 else 1",
				"lambda A1 in Car : if A1 = car1 then 2 else 1",
				"lambda A1 in Car : if A1 = car1 then 1 else 2",
				"lambda A1 in Car : if A1 = car1 then 2 else 2"
				);
		
		// i.e. (2*2)^2
		fType = new FunctionType(new IntegerInterval(1, 2), new Categorical("Car", 2), new Categorical("Bike", 2));
		checkFunctionIteration(fType, 
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
	}
	
	public void checkFunctionIteration(FunctionType type, String... expectedLines) {
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
	}

}
