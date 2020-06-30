package com.sri.ai.test.grinder.interpreter

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.Expressions
import com.sri.ai.expresso.helper.Expressions.parse
import com.sri.ai.grinder.core.TrueContext
import com.sri.ai.grinder.interpreter.CompilationEvaluator
import org.junit.Test

import org.junit.Assert.*

class CompilationEvaluatorTest {

    @Test
    fun evaluate() {
        val tests =
                listOf(
                        "X + Y"
                                to
                                listOf(
                                        assignment(2, 3) to 5,
                                        assignment(-1, 3) to 2),

                        "if X = 1 then Y else Z"
                                to
                                listOf(
                                        assignment(2, 3, 4) to 4,
                                        assignment(1, 5, 0) to 5),

                        "if X != Y + Z then 1 else 0"
                                to
                                listOf(
                                        assignment(2, 3, 4) to 1,
                                        assignment(5, 2, 3) to 0)
                )

        for ((expressionString, expressionTests) in tests) {
            for ((assignment, expected) in expressionTests) {
                val expression = parse(expressionString)
                val interpreter = CompilationEvaluator(variablesInOrder(expression))

                println("Java expression of $expression: ${interpreter.javaExpression(expression)}")
                println("Evaluator definition of $expression:\n${interpreter.evaluatorDefinition(expression)}")

                val actual = interpreter.evaluate(expression, assignment)
                println("Value of $expression under ${interpreter.assignmentMap(assignment)}: $actual")
                assertEquals(expected, actual)
            }
        }
    }

    private fun variablesInOrder(expression: Expression?): List<Expression> =
            Expressions.getVariablesBeingReferenced(expression, TrueContext()).toList()

    private fun assignment(vararg values: Int) = intArrayOf(*values)
}