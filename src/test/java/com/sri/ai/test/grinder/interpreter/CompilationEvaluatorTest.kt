package com.sri.ai.test.grinder.interpreter

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.Expressions
import com.sri.ai.expresso.helper.Expressions.parse
import com.sri.ai.grinder.core.TrueContext
import com.sri.ai.grinder.interpreter.CompilationEvaluator
import com.sri.ai.grinder.interpreter.CompilationIncrementalEvaluator
import org.junit.Test

import org.junit.Assert.*
import java.util.LinkedHashSet

class CompilationEvaluatorTest {

    @Test
    fun evaluate() {
        val evaluatorMakers = listOf<(Expression) -> CompilationEvaluator>(
                { expression -> CompilationEvaluator.makeWithVariablesFrom(expression) }
                ,
                { expression -> CompilationIncrementalEvaluator.makeWithVariablesFrom(expression) }
        )

        val tests: List<Pair<String, List<Pair<IntArray, Number>>>> =
                listOf(
                        "if X != Y + Z then X + 1.0 else Y + 2.0"
                                to
                                listOf(
                                        assignment(2, 3, 4) to 3.0,
                                        assignment(5, 2, 3) to 4.0),

                        "if X = 1 then 0.9 else 0.1"
                                to
                                listOf(
                                        assignment(2) to 0.1,
                                        assignment(1) to 0.9),

                        "if X = 1 then Y else Z"
                                to
                                listOf(
                                        assignment(2, 3, 4) to 4,
                                        assignment(1, 5, 0) to 5),

                        "X + Y"
                                to
                                listOf(
                                        assignment(2, 3) to 5,
                                        assignment(-1, 3) to 2),

                        "if X != Y + Z then 1 else 0"
                                to
                                listOf(
                                        assignment(2, 3, 4) to 1.0,
                                        assignment(5, 2, 3) to 0.0)
                )

        for ((expressionString, expressionTests) in tests) {
            for ((assignment, expected) in expressionTests) {
                val expression = parse(expressionString)
                for (evaluatorMaker in evaluatorMakers) {
                    println(expression)
                    println(CompilationEvaluator.variablesInOrder(expression))
                    val evaluator = evaluatorMaker(expression)

                    println("Evaluator definition of $expression:\n${evaluator.evaluatorClassDefinition(expression)}")

                    val actual = evaluator.evaluate(expression, assignment)
                    println("Value of $expression under ${evaluator.assignmentMap(assignment)}: $actual")
                    assertEquals(expected.toDouble(), (actual as Number).toDouble(), 1e-4)
                }
            }
        }
    }

    private fun assignment(vararg values: Int) = intArrayOf(*values)
}