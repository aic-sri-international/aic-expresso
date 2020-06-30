package com.sri.ai.grinder.interpreter

import com.sri.ai.expresso.api.Expression
import com.sri.ai.grinder.library.FunctorConstants
import com.sri.ai.util.Util.join
import com.sri.ai.util.compilation.Compiler

class CompilationEvaluator(val variables: List<Expression>) {

    private val variableIndices = variables.mapIndexed { i, v -> v to i }.toMap()

    interface Evaluator {
        fun apply(assignment: IntArray): Int
    }

    fun evaluate(expression: Expression, assignment: IntArray) =
        compile(expression).getDeclaredConstructor().newInstance().apply(assignment)

    private fun compile(expression: Expression): Class<Evaluator> {
        val evaluatorDefinition = evaluatorDefinition(expression)
        val compilation = Compiler.compile<Evaluator>("CompiledEvaluator", evaluatorDefinition)
        if (compilation.succeeded) {
            return compilation.compiledClass
        }
        else {
            throw Error(join("\n", compilation.diagnostics.diagnostics))
        }
    }

    fun evaluatorDefinition(expression: Expression) =
        """
        public class CompiledEvaluator implements com.sri.ai.grinder.interpreter.CompilationEvaluator.Evaluator {
            @Override
            public int apply(int[] assignment) {
                return ${javaExpression(expression)};
            }
        }
        """.trimIndent()

    fun javaExpression(expression: Expression): String =
            "(" +
                    when {
                        expression.syntacticFormType == "Symbol" && variableIndices.containsKey(expression)
                        -> "assignment[${variableIndices[expression]}]"

                        expression.syntacticFormType == "Symbol"
                        -> expression.toString()

                        expression.hasFunctor("+")
                        -> join("+", expression.arguments.map(::javaExpression))

                        expression.hasFunctor(">")
                        -> "${javaExpression(expression.get(0))} > ${javaExpression(expression.get(1))}"

                        expression.hasFunctor("<")
                        -> "${javaExpression(expression.get(0))} < ${javaExpression(expression.get(1))}"

                        expression.hasFunctor(">=")
                        -> "${javaExpression(expression.get(0))} >= ${javaExpression(expression.get(1))}"

                        expression.hasFunctor("<=")
                        -> "${javaExpression(expression.get(0))} <= ${javaExpression(expression.get(1))}"

                        expression.hasFunctor("=")
                        -> "${javaExpression(expression.get(0))} == ${javaExpression(expression.get(1))}"

                        expression.hasFunctor("!=")
                        -> "${javaExpression(expression.get(0))} != ${javaExpression(expression.get(1))}"

                        expression.hasFunctor(FunctorConstants.IF_THEN_ELSE)
                        -> javaExpression(expression.get(0)) + "? " +
                                javaExpression(expression.get(1)) + " : " +
                                javaExpression(expression.get(2))

                        else
                        -> throw Error("Unsupported expression: $expression")
                    } +
            ")"

    fun assignmentMap(assignment: IntArray): Map<Expression, Int> =
            assignment.mapIndexed { variableIndex, value ->  variables[variableIndex] to value }.toMap()
}