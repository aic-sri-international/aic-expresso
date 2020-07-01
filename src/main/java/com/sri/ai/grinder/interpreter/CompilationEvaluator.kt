package com.sri.ai.grinder.interpreter

import com.sri.ai.expresso.api.Expression
import com.sri.ai.grinder.library.FunctorConstants
import com.sri.ai.util.Util.join
import com.sri.ai.util.compilation.Compiler

open class CompilationEvaluator(open val variables: List<Expression>) {

    protected val variableIndices = variables.mapIndexed { i, v -> v to i }.toMap()

    interface Evaluator {
        fun apply(assignment: IntArray): Int
    }

    fun evaluate(expression: Expression, assignment: IntArray) =
        compile(expression).getDeclaredConstructor().newInstance().apply(assignment)

    @Suppress("MemberVisibilityCanBePrivate")
    fun compile(expression: Expression): Class<Evaluator> {
        val evaluatorClassDefinition = evaluatorClassDefinition(expression)
        val compilation = Compiler.compile<Evaluator>("CompiledEvaluator", evaluatorClassDefinition)
        if (compilation.succeeded) {
            return compilation.compiledClass
        }
        else {
            throw Error(join("\n", compilation.diagnostics.diagnostics))
        }
    }

    open fun evaluatorClassDefinition(expression: Expression) =
        """
        public class CompiledEvaluator implements com.sri.ai.grinder.interpreter.CompilationEvaluator.Evaluator {

            @Override
            public int apply(int[] assignment) {
                return ${javaExpression(expression)};
            }
            ${supplementaryDefinitions(expression)}
        }
        """.trimIndent()

    protected open fun supplementaryDefinitions(expression: Expression): String = ""

    protected open fun javaExpression(expression: Expression): String = javaExpressionAndType(expression).first

    protected open fun javaExpressionAndType(expression: Expression): Pair<String, String> =
            parenthesize(when {
                expression.syntacticFormType == "Symbol" && isVariable(expression)
                -> Pair("assignment[${variableIndices[expression]}]", "int")

                expression.syntacticFormType == "Symbol"
                -> Pair(expression.toString(), "int")

                expression.hasFunctor("+")
                -> Pair(expression.arguments.joinToString(" + ") { javaExpression(it) }, "int")

                expression.hasFunctor(">")
                -> Pair("${javaExpression(expression.get(0))} > ${javaExpression(expression.get(1))}", "boolean")

                expression.hasFunctor("<")
                -> Pair("${javaExpression(expression.get(0))} < ${javaExpression(expression.get(1))}", "boolean")

                expression.hasFunctor(">=")
                -> Pair("${javaExpression(expression.get(0))} >= ${javaExpression(expression.get(1))}", "boolean")

                expression.hasFunctor("<=")
                -> Pair("${javaExpression(expression.get(0))} <= ${javaExpression(expression.get(1))}", "boolean")

                expression.hasFunctor("=")
                -> Pair("${javaExpression(expression.get(0))} == ${javaExpression(expression.get(1))}", "boolean")

                expression.hasFunctor("!=")
                -> Pair("${javaExpression(expression.get(0))} != ${javaExpression(expression.get(1))}", "boolean")

                expression.hasFunctor(FunctorConstants.IF_THEN_ELSE)
                -> {
                    val condition = javaExpressionAndType(expression.get(0)).first
                    val thenBranch = javaExpressionAndType(expression.get(1))
                    val elseBranch = javaExpressionAndType(expression.get(2))
                    if (thenBranch.second != elseBranch.second) {
                        throw Error(
                                "if then else branches must have the same type but got " +
                                        "${thenBranch.second} and ${elseBranch.second} " +
                                        "for ${thenBranch.first} and ${elseBranch.first}")
                    }
                    Pair(condition + "? " + thenBranch.first + " : " + elseBranch.first, thenBranch.second)
                }

                else
                -> throw Error("Unsupported expression: $expression")
            })

    private fun parenthesize(pair: Pair<String, String>) = Pair("(" + pair.first + ")", pair.second)

    protected fun isVariable(expression: Expression) = variableIndices.containsKey(expression)

    fun assignmentMap(assignment: IntArray): Map<Expression, Int> =
            assignment.mapIndexed { variableIndex, value ->  variables[variableIndex] to value }.toMap()
}