package com.sri.ai.grinder.interpreter

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.Expressions
import com.sri.ai.grinder.api.Context
import com.sri.ai.grinder.core.TrueContext
import com.sri.ai.grinder.helper.UniquelyNamedConstantIncludingBooleansAndNumbersPredicate
import com.sri.ai.grinder.library.FunctorConstants
import com.sri.ai.util.Util.join
import com.sri.ai.util.Util.println
import com.sri.ai.util.compilation.Compiler
import com.sri.ai.util.math.Rational

open class CompilationEvaluator(open val variables: List<Expression>) {

    protected val variableIndices = variables.mapIndexed { i, v -> v to i }.toMap()

    companion object {

        private val constants: Set<out Expression> =
                mutableSetOf("if . then . else .", "+", "and", "or", "not", "<", ">", "<=", ">=", "=", "!=")
                        .map(Expressions::makeSymbol)
                        .toMutableSet()

        @JvmStatic
        fun makeWithVariablesFrom(expression: Expression) = CompilationEvaluator(variablesInOrder(expression))

        @JvmStatic
        fun makeEvaluator(expression: Expression, variables: ArrayList<out Expression>): Evaluator? {
            return makeEvaluator(::CompilationEvaluator, expression, variables)
        }

        fun makeEvaluator(
                fromVariablesInOrderToCompilationEvaluatorInstance: (List<Expression>) -> CompilationEvaluator,
                expression: Expression,
                variables: ArrayList<out Expression>): Evaluator? {

            val compiler = fromVariablesInOrderToCompilationEvaluatorInstance(variables)
            val start = System.currentTimeMillis()
            val compiledClass = compiler.compile(expression)
            val end = System.currentTimeMillis()
            println("Compilation time: ${(end - start) / 1000} seconds")
            try {
                return compiledClass.getDeclaredConstructor().newInstance()
            } catch (exception: Exception) {
                throw java.lang.Error(exception)
            }
        }

        @JvmStatic
        fun variablesInOrder(expression: Expression?): List<Expression> {
            var context: Context = TrueContext()
            val constants: Set<out Expression> =
                    constants
            context = context.setIsUniquelyNamedConstantPredicate(UniquelyNamedConstantIncludingBooleansAndNumbersPredicate(constants))
            val variablesBeingReferenced = Expressions.getVariablesBeingReferenced(expression, context)
            return variablesBeingReferenced.toList()
        }
    }

    interface Evaluator {
        fun apply(assignment: IntArray): Any
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
            public Object apply(int[] assignment) {
                return ${java(expression)};
            }
            ${supplementaryDefinitions(expression)}
        }
        """.trimIndent()

    protected open fun supplementaryDefinitions(expression: Expression): String = ""

    data class TypedJava(val java: String, val type: String)

    protected open fun java(expression: Expression): String = typedJava(expression).java

    protected open fun typedJava(expression: Expression): TypedJava =
            parenthesize(when {
                expression.syntacticFormType == "Symbol" && isVariable(expression)
                -> TypedJava("assignment[${variableIndices[expression]}]", "int")

                expression.syntacticFormType == "Symbol"
                -> TypedJava(expression.toString(), constantSymbolJavaType(expression))

                expression.hasFunctor("+")
                -> TypedJava(
                        expression.arguments.joinToString(" + ") {java(it)},
                        numericResultJavaType(expression.arguments))

                expression.hasFunctor("and")
                -> TypedJava("${java(expression.get(0))} && ${java(expression.get(1))}", "boolean")

                expression.hasFunctor("or")
                -> TypedJava("${java(expression.get(0))} || ${java(expression.get(1))}", "boolean")

                expression.hasFunctor("not")
                -> TypedJava("!${java(expression.get(0))}", "boolean")

                expression.hasFunctor(">")
                -> TypedJava("${java(expression.get(0))} > ${java(expression.get(1))}", "boolean")

                expression.hasFunctor("<")
                -> TypedJava("${java(expression.get(0))} < ${java(expression.get(1))}", "boolean")

                expression.hasFunctor(">=")
                -> TypedJava("${java(expression.get(0))} >= ${java(expression.get(1))}", "boolean")

                expression.hasFunctor("<=")
                -> TypedJava("${java(expression.get(0))} <= ${java(expression.get(1))}", "boolean")

                expression.hasFunctor("=")
                -> TypedJava("${java(expression.get(0))} == ${java(expression.get(1))}", "boolean")

                expression.hasFunctor("!=")
                -> TypedJava("${java(expression.get(0))} != ${java(expression.get(1))}", "boolean")

                expression.hasFunctor(FunctorConstants.IF_THEN_ELSE)
                -> {
                    val condition = typedJava(expression.get(0)).java
                    val thenBranch = typedJava(expression.get(1))
                    val elseBranch = typedJava(expression.get(2))
                    if (thenBranch.type != elseBranch.type) {
                        throw Error(
                                "if then else branches must have the same type but got " +
                                        "${thenBranch.type} and ${elseBranch.type} " +
                                        "for ${thenBranch.java} and ${elseBranch.java}")
                    }
                    TypedJava(condition + "? " + thenBranch.java + " : " + elseBranch.java, thenBranch.type)
                }

                else
                -> throw Error("Unsupported expression: $expression")
            })

    private fun symbolJavaType(symbol: Expression): String {
        return if (isVariable(symbol)) "int" else constantSymbolJavaType(symbol)
    }

    private fun constantSymbolJavaType(symbol: Expression): String {
        return when (symbol.value) {
            is Rational -> "double"
            is Boolean -> "boolean"
            else -> throw Error("Constant symbol has value of unexpected type ${symbol.value.javaClass}: ${symbol.value}")
        }
    }

    private fun numericResultJavaType(arguments: List<Expression>): String {
        return if (arguments.any { symbolJavaType(it) == "double" }) "double" else "int"
    }

    private fun parenthesize(pair: TypedJava) = TypedJava("(" + pair.java + ")", pair.type)

    protected fun isVariable(expression: Expression) = variableIndices.containsKey(expression)

    fun assignmentMap(assignment: IntArray): Map<Expression, Int> {
        assert(assignment.size == variables.size) { "Assignment has ${assignment.size} value(s) but there are only ${variables.size} variable(s): ${assignment.joinToString()} and $variables" }
        return assignment.mapIndexed { variableIndex, value ->  variables[variableIndex] to value }.toMap()
    }
}