/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.grinder.parser.antlr;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.INTEGER_INTERVAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.NOT;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.Token;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultCountingFormula;
import com.sri.ai.expresso.core.DefaultLambdaExpression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.helper.FunctionSignature;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAll;
import com.sri.ai.grinder.sgdpllt.library.boole.Or;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExists;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;
import com.sri.ai.util.Util;

@Beta
public class ExpressionVisitor extends AntlrGrinderBaseVisitor<Expression> {
	
	private Collection<FunctionSignature> randomPredicatesSignatures;
	
	// Note track bracketed expressions based on identity to ensure no accidental replacement by value.
	private Map<Expression, Expression> parenthesizedExpressions = new IdentityHashMap<Expression, Expression>(); 
	
	public ExpressionVisitor(Collection<FunctionSignature> randomPredicatesSignatures) {
		super();
		this.randomPredicatesSignatures = randomPredicatesSignatures;
	}

	// parenthesis, e.g.:(1+2)
	// '(' expr ')' #parenthesesAroundExpression
	@Override 
	public Expression visitParenthesesAroundExpression(
			AntlrGrinderParser.ParenthesesAroundExpressionContext ctx) {
		Expression result = visit(ctx.expr());
		
		// Keep track of explicitly bracketed expressions
		// so that the are not flattened as part of the 
		// possiblyFlatten()
		// call for some expressions, e.g.: 1 + 2 + 3.
		parenthesizedExpressions.put(result, result);
		return result;
	}
	
	// an expression symbol, e.g.:<X + Y>
	// '<' expr '>' #expressionSymbol
	@Override
	public Expression visitExpressionSymbol(
			AntlrGrinderParser.ExpressionSymbolContext ctx) {
		Expression expr   = visit(ctx.expr());
		Expression result = Expressions.makeSymbol(expr);
		return result;
	}
	
	
	// function application, e.g.: f(X)
	// functor=expr '(' ( args+=expr (',' args+=expr)* )? ')' # functionApplication
	@Override 
	public Expression visitFunctionApplication(
			AntlrGrinderParser.FunctionApplicationContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(randomPredicatesSignatures, visit(ctx.functor), expressions(ctx.args));
		
		return result;
	}
	
	@Override 
	public Expression visitCartesianProduct(AntlrGrinderParser.CartesianProductContext ctx) { 
		Object[] arguments = new Object[1+ctx.additionalargs.size()];
		arguments[0] = visit(ctx.firstarg);
		for (int i = 0; i < ctx.additionalargs.size(); i++) {
			arguments[i+1] = visit(ctx.additionalargs.get(i));
		}
		Expression result = Expressions.apply(FunctorConstants.CARTESIAN_PRODUCT, arguments);
		result = possiblyFlatten(result);
		return result;
	}
	
	@Override 
	public Expression visitFunctionType(AntlrGrinderParser.FunctionTypeContext ctx) { 
		Object[] arguments  = new Object[2];
		if (ctx.domaintypes.size() == 1) {
			arguments[0] = visit(ctx.domaintypes.get(0));
		}
		else {
			Object[] domainArgs = new Object[ctx.domaintypes.size()];
			for (int i = 0; i < ctx.domaintypes.size(); i++) {
				domainArgs[i] = visit(ctx.domaintypes.get(i));
			}
			Expression cartesianProduct = Expressions.apply(FunctorConstants.CARTESIAN_PRODUCT, domainArgs);
			cartesianProduct = possiblyFlatten(cartesianProduct);
			arguments[0] = cartesianProduct;
		}
		arguments[1] = visit(ctx.rangetype);
		Expression result = Expressions.apply(FunctorConstants.FUNCTION_TYPE, arguments);
		return result;
	}
	
	// tuple, e.g.: (A, B, C)
	// '(' expr ',' expr (',' expr)* ')' #tuple
	@Override 
	public Expression visitTuple(AntlrGrinderParser.TupleContext ctx) {	
		Expression result = Expressions.makeTuple(expressions(ctx.expr()));
		return result;
	}

	// counting formula, e.g.: | X in 1..10 : X < 5 |
	//  '|' ( indexes+=expr (',' indexes+=expr)* )? ':' body=expr '|' #countingFormula
	@Override 
	public Expression visitCountingFormula(AntlrGrinderParser.CountingFormulaContext ctx) { 
		Expression result = new DefaultCountingFormula(cartesianProductsToTupleType(expressionsList(ctx.indexes)), 
										visit(ctx.body));
		return result;
	}
	
	// cardinality, e.g.: | X |
	// '|' expr '|' #cardinality
	@Override
	public Expression visitCardinality(AntlrGrinderParser.CardinalityContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.CARDINALITY, visit(ctx.expr()));
		return result;
	}
	
	// intensional uniset, e.g.: { (on X) f(X) | X != a }
	// '{' ('{' ON ( scopeargs+=expr (',' scopeargs+=expr)* )? ')')? head=expr ('|' condition=expr)? '}' #intensionalUniset
	@Override
	public Expression visitIntensionalUniset(AntlrGrinderParser.IntensionalUnisetContext ctx) {
		Expression result = makeIntensionalSet(IntensionalSet.UNI_SET_LABEL, ctx.scope, ctx.scopeargs, ctx.head, ctx.condition);
		return result;
	}
	
	// intensional multiset, e.g.: {{ (on X) f(X) | X != a }}
	// '{{' ('(' ON ( scopeargs+=expr (',' scopeargs+=expr)* )? ')')? head=expr ('|' condition=expr)? '}}' #intensionalMultiset
	@Override
	public Expression visitIntensionalMultiset(
			AntlrGrinderParser.IntensionalMultisetContext ctx) {
		Expression result = makeIntensionalSet(IntensionalSet.MULTI_SET_LABEL, ctx.scope, ctx.scopeargs, ctx.head, ctx.condition);
		return result;
	}
	
	// extensional uniset, e.g.: { A, B, C, C, D }
	// '{' ( expr (',' expr)* )? '}' #extensionalUniset
	@Override
	public Expression visitExtensionalUniset(
			AntlrGrinderParser.ExtensionalUnisetContext ctx) {
		Expression result = ExtensionalSet.makeUniSet(expressionsList(ctx.expr()));
		return result;
	}
	
	// extensional multiset, e.g.: {{ A, B, C, C, D }}
	// '{{' ( expr (',' expr)* )? '}}' #extensionalMultiset
	@Override 
	public Expression visitExtensionalMultiset(AntlrGrinderParser.ExtensionalMultisetContext ctx) { 
		Expression result = ExtensionalSet.makeMultiSet(expressionsList(ctx.expr()));
		return result;
	}
	
	// bracketed expression, for parfactors and random variables, e.g. [if p(X) then 1 else 2]
	// '[' expr ']' #bracketedExpression
	@Override
	public Expression visitBracketedExpression(
			AntlrGrinderParser.BracketedExpressionContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(randomPredicatesSignatures, FunctorConstants.LEFT_DOT_RIGHT, visit(ctx.expr()));
		return result;
	}
	
	// not, e.g.: not A and B -> (not(A)) and B
	// NOT expr #not
	@Override
	public Expression visitNot(AntlrGrinderParser.NotContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(NOT, visit(ctx.expr()));
		return result;
	}
	
	// negative, e.g.: 2 * -1 -> 2 * (-1)
	// '-' expr #negative
	@Override
	public Expression visitNegative(AntlrGrinderParser.NegativeContext ctx) {
		Expression argument = visit(ctx.expr());
		Expression result;
		if (argument.getValue() instanceof Number) {
			result = Expressions.makeSymbol(argument.rationalValue().negate());
		}
		else {
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.MINUS, argument);
		}
		return result;
	}
	
	// exponentiation, e.g. 2^3^4 -> 2^(3^4)
	// base=expr '^'<assoc=right> exponent=expr #Exponentiation
	@Override
	public Expression visitExponentiation(AntlrGrinderParser.ExponentiationContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.EXPONENTIATION, visit(ctx.base), visit(ctx.exponent));
		return result;
	}
	
	// multiplication or division or integer interval, e.g.: 2*3/2 -> 2*(3/2)
	// numerator=expr op=('*' | '/' | '..') denominator=expr #multiplicationOrDivisionOrIntegerInterval
	@Override
	public Expression visitMultiplicationOrDivisionOrIntegerInterval(AntlrGrinderParser.MultiplicationOrDivisionOrIntegerIntervalContext ctx) {
		Expression result;	
		if (ctx.op.getText().equals("..")) {			
			result = apply(INTEGER_INTERVAL, visit(ctx.leftop), visit(ctx.rightop));
			// result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.INTEGER_INTERVAL, visit(ctx.leftop), visit(ctx.rightop));
			// using the above line places extra quotes around '..', not sure why.
		}
		else {			
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		}		
		result = possiblyFlatten(result);
		return result;
	}
	
	// addition or subtraction, e.g.: 1-2+3 -> (1-2)+3
	// leftop=expr op=('+' | '-') rightop=expr #additionOrSubtraction
	@Override
	public Expression visitAdditionOrSubtraction(AntlrGrinderParser.AdditionOrSubtractionContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}
	
	@Override
	public Expression visitRealInterval(AntlrGrinderParser.RealIntervalContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.leftBracket.getText()+" . ; . "+ctx.rightBracket.getText(), visit(ctx.lower), visit(ctx.upper));
		return result;
	}
	
	// set intersection, e.g.: {a, b, c} intersection {b}
	// leftop=expr INTERSECTION rightop=expr #intersection
	@Override
	public Expression visitIntersection(
			AntlrGrinderParser.IntersectionContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.INTERSECTION, visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}
	
	// set union, {a, b, c} union {b, d}
	// leftop=expr UNION rightop=expr #union
	@Override
	public Expression visitUnion(AntlrGrinderParser.UnionContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}	
	
	// set membership, x in {x, y, z}
	// leftop=expr IN rightop=expr #in
	@Override
	public Expression visitIn(AntlrGrinderParser.InContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.IN, visit(ctx.leftop), visit(ctx.rightop));
		return result;
	}
	
	// comparison operators, e.g.: X = Y, 2 < 3
	// leftop=expr op=('<' | '<=' | '=' | '!=' | '>=' | '>') rightop=expr #comparison
	@Override
	public Expression visitComparison(AntlrGrinderParser.ComparisonContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result;
	}
	
	// conjunction, e.g.: A or B and C -> A or (B and C)
	// leftconj=expr AND rightconj=expr #and
	@Override
	public Expression visitAnd(AntlrGrinderParser.AndContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(And.FUNCTOR, visit(ctx.leftconj), visit(ctx.rightconj));
		result = possiblyFlatten(result);
		return result;
	}
	
	// disjunction, e.g.: A => B or C -> A => (B or C)
	// leftdisj=expr OR rightdisj=expr #or
	@Override
	public Expression visitOr(AntlrGrinderParser.OrContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(Or.FUNCTOR, visit(ctx.leftdisj), visit(ctx.rightdisj));
		result = possiblyFlatten(result);
		return result;
	}
	
	// implication, e.g.: A = B => C = D
	// antecedent=expr IMPLICATION consequent=expr #implication
	@Override
	public Expression visitImplication(AntlrGrinderParser.ImplicationContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.IMPLICATION, visit(ctx.antecedent), visit(ctx.consequent));
		return result;
	}
	
	// biconditional, e.g.: A = B <=> C = D
	// leftop=expr BICONDITIONAL rightop=expr #biconditional
	@Override
	public Expression visitBiconditional(
			AntlrGrinderParser.BiconditionalContext ctx) {
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.EQUIVALENCE, visit(ctx.leftop), visit(ctx.rightop));
		return result;
	}
	
	// conditional, e.g.: if X = Y then 1 else 2
	// IF condition=expr THEN thenbranch=expr ELSE elsebranch=expr #ifThenElse
	@Override
	public Expression visitIfThenElse(AntlrGrinderParser.IfThenElseContext ctx) {
		Expression condition  = visit(ctx.condition);
		Expression thenBranch = visit(ctx.thenbranch);
		Expression elseBranch = visit(ctx.elsebranch);
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.IF_THEN_ELSE, condition, thenBranch, elseBranch);
		return result;
	}
	
	// lambda, e.g.: lambda f(X) : 2 + f(X)
	// LAMBDA ( parameters+=expr (',' parameters+=expr)* )? ':' body=expr #lamda
	@Override
	public Expression visitLamda(
			AntlrGrinderParser.LamdaContext ctx) {
		Expression result = new DefaultLambdaExpression(cartesianProductsToTupleType(expressionsList(ctx.parameters)), visit(ctx.body));
		return result;
	}
	
	// universal quantification, e.g.: for all X : X != a
	// FOR ALL index=expr ':' body=expr #forAll
	@Override
	public Expression visitForAll(AntlrGrinderParser.ForAllContext ctx) {
		Expression result = ForAll.make(visit(ctx.index), visit(ctx.body));
		return result;
	}

	// existential quantification, e.g.: there exists X : X = a
	// THERE EXISTS index=expr ':' body=expr #thereExists
	@Override
	public Expression visitThereExists(AntlrGrinderParser.ThereExistsContext ctx) {
		Expression result = ThereExists.make(visit(ctx.index), visit(ctx.body));
		return result;
	}
	
	@Override
	public Expression visitSymbol(AntlrGrinderParser.SymbolContext ctx) { 	
		Expression result = newSymbol(ctx.getText());
		return result;
	}

	//
	// PROTECTED
	//
	protected Expression newSymbol(String text) {
		Expression result = Expressions.parseTextAndMakeSymbolOrStringLiteral(text);
		return result;
	}
	
	protected Expression makeIntensionalSet(Object label, Token scope, List<AntlrGrinderParser.ExprContext> scopeargs, 
			AntlrGrinderParser.ExprContext head, AntlrGrinderParser.ExprContext condition) {
		List<Expression> indexExpressionsList = Util.list();
		if (scope != null) {
			indexExpressionsList = cartesianProductsToTupleType(expressionsList(scopeargs));
		}
		Expression headExpression      = visit(head);
		Expression conditionExpression = null;
		if (condition != null) {
			conditionExpression = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(IntensionalSet.CONDITION_LABEL, visit(condition));
		}
	
		Expression result = null;
		if (scope == null && conditionExpression == null) {
			// We need to construct an extensional set in this case
			if (label.equals(IntensionalSet.UNI_SET_LABEL)) {
				result = ExtensionalSet.makeUniSet(headExpression);
			}
			else {
				result = ExtensionalSet.makeMultiSet(Arrays.asList(headExpression));
			}
		}
		else {
			conditionExpression = conditionExpression == null? Expressions.TRUE : conditionExpression.get(0); // contains the "|";
			result = IntensionalSet.make(label, new ExtensionalIndexExpressionsSet(indexExpressionsList), headExpression, conditionExpression);
		}
		
		return result;
	}
	
	protected Expression[] expressions(List<AntlrGrinderParser.ExprContext> exprContexts) {
		List<Expression> result = new ArrayList<Expression>();
		for (AntlrGrinderParser.ExprContext exprContext : exprContexts) {
			result.add(visit(exprContext));
		}
		return result.toArray(new Expression[result.size()]);
	}
	
	protected List<Expression> expressionsList(List<AntlrGrinderParser.ExprContext> exprContexts) {
		List<Expression> indexExpressionsList = new ArrayList<Expression>();
		for (AntlrGrinderParser.ExprContext exprContext : exprContexts) {
			indexExpressionsList.add(visit(exprContext));
		}
		return indexExpressionsList;
	}
	
	protected List<Expression> cartesianProductsToTupleType(List<Expression> indexExpressions) {
		List<Expression> result = indexExpressions.stream()
				.map(ExpressionVisitor.this::cartesianProductToTupleType)
				.collect(Collectors.toList());
		
		return result;
	}
	
	protected Expression cartesianProductToTupleType(Expression indexExpression) {
		Expression result = indexExpression;
		
		if (indexExpression.hasFunctor(FunctorConstants.IN)) {
			result = replaceCartesianProductWithTupleType(indexExpression);
		}
		
		return result;
	}
	
	protected Expression replaceCartesianProductWithTupleType(Expression expression) {
		Expression result = Expressions.replaceImmediateSubexpressions(expression, e -> {
			if (e.hasFunctor(FunctorConstants.CARTESIAN_PRODUCT) && 
				// Ensure its not the cartesian product argument to a function type.
				!(expression.hasFunctor(FunctorConstants.FUNCTION_TYPE) && e == expression.get(0) && expression.numberOfArguments() == 2)) {					
				return Expressions.apply(FunctorConstants.TUPLE_TYPE, 
						replaceCartesianProductWithTupleType(e).getArguments());
			}
			else {
				return replaceCartesianProductWithTupleType(e);
			}
		});
		return result;
	}
	
	protected Expression possiblyFlatten(Expression expression) {
		Expression result = expression;
		
		Object functor = expression.getFunctor();
		if (functor != null) {
			if (functor.equals(FunctorConstants.TIMES) || 
				functor.equals(FunctorConstants.PLUS) || 
			    functor.equals(FunctorConstants.INTERSECTION) || 
			    functor.equals(FunctorConstants.UNION) || 
			    functor.equals(FunctorConstants.EQUAL) ||
			    functor.equals(FunctorConstants.AND) || 
			    functor.equals(FunctorConstants.OR) ||
			    functor.equals(FunctorConstants.CARTESIAN_PRODUCT)) {
				List<Expression> args = new ArrayList<Expression>();
				for (Expression arg : expression.getArguments()) {
					if (arg.getFunctor() != null && functor.equals(arg.getFunctor()) && !parenthesizedExpressions.containsKey(arg)) {
						args.addAll(arg.getArguments());
					}
					else {
						args.add(arg);
					}
				}
				result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(functor, args.toArray());
			}
		}
		
		// Clear in order manage memory
		parenthesizedExpressions.clear();
		
		return result;
	}
}
