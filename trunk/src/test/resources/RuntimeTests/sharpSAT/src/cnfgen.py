import sys, os, time,  os.path,  gc
import subprocess,  math
from formula import *
from formulas_list import *

variables = {}
constants = {}
lines = []
clause_num = 0
comments = True
runSharpSAT = True
ignoreExistingFile = True
table = []
maxTime = 60000 * 2 # 2 minutes
usingCNFFormulas = False
sizes = (3, 4)
IGNORE = cnf_formulas


def getIndex(literal):
    global variables, constants
    if isinstance(literal, Not):
        return -getIndex(literal.inner)
    const_num = len(constants)
    neg = 1
    if isinstance(literal, Disequality):
        neg = -1
    vidx = variables[literal.var]
    cidx = constants[literal.term]
    val = neg*((vidx-1)*const_num + cidx)
    return val
    
def expandVariableVariableLiterals(formula):
    global constants
    global _VARIABLES_

    if isLiteral(formula):
        if formula.term[0] in "uvwxyzUVWXYZ":
            sgn = "=" if isinstance(formula, Equality) else "!="
                
            conjunctions = [Conjunction(["%s = %s"%(formula.var, c), "%s %s %s"%(formula.term, sgn, c)]) for c in constants.keys()]
            return (Disjunction(conjunctions),  True)
        return (formula,  False)
    if isinstance(formula, Not):
        expanded = expandVariableVariableLiterals(formula.inner)
        return (Not(expanded[0]),  expanded[1])
    elif isinstance(formula, Conjunction):
        expanded = [expandVariableVariableLiterals(e) for e in formula.elements]
        conjuncts,  shouldNegateList = zip(*expanded)
        shouldNegate = (True in shouldNegateList)
        return (Conjunction(conjuncts),  shouldNegate)
    elif isinstance(formula, Disjunction):
        expanded = [expandVariableVariableLiterals(e) for e in formula.elements]
        disjuncts,  shouldNegateList = zip(*expanded)
        shouldNegate = (True in shouldNegateList)       
        return (Disjunction(disjuncts),  shouldNegate)
    raise Exception("The code should not reach here for %s"%(formula))
    

def clause2CNFNumbers(clause):
    clause = flatten(clause)
    if isLiteral(clause):
        return "%s 0"%getIndex(clause)
    elif isOrClause(clause):
        return " ".join([str(getIndex(c)) for c in clause.elements]) + " 0"

def addClause(clause):
    global lines, clause_num
    line = clause2CNFNumbers(clause)
    lines.append(line)
    clause_num = clause_num + 1

def write(f, lines):
    for line in lines:
        f.write(line)
        f.write("\n")
        
def prepareFormula(formula):
    if usingCNFFormulas:
        f1 = moveNotIn(Not(formula))
        f2,  shouldNegate = expandVariableVariableLiterals(f1)
        if shouldNegate:
            f3 = moveNotIn(Not(f2))
            return (toCNF(f3),  False)
        else:
            return (toCNF(f2),  True)
    else:
        f1,  shouldNegate = expandVariableVariableLiterals(formula)
        f2 = flatten(Not(f1))
        f3 = toCNF(f2)
        return (f3,  True)
    
def makefile(theFormula,  cons_num,  filename = None):
    global variables,  constants,  comments,  clause_num,  lines
    lines = []
    variables = {}
    constants = {}
    clause_num = 0    
    vs, cs = getVariablesAndConstants(theFormula)
    i = 1
    for v in vs:
        variables[v] = i
        i = i + 1
    i = 1
    for c in cs:
        constants[c] = i
        i = i + 1
    while i <= cons_num:
        constants["cons%s"%i] = i
        i = i + 1
        
    var_num = len(vs)
    lines = []
    starting = time.time()

    print "Creating clauses..."
    if comments:
        lines.append("\nc Each of the following rows say xi is one of the elements in the universe:")
    for v in variables.keys():
        orClause = Disjunction(["%s = %s"%(v, c) for c in constants.keys()])
        addClause(orClause)

    if comments:
        lines.append("\nc Each of the following rows say xi is at most one of the elements in the universe:")
    for k in range(var_num):
        for i in range(cons_num):
            for j in range(i):
                clause = Disjunction(["%s != %s"%(variables.keys()[k], constants.keys()[i]), "%s != %s"%(variables.keys()[k], constants.keys()[j])])
                addClause(clause)
        
    cnff,  shouldSubstract = prepareFormula(theFormula)
    #print "CNF is: %s"%cnff
    if comments:
        lines.append("\nc Each of the following rows is a clause describing f:")
        #lines.append("c CNF form: %s"%cnff)
        #lines.append("c CNF form: %s"%cnff)
    if isinstance(cnff,  Conjunction):
        for clause in cnff:
            addClause(clause)
    else:
        addClause(cnff)
           
    if comments:
        lines.insert(0, "c %s variables and %s clauses"%(var_num*cons_num, clause_num))
    lines.insert(1, "p cnf %s %s"%(var_num*cons_num, clause_num))
    
    print "Writing to %s..."%filename
    if filename == None:
        write(sys.stdout, lines)
    else:
        f = open(filename, "wt")
        #filepath = os.path.abspath(f.name)
        write(f, lines)
        f.close()
    ending = time.time()
    difference = int(1000*(ending - starting))
    print "Time to create %s: %sms"%(filename,  difference)
    #print "Number of clauses: %s"%clause_num
    return (difference,  shouldSubstract, clause_num)
    
def executeSharpSAT(filename):
    pathToSharpSAT = "/home/saadati/projects/Formula/"
    sharpSAT = pathToSharpSAT + "sharpSAT"
    args = [sharpSAT,  pathToSharpSAT + filename]
    if os.path.exists(pathToSharpSAT + filename):
        output = subprocess.check_output(args)
        #print output
        lines = output.split("\n")
        timeline = lines[len(lines)-3]
        theTime = int(float(timeline[6:][:-1])  * 1000 + 0.5)
        solution = int(lines[len(lines) - 6].strip())
        #print "sharpSAT solution: %s"%solution
        print "sharpSAT time: %s"%theTime
        return (theTime,  solution)
    else:
        print "%s does not exist..."%filename
        return -1

def formatFormulaAsOurAlgorithm(index):
    theFormula = convertIt(formulas[index-1])
    vs, cs = getVariablesAndConstants(theFormula)
    vsStr = ", ".join(vs)
    return "\"| { ( on %s ) ( %s ) | %s } |\""%(vsStr,  vsStr,  theFormula)

def execute():
    global sizes, formulas, usingCNFFormulas, IGNORE, maxTime, table
    for domainSize in sizes:
        for i in range(len(formulas)):
            gc.collect()
            if usingCNFFormulas:
                filename = "cnf%s-s%s.cnf"%(i+1,  domainSize)
            else:
                filename = "dnf%s-s%s.cnf"%(i+1,  domainSize)
            if (not ignoreExistingFile) and os.path.exists(filename):
                print "%s already exists. Skipping..."%filename
                continue
            formula = formulas[i]
            if formula in IGNORE:
                print "Ignoring f%s..."%(i+1)
                table.append([domainSize, None,  None,  None,  None])
                continue
            theFormula = convertIt(formula)
            vs, cs = getVariablesAndConstants(theFormula)
            if domainSize < len(cs):
                raise Exception("Formula %s has %s many constants"%(i+1,  len(cs)))
            indices = len(vs)
            print "Formula: %s"%theFormula
            timeToMakeFile,  shouldSubstract, numberOfClauses = makefile(theFormula,  domainSize,  filename)
            if timeToMakeFile > maxTime:
                print "It took too long to generate CNF, skipping the remaining cases for formula %s"%(i+1)
                IGNORE.append(formula)
                
            name = "cnf-%s"%(i+1) if usingCNFFormulas else "dnf-%s"%(i+1)
            if runSharpSAT:
                (timeToRunSharpSAT,  solution) = executeSharpSAT(filename)
                if timeToRunSharpSAT > -1:
                    sharpSATFinalSolution = int(math.pow(domainSize,  indices) - solution) if shouldSubstract else solution
                    
                    table.append([name, domainSize, timeToMakeFile, timeToRunSharpSAT, timeToMakeFile+timeToRunSharpSAT, sharpSATFinalSolution, numberOfClauses])
            else:
                table.append([name, domainSize, timeToMakeFile, numberOfClauses])
            print
    print "____________________________________________"
    
def countLiterals(formula):
    if isinstance(formula,  Literal):
        return 1
    elif isinstance(formula,  Not):
        return countLiterals(formula.inner)
    else:
        return sum([countLiterals(e) for e in formula.elements])

if __name__ == "__main__":
    if runSharpSAT:
        titles = ["Name", "Domain Size", "Time to Make File", "Time to Run sharpSAT", "Total Time", "sharpSAT Final Solution", "Number of Clauses"]
    else:
        titles = ["Name", "Domain Size", "Time to Make File", "Number of Clauses"]
    table.append(titles)
    
    starting = time.time()

    usingCNFFormulas = True
    formulas = cnf_formulas
    print "Solving the CNF formulas"
    execute()

    usingCNFFormulas = False
    formulas = dnf_formulas
    print "Solving the DNF formulas"
    execute()
    
    sharpSATTime = {}
    totalTime = {}
    for s in sizes:
        sharpSATTime[s] = 0
        totalTime[s] = 0

    f = open("sharpSAT-running-data.csv",  "w")
    f.write(",".join(table[0]) + "\n")
    for row in table[1:]:
        sharpSATTime[row[1]] = sharpSATTime[row[1]] + row[3]
        totalTime[row[1]] = totalTime[row[1]] + row[4]
        f.write(",".join([str(item) for item in row]) + "\n")
    f.close()
    
    print "sharpSATTime: %s"%(sharpSATTime, )
    print "totalTime: %s"%(totalTime, )

    ending = time.time()
    difference = int(1000000*(ending - starting))
    print "The entire process took %s seconds."%(difference)    
