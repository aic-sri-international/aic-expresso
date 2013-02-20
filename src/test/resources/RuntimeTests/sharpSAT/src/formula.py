
_EQUALITY_ = "="
_DISEQUALITY_ = "!="
_AND_ = "and"
_OR_ = "or"
_NOT_ = "not"
_IMPLIES_ = "=>"
_IFF_ = "<=>"

_VARIABLES_ = "uvwxyzUVWXYZ"
_CONSTANTS_ = "abcdef0123456789"

class Formula:
    pass

class Literal(Formula):
    pass

class Equality(Literal):
    def __init__(self, s):
        if _EQUALITY_ in s and not (_DISEQUALITY_ in s):
            parts = s.split(_EQUALITY_)
            parts = [p.strip() for p in parts]
            if parts[0][0] in _VARIABLES_:
                self.var = parts[0]
                self.term = parts[1]
            elif parts[1][0] in _VARIABLES_:
                self.var = parts[1]
                self.term = parts[0]
            else:
                print "Warning: Neither terms of %s is a variable"%s
                self.term0 = parts[0]
                self.term1 = parts[0]
        else:
            raise Exception("%s is not an equality"%s)
        
    def __str__(self):
        return "%s %s %s"%(self.var, _EQUALITY_, self.term)

class Disequality(Literal):
    def __init__(self, s):
        if _DISEQUALITY_ in s:
            parts = s.split(_DISEQUALITY_)
            parts = [p.strip() for p in parts]
            if parts[0][0] in _VARIABLES_:
                self.var = parts[0]
                self.term = parts[1]
            elif parts[1][0] in _VARIABLES_:
                self.var = parts[1]
                self.term = parts[0]
            else:
                print "Warning: Neither terms of %s is a variable"%s
                self.term0 = parts[0]
                self.term1 = parts[0]
        else:
            raise Exception("%s is not a disequality"%s)
        
    def __str__(self):
        return "%s %s %s"%(self.var, _DISEQUALITY_, self.term)

class Conjunction(Formula):
    def __init__(self, c):
        self.elements = [convertIt(x) for x in c]
        
    def __getitem__(self, key):
        return self.elements[key]
        
    def __delitem__(self, key):
        del self.elements[key]
    
    def __str__(self):
        return "(" + " and ".join((str(e) for e in self.elements)) + ")"

class Disjunction(Formula):
    def __init__(self, c):
        self.elements = [convertIt(x) for x in c]
        
    def __getitem__(self, key):
        return self.elements[key]
        
    def __delitem__(self, key):
        del self.elements[key]
    
    def __str__(self):
        return "(" + " or ".join((str(e) for e in self.elements)) + ")"

class Not(Formula):
    def __init__(self, i):
        self.inner = convertIt(i)
        
    def __str__(self):
        return "not(%s)"%(self.inner)


def convertIt(obj):
    if isinstance(obj, Formula):
        return obj
    elif isinstance(obj, (list, tuple)):
        functor = obj[0]
        if functor == _AND_:
            return Conjunction(obj[1:])
        elif functor == _OR_:
            return Disjunction(obj[1:])
        elif functor == _NOT_:
            newObj = Not(obj[1])
            if isinstance(newObj.inner, Equality):
                return convertIt("%s != %s"%(newObj.inner.var, newObj.inner.term))
            elif isinstance(newObj.inner, Disequality):
                return convertIt("%s = %s"%(newObj.inner.var, newObj.inner.term))
            else:
                return newObj
        else:
            raise Exception("Unknown input: %s"%(obj))

    elif isinstance(obj, basestring):
        if _DISEQUALITY_ in obj:
            return Disequality(obj)
        elif _EQUALITY_ in obj:
            return Equality(obj)
        else:
            raise Exception("Unknown input: %s"%(obj))
            

def isLiteral(x):
    return isinstance(x, Literal) or (isinstance(x, Not) and isLiteral(x.inner))

def isOrClause(x):
    result = False
    if isLiteral(x):
        result = True
    elif isinstance(x, Disjunction):
        for d in x:
            if not isLiteral(d):
                break
        else:
            result = True
    return result

def isAndClause(x):
    result = False
    if isLiteral(x):
        result = True
    elif isinstance(x, Conjunction):
        for d in x:
            if not isLiteral(d):
                break
        else:
            result = True
    return result

def moveNotIn(x):
    if isinstance(x, Not):
        inn = x.inner
        if isinstance(inn, Equality):
            return convertIt("%s != %s"%(inn.var, inn.term))
        elif isinstance(inn, Disequality):
            return convertIt("%s = %s"%(inn.var, inn.term))
        elif isinstance(inn, Conjunction):
            dis = Disjunction([])
            dis.elements = [moveNotIn(Not(p)) for p in inn]
            return dis
        elif isinstance(inn, Disjunction):
            con = Conjunction([])
            con.elements = [moveNotIn(Not(p)) for p in inn]
            return con
        elif isinstance(inn, Not):
            return moveNotIn(inn.inner)
    raise Exception("The code should not reach here for %s"%(x))
    

def flatten(x):
    if isLiteral(x):
        return x
    elif isinstance(x, Not):
        return flatten(moveNotIn(x))
    elif isinstance(x, Conjunction):
        nc = []
        for c in x:
            if isinstance(c, Conjunction):
                fc = flatten(c)
                if isinstance(fc,  Conjunction):
                    nc = nc + fc.elements
                else:
                    nc = nc.append(fc)
            else:
                nc.append(flatten(c))
        if len(nc) == 1:
            return nc[0]
        return Conjunction(nc)
    elif isinstance(x, Disjunction):
        nc = []
        for c in x:
            if isinstance(c, Disjunction):
                fc = flatten(c)
                if isinstance(fc,  Disjunction):
                    nc = nc + fc.elements
                else:
                    nc = nc.append(fc)
            else:
                nc.append(flatten(c))
        if len(nc) == 1:
            return nc[0]
        return Disjunction(nc)
    else:
        raise Exception("Unknown input: "%(x))

def findElementForDistribution(x, ddd = False):
    if isinstance(x, Conjunction):
        elements = x.elements[:]
        for i in range(len(elements)):
            isItGood = (not isOrClause(x[i])) if ddd else isinstance(x[i], Disjunction)
            if isItGood:
                dis = x[i]
                del elements[i]
                return (flatten(Conjunction(elements)), dis)
    elif isinstance(x, Disjunction):
        elements = x.elements[:]
        for i in range(len(elements)):
            isItGood = (not isAndClause(x[i])) if ddd else isinstance(x[i], Conjunction)
            if isItGood:
                con = x[i]
                del elements[i]
                return (flatten(Disjunction(elements)), con)
    return None
 

def applyDistribution(x, e):
    if isinstance(e, Disjunction):
        e1 = e[0]
        e2 = flatten(Disjunction(e.elements[1:]))
        if isinstance(x, Conjunction):
            xe1 = x.elements[:] + [e1]
            xe2 = x.elements[:] + [e2]
        else:
            xe1 = [x, e1]
            xe2 = [x, e2]            
        c1 = Conjunction(xe1)
        c2 = Conjunction(xe2)
        return Disjunction([c1, c2])
    elif isinstance(e, Conjunction):
        e1 = e[0]
        e2 = flatten(Conjunction(e.elements[1:]))
        if isinstance(x, Disjunction):
            xe1 = x.elements[:] + [e1]
            xe2 = x.elements[:] + [e2]
        else:
            xe1 = [x, e1]
            xe2 = [x, e2]            
        c1 = Conjunction(xe1)
        c2 = Conjunction(xe2)
        return Conjunction([c1, c2])
    raise Exception("Bad arguments: %s and %s"%(x, e))

def isCNF(x):
    result = False
    if isOrClause(x):
        result = True
    elif isinstance(x, Conjunction):
        for d in x:
            if not isOrClause(d):
                break
        else:
            result = True
    return result
    
def isDNF(x):
    result = False
    if isAndClause(x):
        result = True
    elif isinstance(x, Disjunction):
        for d in x:
            if not isAndClause(d):
                break
        else:
            result = True
    return result
    
def toCNF(x):
    while not isCNF(x):
        x = flatten(x)
        if isinstance(x, Conjunction):
            if not isCNF(x):
                x = Conjunction([toCNF(e) for e in x])
        elif isinstance(x, Disjunction):  
            if not isCNF(x): # one disjunct is a conjunction
                res = findElementForDistribution(x)
                x = Conjunction([Disjunction([res[0], b]) for b in res[1].elements])
    return x

def toDNF(x):
    while not isDNF(x):
        x = flatten(x)
        if isinstance(x, Disjunction):
            if not isDNF(x):
                x = Disjunction([toDNF(e) for e in x])
        elif isinstance(x, Conjunction):  
            if not isDNF(x): # one disjunct is a conjunction
                res = findElementForDistribution(x)
                elements = res[0].elements if isinstance(res[0], Conjunction) else [res[0]]
                x = Disjunction([Conjunction([a, b]) for a in elements for b in res[1].elements]) 
    return x
    
def getVariablesAndConstants(x):
    vars = []
    cons = []
    if isinstance(x, Equality) or isinstance(x, Disequality):
        if x.var[0] in _VARIABLES_ and (not x.var in vars):
            vars.append(x.var)
        elif x.var[0] in _CONSTANTS_ and (not x.var in cons):
            cons.append(x.var)
        if x.term[0] in _VARIABLES_ and (not x.term in vars):
            vars.append(x.term)
        elif x.term[0] in _CONSTANTS_ and (not x.term in cons):
            cons.append(x.term)
        return (vars, cons)
    elif isinstance(x, Not):
        return getVariablesAndConstants(x.inner)
    elif isinstance(x, Conjunction) or isinstance(x, Disjunction):
        for e in x:
            vs, cs = getVariablesAndConstants(e)
            for vv in vs:
                if not vv in vars:
                    vars.append(vv)
            for cc in cs:
                if not cc in cons:
                    cons.append(cc)
        return (vars, cons)
    raise Exception("The code should not reach here for %s"%x)
            
    
if __name__ == "__main__":
    w = (_AND_, (_OR_, (_AND_, "x1 = a1", "x2 != a1"), (_AND_, "x1 = cons2", "x2 != cons2"), (_AND_, "x1 = cons3",  "x2 != cons3")) , "x1 != a1")
    #w = (_AND_, (_OR_, (_AND_, "x1 = a1", "x2 != a1"), (_AND_, "x1 = cons2", "x2 != cons2")) , "x1 != a1")
    a = [_AND_, [_OR_, "x != y", [_AND_, "w=x", "z = a", "v!=4"]], [_NOT_, "x =z"]]
    print "Original formula is ", str(w)
    ac = convertIt(w)
    print "Converted formula is %s\n"%(ac)
    b = toCNF(ac)
    print "\nCNF formula is %s"%(b)
    c = toDNF(ac)
    print "DNF formula is %s"%(c)
    d, e = getVariablesAndConstants(ac)
    print "Variables: %s\t\tConstants: %s"%(d, e)
    

    


