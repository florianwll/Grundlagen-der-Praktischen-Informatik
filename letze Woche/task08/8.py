def showLit(literal):
    atom, truth_value = literal
    return f"{'~' if not truth_value else ''}{atom}"

# Beispieltest
print(showLit(('A', False)) + "\n" + showLit(('B', True)))

def showClause(clause):
    return " v ".join(showLit(literal) for literal in clause)

def showCNF(cnf):
    return " ^ ".join(f"({showClause(clause)})" for clause in cnf)

# Beispieltest
a = [('B', False)]
b = [('A', True), ('B', True), ('C', False)]
c = [a, b]

print(showClause(a) + "\n" + showClause(b))
print(showCNF(c))

def alphaLit(literal, alpha):
    atom, truth_value = literal
    return (atom in alpha) == truth_value

def alphaClause(clause, alpha):
    return any(alphaLit(literal, alpha) for literal in clause)

def alphaCNF(cnf, alpha):
    return all(alphaClause(clause, alpha) for clause in cnf)

# Beispieltest
alpha = ['A', 'B']

print(alphaLit(('A', True), alpha))  # True
print(alphaLit(('B', False), alpha))  # False
print(alphaLit(('C', True), alpha))  # False
print(alphaClause(a, alpha))  # False
print(alphaClause(b, alpha))  # True
print(alphaCNF(c, alpha))  # False

# Beispieltest für erfüllende und nicht erfüllende Belegung
alpha_erfüllend = ['A', 'B']
alpha_nichterfüllend = ['A']

print("Erfüllende Belegung:", alpha_erfüllend)
print("alphaCNF(c, alpha_erfüllend):", alphaCNF(c, alpha_erfüllend))  # True

print("Nicht erfüllende Belegung:", alpha_nichterfüllend)
print("alphaCNF(c, alpha_nichterfüllend):", alphaCNF(c, alpha_nichterfüllend))  # False
