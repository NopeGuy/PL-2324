import ply.lex as lex
import ply.yacc as yacc

#NOTAS IMPORTANTES
#   TODO -> Não verificado / Dúvida
#   BUG -> bug
#   TODO: Por fazer

#Lista de BUG's
#BUG na p_str_prints

#Estados possíveis 
states = (
    #TODO Acho q functionDefState será inclusiva, uma vez que funções, darão para chamar outras dentro
    ('functionDefState','inclusive'), # Definição lógica da função
    ('funInpState','exclusive'), # Declaração de input de funções
    ('funOutState','exclusive'), # Declaração de output de funções
    ('conditionalState','inclusive'), # Inicia com IF
) #colocar vírgula no fim de cada tuplo!

#Tokens possíveis
tokens = (
'NUMBER',
'ADD',
'SUB',
'MUL',
'DIV',
'MOD',
'EQUALS',
'DIFF',
'GREQUAL', # >=
'LESEQUAL', # <=
'LESSER', # <
'GREATER', # >
'COMMENT', # \ comentario
'COMMENT2', # ( comentario )
'FUNCTIONSTART', # :
'FUNCTIONEND', # ;
'FUNIN', # ( |a b| -- ... )
'FUNOUT', # ( ... -- |out1 out2| )
'LPAREN', # (
'RPAREN', # )
'ARGSEP', # Separador de input/output das funções  --
'WORD', # Funções
'POPPRINT', # .
'PRINTDELIM', # ."
'EMIT', 
'KEY', 
'SPACES', 
'SPACE', 
'CHAR', # Função FORTH 
'CR',
'IF',
'THEN',
'ELSE',
'DO',
'LOOP',
'SWAP', #TODO:
'DUP', #TODO:
'OVER', #TODO:
'ROT', #TODO:
'DROP', #TODO:
'CHARACTER' # Representa um caracter apenas
) #colocar vírgula no fim de cada token!

#COMENTARIOS

def t_ANY_COMMENT(t):
    r'\\\s.+\n'

def t_ANY_COMMENT2(t):
    r'\(\s.+\s\)'

#FIM COMENTARIOS

#CONDICIONAIS

def t_INITIAL_IF(t):
    r'IF\b'
    t.lexer.push_state('conditionalState')
    return t

def t_INITIAL_conditionalState_ELSE(t):
    r'ELSE\b'
    return t
    
def t_INITIAL_conditionalState_THEN(t):
    r'THEN\b'
    t.lexer.pop_state()
    return t

#FIM CONDICIONAIS

#FUNCOES

def t_FUNCTIONSTART(t):
    r':\s'
    t.lexer.push_state('functionDefState')
    return t

def t_FUNCTIONEND(t):
    r';(?!.*;)'
    t.lexer.pop_state()
    return t

##ARGUMENTOS DE FUNCOES

def t_LPAREN(t):
    r'\(\s'
    t.lexer.push_state('funInpState')
    print("Entered funInpState")
 
def t_funOutState_RPAREN(t):
    r'\s\)'
    t.lexer.pop_state()
    print("Exited funOutState")

def t_funInpState_ARGSEP(t):
    r'\s--\s'
    t.lexer.pop_state()
    t.lexer.push_state('funOutState')
    print("Entered funOutState")
    return t

def t_funInpState_FUNIN(t):
    r'[^-\s\n]+'
    return t

def t_funOutState_FUNOUT(t):
    r'[^\s\)\n]+'
    return t


##FIM ARGUMENTOS DE FUNCOES


# LOW SPEFICIFICATIONS

def t_INITIAL_conditionalState_PRINTDELIM(t): 
    r'\."\s[^"]+\s"'
    t.value = t.value[3:-2]
    return t

def t_INITIAL_conditionalState_POPPRINT(t): 
    r'\.'                           
    return t

def t_INITIAL_conditionalState_KEY(t): 
    r'KEY\b'                           
    return t

def t_INITIAL_conditionalState_SPACES(t): 
    r'SPACES\b'
    return t

def t_INITIAL_conditionalState_SPACE(t): 
    r'SPACE\b'                           
    return t

def t_INITIAL_conditionalState_CR(t):
    r'CR\b'
    return t

def t_INITIAL_conditionalState_CHAR(t):
    r'CHAR\b'
    return t

def t_INITIAL_conditionalState_EMIT(t):
    r'EMIT\b'
    return t

def t_INITIAL_conditionalState_DO(t):
    r'DO\b'
    return t

def t_INITIAL_conditionalState_LOOP(t):
    r'LOOP\b'
    return t

def t_INITIAL_conditionalState_DUP(t):
    r'DUP\b'
    return t

def t_INITIAL_conditionalState_DROP(t):
    r'DROP\b'
    return t

def t_INITIAL_conditionalState_OVER(t):
    r'OVER\b'
    return t

def t_INITIAL_conditionalState_ROT(t):
    r'ROT\b'
    return t

def t_INITIAL_conditionalState_NUMBER(t):
    r'\d+'
    return t

def t_INITIAL_conditionalState_SWAP(t):
    r'SWAP\b'
    return t

def t_INITIAL_conditionalState_WORD(t): 
    r'[A-Z0-9][A-Z0-9]+'                          
    return t

def t_INITIAL_conditionalState_CHARACTER(t):
    r'[A-Z]'
    return t


t_INITIAL_conditionalState_ADD = r'\+'
t_INITIAL_conditionalState_SUB = r'-'
t_INITIAL_conditionalState_MUL = r'\*'
t_INITIAL_conditionalState_DIV = r'/'
t_INITIAL_conditionalState_MOD = r'%'
t_INITIAL_conditionalState_DIFF = r'<>'
t_INITIAL_conditionalState_GREQUAL = r'>='
t_INITIAL_conditionalState_LESEQUAL = r'<='
t_INITIAL_conditionalState_EQUALS = r'='
t_INITIAL_conditionalState_LESSER = r'<'
t_INITIAL_conditionalState_GREATER = r'>'


# IGNORES

t_ignore = ' \t\n'
t_funInpState_funOutState_ignore = ' \t\n'
t_conditionalState_ignore = ' \t\n'

# FIM IGNORES

def t_ANY_error(t):
    print('Illegal character: ',t.value[0],' Pos: ',t.lexpos)
    t.lexer.skip(1)

# MAIN

lexer = lex.lex(#debug=True
    )

forth = ''' 3 5 < 5 >'''


lexer.input(forth)

# print results from lexer
# while tok := lexer.token():
#    print(tok)

precedence = (
    ('left','SPACES','EMIT'),
    ('left', 'EQUALS', 'DIFF', 'GREQUAL', 'LESEQUAL', 'LESSER', 'GREATER'),
    ('left', 'ADD', 'SUB', 'MUL', 'DIV', 'MOD'),
    ('left', 'THEN'),
    ('right', 'ELSE'),
    ('right', 'IF'),
    
) # Virgulas no fim de cada tuplo

def p_exp_translate(p):
    '''exp : numExp
           | cond'''
    p[0] = p[1]

#TODO Mais 3 versões (sem input/output, sem input apenas, sem output apenas)
#TODO mudar exp do corpo da func
def p_exp_funcInOut(p):
    '''exp :  exp FUNCTIONSTART WORD LPAREN inputList ARGSEP outputList RPAREN exp FUNCTIONEND'''
    #Adicionar ao dicionario
    l_func = 'l' + str(parser.label) #label da func
    parser.label+=1
    nome = p[3]
    parser.func = nome
    data = (nome,l_func,0)
    parser.enderecos.append(data)

    # Definir com código da VM TODO
    p[0] = p[1]
    p[0] += l_func + ':\n'
    p[0] += p[9] # corpo da func
    p[0] += p[7] # pushg do output


def p_inputList(p):
    '''inputList : FUNIN
                 | inputList FUNIN'''
    for key,label,num in parser.enderecos:
        if key == parser.func:
            num+=1

    if len(p) == 2:
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]
    

def p_outputList(p):
    '''outputList : FUNOUT
                  | outputList FUNOUT'''
    # declarar variaveis globais (pushg) TODO
    parser.i+=1

#TODO Definição de WORD com verificação do numero de argumentos

def p_numExp_arit(p):
    '''numExp : numExp num ADD
              | numExp num SUB
              | numExp num MUL
              | numExp num DIV
              | numExp num MOD'''
    p[0] = p[1] + p[2]
    if p[3]== '+':
        p[0] += 'ADD\n'
    elif p[3] == '-':
        p[0] += 'SUB\n'
    elif p[3] == '/':
        p[0] += 'DIV\n'
    elif p[3] == '*':
        p[0] += 'MUL\n'
    elif p[3] == '%':
        p[0] += 'MOD\n'


def p_cond_relOp(p):
    '''cond : numExp num LESSER
            | numExp num GREATER
            | numExp num DIFF
            | numExp num GREQUAL
            | numExp num LESEQUAL
            | numExp num EQUALS'''
    p[0] = p[1] + p[2]
    if p[3] == '<':
        p[0] += 'INF\n'
    elif p[3] == '>':
        p[0] += 'SUP\n'
    elif p[3] == '<>':
        p[0] += 'NOT\n'
    elif p[3] == '<=':
        p[0] += 'INFEQ\n'
    elif p[3] == '>=':
        p[0] += 'SUPEQ\n'
    elif p[3] == '=':
        p[0] += 'EQUAL\n'


def p_exp_ifThen(p): 
    '''exp : cond IF exp THEN'''
    false = 'l' + str(parser.labels)
    p[0] = p[1]
    p[0] += 'JZ ' + false + '\n' # Salto caso seja falso
    p[0] += p[3] # Condição caso verdadeiro
    p[0] += false + ':\n' # Salto (falso)

    parser.labels+=1

def p_exp_ifElseThen(p):
    '''exp : cond IF exp ELSE exp THEN'''
    fi = 'l' + str(parser.labels)
    parser.labels+=1
    els = 'l' + str(parser.labels)
    parser.labels+=1
    fim = 'l' + str(parser.labels)
    p[0] = p[1]
    p[0] += 'JZ ' + els + '\n' # Salto caso seja falso
    p[0] += p[3] # V
    p[0] += 'jump ' + fim + '\n' # Salto para o fim
    p[0] += els + ':\n' # Salto F
    p[0] += p[5] # F
    p[0] += fim + ':\n' # fim
    
    parser.labels+=1


def p_num_char(p):
    '''num : CHAR CHARACTER'''
    p[0] = 'pushs "' + p[2] + '"\n'
    p[0] += 'CHRCODE\n'

def p_numExp_print(p): # func de output print sem argumentos
    '''numExp : numExp PRINTDELIM
              | numExp CR
              | numExp SPACE
              | numExp KEY'''
    if p[1] == 'CR':
        p[0] = 'writeln'
    elif p[1] == 'SPACE':
        p[0] = 'pushs " " \n'
    elif p[1] == 'KEY':
        p[0] = 'read\n'
        p[0] += 'chrcode\n'
        p[0] += 'writei\n'
    else: # PRINTDELIM
        p[0] = 'pushs "' + p[1] + '"\n'
        p[0] += 'writes \n'

def p_numExp_prints(p): # func de output print com argumentos - BUG Mudança de term para NUMBER deu erro (possivelment utilizar tag num como intermedia entre NUMBER e term)
    '''numExp : numExp num SPACES 
              | numExp num EMIT'''
    strt = 'l' + str(parser.labels) 
    parser.labels +=1
    fin = 'l' + str(parser.labels) 
    parser.labels +=1


    if p[2] == 'SPACES':
        #loop core 1
        p[0] += strt + ':\n'
        p[0] += 'dup 1\npushi 0\nsup\njz ' + fin + '\n'
        #print espaço
        p[0] += 'pushs " "\nwrites\n'
        #loop core 2
        p[0] += 'pushi 1\nsub\njump ' + strt + '\n'
        p[0] += fin + ':\n' + 'pop 1\n'
    else: # EMIT
        p[0] += 'writechr\n'

def p_numExp_loop(p):
    '''numExp : numExp num DO exp LOOP'''
    strt = 'l' + str(parser.labels) 
    parser.labels +=1
    fin = 'l' + str(parser.labels) 
    parser.labels +=1

    # Calculo do nº de ciclos 
    p[0] = p[1]
    p[0] += 'pushi ' + p[2] + '\n'
    p[0] += 'sub\n'
    #loop core 1
    p[0] += strt + ':\n'
    p[0] += 'dup 1\npushi 0\nsup\njz ' + fin + '\n'
    #código do loop
    p[0] += p[4]
    #loop core 2
    p[0] += 'pushi 1\nsub\njump ' + strt + '\n'
    p[0] += fin + ':\n' + 'pop 1\n'


def p_num_number(p):
    '''num : NUMBER'''
    p[0] = 'pushi ' + p[1] + '\n'


def p_numExp_translate(p):
    '''numExp : num'''
    p[0] = p[1]


def p_empty(p):
    'empty :'
    pass

# Error Handling

def p_numExp_arit_error(p):
    '''numExp : numExp error ADD
              | numExp error SUB
              | numExp error MUL
              | numExp error DIV
              | numExp error MOD'''
    print("Syntax error in arithmetric expression. Missing number for operation.")
    raise SyntaxError

def p_numExp_relOp_error(p):
    '''numExp : numExp error LESSER
              | numExp error GREATER
              | numExp error DIFF
              | numExp error GREQUAL
              | numExp error EQUALS'''
    print("Syntax error in logical expression. Missing number for operation.")
    raise SyntaxError

def p_exp_ifThen_error(p):
    '''exp : cond IF error THEN
           | error IF exp THEN
           | numExp IF exp THEN
           | exp IF exp THEN'''
    print('Syntax error in if-then conditional expression.')
    raise SyntaxError

def p_exp_ifElseThen_error(p):
    '''exp : cond IF exp error
           | cond IF exp ELSE error
           | cond IF exp ELSE exp error'''
    print('Syntax error in if-then-else conditional expression.')
    raise SyntaxError

def p_error(p):
    print("Syntax error in input!")
    raise SyntaxError

parser = yacc.yacc()

parser.enderecos = {} # key: WORD; label, número argumentos 
parser.i = 0 #Número de inputs disponiveis na stack
parser.labels = 1
parser.func = ""

# print results from yacc
res = 'START\n'
res += str(parser.parse(forth, lexer=lexer,debug=True))
res += 'STOP'

with open('res.txt', 'w') as file:
    file.write(res)