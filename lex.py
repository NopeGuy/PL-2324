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
    r'[A-Z0-9]+'                          
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

forth = ''' : W ( ab cd -- ) 3 5 + 5 - ;'''


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
    '''exp : numExp'''
    p[0] = p[1]

#TODO mudar exp do corpo da func
def p_exp_funcExp(p):
    '''exp : funNameDef arguments exp FUNCTIONEND'''
    p[0] = p[1] + p[3]
    p[0] += 'return\n'

def p_numExp_funcNumExp(p):
    '''exp : funNameDef arguments numExp FUNCTIONEND'''
    p[0] = p[1] + p[3]
    p[0] += 'return\n'

def p_funNameDef(p):
    '''funNameDef : FUNCTIONSTART WORD'''
    
    if p[1] in parser.enderecos:
        print('Function name: ' + p[2] + ' already exists.')
        raise SyntaxError
        
    l_func = 'l' + str(parser.labels) #label da fun
    parser.labels+=1
    nome = p[2]
    parser.func = nome
    data = ({nome: (l_func, 0)})
    parser.enderecos.update(data)
    print(parser.enderecos)
    p[0] = l_func + ':\n'

def p_arguments(p):
    '''arguments : LPAREN inputList outputList RPAREN
                 | LPAREN outputList RPAREN
                 | LPAREN inputList RPAREN
                 | LPAREN RPAREN
                 | empty''' # TODO Verificar se funciona com isto
    pass
            

def p_inputList(p):
    '''inputList : FUNIN
                 | inputList FUNIN'''
    for key,label,num in parser.enderecos:
        if key == parser.func:
            num+=1
    

def p_outputList(p):
    '''outputList : FUNOUT
                  | outputList FUNOUT'''
    pass


def p_numExp_arit(p):
    '''numExp : 2numExp ADD
              | 2numExp SUB
              | 2numExp MUL
              | 2numExp DIV
              | 2numExp MOD'''
    
    p[0] = p[1]
    if p[2]== '+':
        p[0] += 'ADD\n'
    elif p[2] == '-':
        p[0] += 'SUB\n'
    elif p[2] == '/':
        p[0] += 'DIV\n'
    elif p[2] == '*':
        p[0] += 'MUL\n'
    elif p[2] == '%':
        p[0] += 'MOD\n'


def p_cond_relOp(p):
    '''cond : 2numExp LESSER
            | 2numExp GREATER
            | 2numExp DIFF
            | 2numExp GREQUAL
            | 2numExp LESEQUAL
            | 2numExp EQUALS'''
    
    p[0] = p[1]
    if p[2] == '<':
        p[0] += 'INF\n'
    elif p[2] == '>':
        p[0] += 'SUP\n'
    elif p[2] == '<>':
        p[0] += 'NOT\n'
    elif p[2] == '<=':
        p[0] += 'INFEQ\n'
    elif p[2] == '>=':
        p[0] += 'SUPEQ\n'
    elif p[2] == '=':
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
    '''num : CHAR WORD'''

    if len(p[2]) > 1:
        print("SyntaxError - CHAR applied to more than one character.")
        raise SyntaxError

    p[0] = 'pushs "' + p[2] + '"\n'
    p[0] += 'CHRCODE\n'


def p_exp_printOnly(p):
    '''exp : PRINTDELIM
           | CR
           | SPACE
           | KEY'''
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

def p_exp_print(p): # func de output print sem argumentos
    '''exp : exp PRINTDELIM
           | exp CR
           | exp SPACE
           | exp KEY'''
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

def p_numExp_prints(p):
    '''exp : numExp SPACES 
           | 2numExp SPACES
           | numExp EMIT
           | 2numExp EMIT'''
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

def p_exp_loop2(p):
    '''exp : 2numExp DO exp LOOP'''

    strt = 'l' + str(parser.labels) 
    parser.labels +=1
    fin = 'l' + str(parser.labels) 
    parser.labels +=1

    counter = str(parser.g)
    parser.g += 1 
    limit = str(parser.g)
    parser.g += 1

    p[0] = p[1]

    p[0] += 'storeg ' + counter + ' \n'
    p[0] += 'storeg ' + limit + ' \n'

    p[0] += strt + ':\n'
    p[0] += p[3]

    p[0] += 'pushg ' + counter + '\n'
    p[0] += 'pushi 1\n'
    p[0] += 'sub\n'
    p[0] += 'storeg ' + counter + '\n'

    p[0] += 'pushg ' + counter + '\n'
    p[0] += 'pushg ' + limit + '\n'
    p[0] += 'sub\n'
    p[0] += 'jz ' + fin + '\n'
    p[0] += 'jump ' + strt + '\n'
    p[0] += fin + ':\n'
    p[0] += 'pop 1\n'



def p_num_number(p):
    '''num : NUMBER'''
    p[0] = 'pushi ' + p[1] + '\n'


def p_2numExp(p):
    '''2numExp : numExp num'''
    p[0] = p[1] + p[2]



def p_numExp_nums(p):
    '''numExp : num
              | cond
              | exp num'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[2]

def p_exp_function(p): # TODO Pode dar erro (ambiguidade)
    '''exp : exp WORD
           | WORD'''
    if p[1] not in parser.enderecos:
        print("Syntax Error - Function '" + p[1] + "' not defined.")
        raise SyntaxError
    
    if len(p) == 3:
        p[0] = p[1]
        p[0] += 'call ' + str(parser.enderecos[p[2]][0]) + '\n' #TODO Verificar se está a buscar a label corretamente
    else:
        p[0] = 'call ' + str(parser.enderecos[p[2]][0]) + '\n' #TODO Verificar

def p_empty(p):
    'empty :'
    pass

# Error Handling
#TODO Adicionar erros para arit e cond

def p_error(p):
    print("Syntax error in input!")
    raise SyntaxError

parser = yacc.yacc()

parser.enderecos = {} # key: WORD; label, número argumentos 
parser.labels = 1
parser.g = 0
parser.func = ""

# print results from yacc
res = 'START\n'
res += str(parser.parse(forth, lexer=lexer,debug=True))
res += 'STOP'

with open('res.txt', 'w') as file:
    file.write(res)
