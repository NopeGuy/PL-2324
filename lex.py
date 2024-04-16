import ply.lex as lex

#NOTAS IMPORTANTES
#   NENHUMA função deve ser incluida no 'commentState', logo quando for adicionado um novo estado
#       poderá ser necessário adicioná-lo a todas as outras funções
#   TODO -> Não verificado / Dúvida
#   BUG -> bug
#   TODO: Por fazer

#Lista de BUG's
#Quando adicionado um '\n' dentro dos argumentos de uma função dá erro
#Funções CHAR, SPACES e EMIT n estão a funcionar devido aos espaços, provavelmente deverão ser modificadas para n receber argumentos e ser tratado no yacc

#Estados possíveis 
states = (
    #TODO Acho q functionDefState será inclusiva, uma vez que funções, darão para chamar outras dentro
    ('functionDefState','inclusive'), # Definição lógica da função
    ('commentState','exclusive'), # Comentário normal '\'
    ('functionArgState','exclusive'), # Argumentos de funções (input -- output) -> : FUN ( |o que está aqui dentro| ) logica ;
) #colocar vírgula no fim de cada tuplo!

literals = [] # TODO Ver que chars serão iguais tanto em forth como na VM

#Tokens possíveis
tokens = (
'NUMBER',
'ADD',
'SUB',
'MUL',
'DIV',
'MOD',
'SHORTADD',
'SHORTSUB',
'SHORTMUL',
'SHORTDIV',
'COMMENTSTART', # Inicio de comentário
'COMMENT', #Tudo que for com a tipo comment será escrito como está
'FUNCTIONSTART', # :
'NEWLINE', # \n (para commentState apenas)
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
'CHAR', 
'CR',
) #colocar vírgula no fim de cada tuplo!

#COMENTARIOS (Deverá permanecer no topo)

#Ignorar comentários através de função, no caso de haver regex mais especificos que anulem
def t_INITIAL_functionDefState_COMMENTSTART(t):  #TODO Verificar estados
    r'\\ '
    if(t.lexer.current_state()!='commentState'):
        t.lexer.push_state('commentState')
        print("Entered commentState")
    return t
#Alternativa ao t_ANY_COMMENT # t_ANY_ignore_COMMENT = r'\\.*' 

def t_commentState_COMMENT(t):
    r'[^\n]+'
    return t

def t_commentState_NEWLINE(t):
    r'\n'
    t.lexer.pop_state()
    print("Ended commentState")

def t_commentState_error(t):
    print('Illegal comment character: ',t.value[0],' Pos: ',t.lexpos)
    t.lexer.skip(1)

#FIM COMENTARIOS

#FUNCOES

def t_FUNCTIONSTART(t):
    r': '
    t.lexer.push_state('functionDefState')
    return t

def t_functionDefState_FUNCTIONEND(t):
    r';(?!.*;)'
    t.lexer.pop_state()
    return t

##ARGUMENTOS DE FUNCOES

def t_functionDefState_LPAREN(t):
    r'\( '
    t.lexer.push_state('functionArgState')
    print("Entered functionArgState")
 
def t_functionArgState_RPAREN(t):
    r' \)'
    t.lexer.pop_state()
    print("Exited functionArgState") 

def t_functionArgState_ARGSEP(t):
    r'--'
    return t

def t_functionArgState_FUNIN(t):
    r'[^- ]+(?=.+--)'
    return t

def t_functionArgState_FUNOUT(t):
    r'[^ )]+(?=.+\))'
    return t


##FIM ARGUMENTOS DE FUNCOES

#TODO: Colocar aqui as condicionais


#FIM FUNCOES

#Abreviação Aritmética 
def t_INITIAL_functionDefState_SHORTADD(t):
    r'\d+[+]'
    t.value = int(t.value[:-1])
    return t

def t_INITIAL_functionDefState_SHORTSUB(t):
    r'\d+[-]'
    t.value = int(t.value[:-1])
    return t

def t_INITIAL_functionDefState_SHORTMUL(t):
    r'\d+[*]'
    t.value = int(t.value[:-1])
    return t

def t_INITIAL_functionDefState_SHORTDIV(t):
    r'\d+[\\]'
    t.value = int(t.value[:-1])
    return t
#FIM Abreviação Aritmética


# LOW SPEFICIFICATION (Colocar aqui as funções menos específicas)
#TODO: Falta avaliar a ordem destas funções

def t_INITIAL_functionDefState_PRINTDELIM(t): 
    r'\.".+"'
    t.value = t.value[2:-1]                       
    return t

def t_INITIAL_functionDefState_POPPRINT(t): 
    r'\.'                           
    return t

def t_INITIAL_functionDefState_KEY(t): 
    r'KEY\b'                           
    return t

def t_INITIAL_functionDefState_SPACES(t): # BUG Não está a ser reconhecido por causa do ignore de espaços (possivel de concertar utilizando mais um estado)
    r'\d+ SPACES'
    len=0
    for char in t.value: # TODO Ainda n foi verificado
        if char != ' ':
            len+=1
        else:
            pass
    t.value = int(t.value[:len])                      
    return t

def t_INITIAL_functionDefState_SPACE(t): 
    r'SPACE\b'                           
    return t

def t_INITIAL_functionDefState_CR(t):
    r'CR\b'
    return t

def t_INITIAL_functionDefState_CHAR(t): # BUG Não está a ser reconhecido por causa do ignore de espaços (possivel de concertar utilizando mais um estado)
    r'. CHAR'
    t.value = t.value[-1]
    try:
        t.value = ord(t.value)
        print(t.value)
    except:
        print("ERROR converting to ASCII. POS: ",t.lexpos)

def t_INITIAL_functionDefState_EMIT(t): # BUG Não está a ser reconhecido por causa do ignore de espaços (possivel de concertar utilizando mais um estado)
    r'. EMIT'
    t.value = t.value[1]
    return t

def t_INITIAL_functionDefState_WORD(t): # CUIDADO! Terá de ficar em último (abaixo de funcoes
    r'[A-Z]+'                           #) de print, palavras restritas (EMIT,KEY,...) )
    return t

def t_INITIAL_functionDefState_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# TODO Ver se não será preciso definir funções para estes (penso que n)
t_INITIAL_functionDefState_ADD = r'\+'
t_INITIAL_functionDefState_SUB = r'-'
t_INITIAL_functionDefState_MUL = r'\*'
t_INITIAL_functionDefState_DIV = r'/'
t_INITIAL_functionDefState_MOD = r'%'

# IGNORES

# TODO: Adicionar ignores para todos os states
t_ignore = ' \t\n'
t_functionArgState_ignore = ' \t\n'
t_functionDefState_ignore = ' \t\n'
t_commentState_ignore = ''

# FIM IGNORES

def t_ANY_error(t): #Para já, ignoram-se \n, uma vez que penso que APENAS serão UTEIS para COMENTARIOS 
    print('Illegal character: ',t.value[0],' Pos: ',t.lexpos)
    t.lexer.skip(1)

# MAIN

lexer = lex.lex(debug=True)

forth = ''' SPACE 3 530+ \ boda SPACE
3 5 +
: BODA \ comentario
 ( a b -- hi \ comentario
   has ) \ comentario
 3 KEY \ comentario 
 + ;
: TOFU ." Yummy bean curd!" ;
: MENU
CR TOFU CR SPROUTS CR
;
MENU 5 SPACES'''

fort = ''''''

lexer.input(forth)

while tok := lexer.token():
    print(tok)
