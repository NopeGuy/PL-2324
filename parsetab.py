
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftWORDleftFUNCTIONENDrightCHARleftSPACESEMITleftEQUALSDIFFGREQUALLESEQUALLESSERGREATERleftADDSUBMULDIVMODleftTHENrightELSErightIFADD ARGSEP CHAR COMMENT COMMENT2 CR DIFF DIV ELSE EMIT EQUALS FUNCTIONEND FUNCTIONSTART FUNIN FUNOUT GREATER GREQUAL IF KEY LESEQUAL LESSER LPAREN MOD MUL NUMBER POPPRINT PRINTDELIM RPAREN SPACE SPACES SUB SWAP THEN WORDexps : exps expexps : emptyexp : WORDfunStarted : FUNCTIONSTART WORDfunctionBody : exps FUNCTIONENDexp : funStarted functionBodyexp : ADD\n           | SUB\n           | MUL\n           | DIV\n           | MODexp : LESSER\n            | GREATER\n            | DIFF\n            | GREQUAL\n            | LESEQUAL\n            | EQUALSexp : IF exps THENexp : IF exps ELSE exps THENexp : CHAR WORDexp : PRINTDELIM\n           | CR\n           | SPACE\n           | KEYexp : SPACES \n           | EMITnum : NUMBERexp : numexp : POPPRINTexp : SWAPempty :'
    
_lr_action_items = {'WORD':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[-31,4,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,33,-21,-22,-23,-24,-25,-26,-28,-29,-30,34,-27,-6,4,4,-20,-4,-5,-18,-31,4,-19,]),'ADD':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,6,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,6,6,-20,-4,-5,-18,-31,6,-19,]),'SUB':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,7,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,7,7,-20,-4,-5,-18,-31,7,-19,]),'MUL':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,8,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,8,8,-20,-4,-5,-18,-31,8,-19,]),'DIV':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,9,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,9,9,-20,-4,-5,-18,-31,9,-19,]),'MOD':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,10,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,10,10,-20,-4,-5,-18,-31,10,-19,]),'LESSER':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,11,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,11,11,-20,-4,-5,-18,-31,11,-19,]),'GREATER':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,12,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,12,12,-20,-4,-5,-18,-31,12,-19,]),'DIFF':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,13,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,13,13,-20,-4,-5,-18,-31,13,-19,]),'GREQUAL':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,14,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,14,14,-20,-4,-5,-18,-31,14,-19,]),'LESEQUAL':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,15,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,15,15,-20,-4,-5,-18,-31,15,-19,]),'EQUALS':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,16,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,16,16,-20,-4,-5,-18,-31,16,-19,]),'IF':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,17,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,17,17,-20,-4,-5,-18,-31,17,-19,]),'CHAR':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,18,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,18,18,-20,-4,-5,-18,-31,18,-19,]),'PRINTDELIM':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,19,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,19,19,-20,-4,-5,-18,-31,19,-19,]),'CR':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,20,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,20,20,-20,-4,-5,-18,-31,20,-19,]),'SPACE':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,21,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,21,21,-20,-4,-5,-18,-31,21,-19,]),'KEY':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,22,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,22,22,-20,-4,-5,-18,-31,22,-19,]),'SPACES':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,23,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,23,23,-20,-4,-5,-18,-31,23,-19,]),'EMIT':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,24,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,24,24,-20,-4,-5,-18,-31,24,-19,]),'POPPRINT':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,26,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,26,26,-20,-4,-5,-18,-31,26,-19,]),'SWAP':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,27,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,27,27,-20,-4,-5,-18,-31,27,-19,]),'FUNCTIONSTART':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,28,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,28,28,-20,-4,-5,-18,-31,28,-19,]),'NUMBER':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,],[-31,29,-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,29,29,-20,-4,-5,-18,-31,29,-19,]),'$end':([0,1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,19,20,21,22,23,24,25,26,27,29,30,33,35,36,39,],[-31,0,-2,-1,-3,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,-20,-5,-18,-19,]),'FUNCTIONEND':([2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,19,20,21,22,23,24,25,26,27,29,30,31,33,34,35,36,39,],[-2,-1,-3,-31,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,35,-20,-4,-5,-18,-19,]),'THEN':([2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,32,33,35,36,37,38,39,],[-2,-1,-3,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,36,-20,-5,-18,-31,39,-19,]),'ELSE':([2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,29,30,32,33,35,36,39,],[-2,-1,-3,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31,-21,-22,-23,-24,-25,-26,-28,-29,-30,-27,-6,37,-20,-5,-18,-19,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'exps':([0,5,17,37,],[1,31,32,38,]),'empty':([0,5,17,37,],[2,2,2,2,]),'exp':([1,31,32,38,],[3,3,3,3,]),'funStarted':([1,31,32,38,],[5,5,5,5,]),'num':([1,31,32,38,],[25,25,25,25,]),'functionBody':([5,],[30,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> exps","S'",1,None,None,None),
  ('exps -> exps exp','exps',2,'p_exps','Relatorio_PL_Grupo35.py',220),
  ('exps -> empty','exps',1,'p_exps_empty','Relatorio_PL_Grupo35.py',224),
  ('exp -> WORD','exp',1,'p_exp_word','Relatorio_PL_Grupo35.py',229),
  ('funStarted -> FUNCTIONSTART WORD','funStarted',2,'p_funStarted_WORD','Relatorio_PL_Grupo35.py',247),
  ('functionBody -> exps FUNCTIONEND','functionBody',2,'p_functionBody','Relatorio_PL_Grupo35.py',259),
  ('exp -> funStarted functionBody','exp',2,'p_exp_funDefined','Relatorio_PL_Grupo35.py',274),
  ('exp -> ADD','exp',1,'p_exp_aritOnly','Relatorio_PL_Grupo35.py',279),
  ('exp -> SUB','exp',1,'p_exp_aritOnly','Relatorio_PL_Grupo35.py',280),
  ('exp -> MUL','exp',1,'p_exp_aritOnly','Relatorio_PL_Grupo35.py',281),
  ('exp -> DIV','exp',1,'p_exp_aritOnly','Relatorio_PL_Grupo35.py',282),
  ('exp -> MOD','exp',1,'p_exp_aritOnly','Relatorio_PL_Grupo35.py',283),
  ('exp -> LESSER','exp',1,'p_exp_relOpOnly','Relatorio_PL_Grupo35.py',304),
  ('exp -> GREATER','exp',1,'p_exp_relOpOnly','Relatorio_PL_Grupo35.py',305),
  ('exp -> DIFF','exp',1,'p_exp_relOpOnly','Relatorio_PL_Grupo35.py',306),
  ('exp -> GREQUAL','exp',1,'p_exp_relOpOnly','Relatorio_PL_Grupo35.py',307),
  ('exp -> LESEQUAL','exp',1,'p_exp_relOpOnly','Relatorio_PL_Grupo35.py',308),
  ('exp -> EQUALS','exp',1,'p_exp_relOpOnly','Relatorio_PL_Grupo35.py',309),
  ('exp -> IF exps THEN','exp',3,'p_exp_ifThen','Relatorio_PL_Grupo35.py',333),
  ('exp -> IF exps ELSE exps THEN','exp',5,'p_exp_ifElseThenOnly','Relatorio_PL_Grupo35.py',349),
  ('exp -> CHAR WORD','exp',2,'p_exp_charOnly','Relatorio_PL_Grupo35.py',374),
  ('exp -> PRINTDELIM','exp',1,'p_exp_printOnly','Relatorio_PL_Grupo35.py',390),
  ('exp -> CR','exp',1,'p_exp_printOnly','Relatorio_PL_Grupo35.py',391),
  ('exp -> SPACE','exp',1,'p_exp_printOnly','Relatorio_PL_Grupo35.py',392),
  ('exp -> KEY','exp',1,'p_exp_printOnly','Relatorio_PL_Grupo35.py',393),
  ('exp -> SPACES','exp',1,'p_numExp_prints','Relatorio_PL_Grupo35.py',411),
  ('exp -> EMIT','exp',1,'p_numExp_prints','Relatorio_PL_Grupo35.py',412),
  ('num -> NUMBER','num',1,'p_num_number','Relatorio_PL_Grupo35.py',440),
  ('exp -> num','exp',1,'p_numExp_nums','Relatorio_PL_Grupo35.py',448),
  ('exp -> POPPRINT','exp',1,'p_exp_pop','Relatorio_PL_Grupo35.py',452),
  ('exp -> SWAP','exp',1,'p_exp_swapOnly','Relatorio_PL_Grupo35.py',464),
  ('empty -> <empty>','empty',0,'p_empty','Relatorio_PL_Grupo35.py',468),
]
