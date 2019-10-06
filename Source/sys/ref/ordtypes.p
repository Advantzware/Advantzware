
DEF OUTPUT PARAM op-types AS CHAR NO-UNDO.                        
DEF OUTPUT PARAM op-dscrs AS CHAR NO-UNDO.

ASSIGN
 op-types = "O,C,N,Q,R,T,X"
 op-dscrs = "Original,Change,New,Quality/Re-work,Repeat,Transfer,Complete Re-run".
                                       
