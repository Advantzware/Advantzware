
DEF OUTPUT PARAM op-trans-code  AS CHAR NO-UNDO.                        
DEF OUTPUT PARAM op-trans-dscrs AS CHAR NO-UNDO.

ASSIGN
 op-trans-code  = "A,C,E,I,R,S,T".
 op-trans-dscrs = "Adjustment,Physical Count,Returns,Issue,Receipt,Shipment,Transfer".
                                       
