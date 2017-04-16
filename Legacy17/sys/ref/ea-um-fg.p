
DEF INPUT  PARAM ip-uom LIKE uom.uom NO-UNDO.
DEF OUTPUT PARAM op-is-it-an-ea-uom AS LOG NO-UNDO.


op-is-it-an-ea-uom = LOOKUP(ip-uom,"C,CS,L,M") LE 0.
                                       
