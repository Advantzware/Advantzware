/*util/cvtsmtx.p Convert Sman Matrix to new sturcture*/

DEF VAR i AS INT NO-UNDO.

MESSAGE "Are you ready to convert Sales Rep Matrix to new structure" VIEW-AS ALERT-BOX
    QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.

IF ll-ans THEN
FOR EACH sman-mtx.
    DISP company sman custype custype-no.
    PAUSE 0.
    DO i = 1 TO 10:
       FIND FIRST smanmtrx WHERE smanmtrx.company = sman-mtx.company
                          AND smanmtrx.sman = sman-mtx.sman
                          AND smanmtrx.custype = sman-mtx.custype
                          AND smanmtrx.procat = sman-mtx.procat[i]
                          NO-LOCK NO-ERROR.
       IF NOT AVAIL smanmtrx THEN DO:
          CREATE smanmtrx.
          ASSIGN smanmtrx.company = sman-mtx.company
                 smanmtrx.sman = sman-mtx.sman
                 smanmtrx.custype = sman-mtx.custype
                 smanmtrx.procat = sman-mtx.procat[i]
                 smanmtrx.commbasis = sman-mtx.commbasis[i]
                 smanmtrx.comm = sman-mtx.comm[i]
                 smanmtrx.type-comm = sman-mtx.type-comm
                 .
       END.
    END.
END.
HIDE  ALL NO-PAUSE.
MESSAGE "Completed. " VIEW-AS ALERT-BOX.

