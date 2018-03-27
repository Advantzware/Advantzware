/*{sys/inc/ttRptSel.i}*/

DEF TEMP-TABLE ttRptList
    FIELD TextList AS cha
    FIELD FieldList AS cha
    .
DEF TEMP-TABLE ttRptSelected
    FIELD TextList AS cha
    FIELD FieldList AS cha
    FIELD FieldLength AS INT
    FIELD DisplayOrder AS INT
    FIELD HeadingFromLeft AS LOG
    .


/* function = getEntryNumber */
FUNCTION getEntryNumber RETURN INT
    (cpSource AS cha, cpTarget AS cha) :

    DEF VAR iCnt AS INT NO-UNDO.
    DEF VAR cOne AS cha NO-UNDO.
    DEF VAR iEntry AS INT INIT 1 NO-UNDO.
    DEF VAR cTmp AS cha NO-UNDO.
    
    DO iCnt = 1 TO length(cpSource):
       cOne = SUBSTRING(cpSource,iCnt,1).       
       IF cOne = "," OR iCnt = LENGTH(cpSource) THEN DO:
    
          IF cTmp = cpTarget THEN DO:          
             cTmp = "".
             RETURN iEntry.
          END.    
       END.
       IF cOne = "," THEN ASSIGN iEntry = iEntry + 1
                                 cTmp = "".
       ELSE cTmp = cTmp + cOne.   
    END.       

    RETURN iEntry.
END.
