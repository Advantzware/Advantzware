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

    RETURN LOOKUP(cpTarget, cpSource).
    
END.
