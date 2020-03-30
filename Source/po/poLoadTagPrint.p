/*------------------------------------------------------------------------
    File        : po/poLoadTagprint.p
    Purpose     : print po loadtag

    Syntax      :

    Description : 

    Author(s)   : Sewa singh
    Created     : mon 30 march EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipiPoNo         AS INTEGER NO-UNDO.   
DEFINE INPUT PARAMETER ipcPrintFormat  AS CHARACTER NO-UNDO.  

{sys/inc/var.i shared}

DEFINE VARIABLE ls-full-img1 AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE cRtnChar     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound    AS LOG       NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "LoadTagXprintImage",
    INPUT "C",
    INPUT NO,
    INPUT NO,
    INPUT "",
    INPUT "",
    OUTPUT cRtnChar,
    OUTPUT lRecFound).

FILE-INFO:FILE-NAME = cRtnChar.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".   


FOR EACH po-ordl NO-LOCK
    WHERE po-ordl.company EQ cocode 
    AND po-ordl.po-no EQ ipiPoNo BY po-ordl.i-no:
       
    FIND FIRST oe-ord NO-LOCK 
        WHERE oe-ord.company = cocode
        AND oe-ord.ord-no = po-ordl.ord-no  NO-ERROR.

    FIND FIRST  oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ cocode
        AND oe-ordl.ord-no  EQ po-ordl.ord-no
        AND oe-ordl.i-no EQ po-ordl.i-no  NO-ERROR.      
     
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ po-ordl.cust-no NO-ERROR .
     
    IF ipcPrintFormat EQ "POLoadtag1" THEN 
    DO:
        {po/poLoadtagNoLogo.i}
    END.
    ELSE IF ipcPrintFormat EQ "POLoadtag2" THEN 
        DO:
            {po/poLoadtagLogo.i } 
        END.      
     
END.




