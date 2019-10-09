
/*------------------------------------------------------------------------
    File        : fixGL.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Sep 10 17:07:39 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE BUFFER bNewCoA FOR account.
DEFINE BUFFER bOldCoA FOR account. 

DEF VAR cLine AS CHAR NO-UNDO.
DEF VAR ictr AS INT NO-UNDO.

DEF TEMP-TABLE ttNewCoA
    FIELD fromCompany AS CHAR 
    FIELD fromAcct    AS CHAR 
    FIELD toCompany   AS CHAR 
    FIELD toAcct      AS CHAR 
    FIELD AcctDesc    AS CHAR 
    .
    
DEF TEMP-TABLE ttNewAcct LIKE account.
DEF BUFFER bttAcct FOR ttNewAcct.    

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
INPUT FROM "c:\tmp\LYAccountConversion.csv".
    
REPEAT:
    IMPORT UNFORMATTED cLine.
    CREATE ttNewCoA.
    ASSIGN 
        ttNewCoA.fromCompany = "00" + ENTRY(1,cLine,",")
        ttNewCoA.fromAcct    = ENTRY(2,cLine,",")
        ttNewCoA.toCompany   = "00" + ENTRY(3,cLine,",")
        ttNewCoA.toAcct      = ENTRY(4,cLine,",")
        ttNewCoA.AcctDesc    = ENTRY(5,cLine,",").
END.

DISABLE TRIGGERS FOR LOAD OF account.
    
FOR EACH ttNewCoA:
    FIND FIRST bttAcct WHERE 
        bttAcct.company EQ ttNewCoA.toCompany AND 
        bttAcct.actnum EQ ttNewCoA.toAcct
        NO-LOCK NO-ERROR.
    IF AVAIL bttAcct THEN 
        NEXT.        
    FIND FIRST bOldCoA WHERE 
        bOldCoA.company EQ ttNewCoA.fromCompany AND 
        bOldCoA.actnum EQ ttNewCoA.fromAcct
    EXCLUSIVE NO-ERROR.
    IF AVAIL bOldCoA THEN 
    DO:
        CREATE ttNewAcct.
        BUFFER-COPY bOldCoA EXCEPT company actnum dscr TO ttNewAcct.
        ASSIGN 
            ttNewAcct.company = ttNewCoA.toCompany
            ttNewAcct.actnum  = ttNewCoA.toAcct
            ttNewAcct.dscr    = ttNewCoA.AcctDesc.
        DO ictr = 1 TO 10:
            IF ttNewAcct.dist-actnum[iCtr] EQ "" THEN NEXT.
            FIND FIRST bttAcct WHERE 
                bttAcct.company EQ ttNewCoA.fromCompany AND 
                bttAcct.actnum EQ ttNewCoA.fromAcct
                NO-LOCK NO-ERROR.
            IF AVAIL bttAcct THEN ASSIGN 
                ttNewAcct.dist-actnum[iCtr] = ttNewCoA.toAcct.
        END.
    END.
END.

OUTPUT TO c:\tmp\account.d.
FOR EACH ttNewAcct:
    EXPORT ttNewAcct.
END.

    