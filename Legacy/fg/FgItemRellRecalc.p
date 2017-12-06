
/*------------------------------------------------------------------------
    File        : FgItemRellRecalc.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Nov 14 17:20:14 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcLogFile AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplUpdate AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i &new=NEW}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i NEW SHARED}
{custom/globdefs.i &NEW=NEW}
        

DEF VAR cItem          AS CHAR FORMAT "x(20)".
DEF VAR cLoc           AS CHAR FORMAT "x(4)".
DEFINE VARIABLE cTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDate AS CHARACTER NO-UNDO.
DEF VAR iDispAlloc     AS INT  FORMAT "->>>,>>>,>>9".
DEF VAR iDispBack      AS INT  FORMAT "->>>,>>>,>>9".
DEF VAR iDispAllocCalc AS INT  FORMAT "->>>,>>>,>>9".
DEF VAR iDispBackCalc  AS INT  FORMAT "->>>,>>>,>>9".
DEF VAR iq-alloc       AS INT  NO-UNDO.
DEF VAR iq-back        AS INT  NO-UNDO.
DEF VAR iLoc-q-back    AS INT  NO-UNDO.
DEF VAR iLoc-q-alloc   AS INT  NO-UNDO.
DEFINE BUFFER bf-itemfg FOR itemfg.
DEFINE BUFFER bf-itemfg-loc FOR itemfg-loc.

FORM
    cDate
    cTime
    cItem  
    cLoc
    iDispAlloc
    iDispBack 
    iDispAllocCalc
    iDispBackCalc 
    WITH FRAME f-exceptions 20 DOWN.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/* ipcLogfile = "c:\tmp\inventory-exception.txt". */
IF ipcLogFile GT "" THEN 
  OUTPUT to VALUE(ipcLogfile) APPEND.

FOR EACH company NO-LOCK:
    ASSIGN 
        cocode = company.company
        locode = "Main"
        .
    ASSIGN 
        g_company = company.company 
        g_loc     = locode
        .
    {sys/inc/oereordr.i}        
    FIND FIRST oe-ord WHERE oe-ord.company EQ company.company
        AND oe-ord.ord-date GE TODAY - 90 
        NO-LOCK NO-ERROR.
    IF NOT AVAIL oe-ord THEN 
        NEXT.


    FOR EACH oe-rell NO-LOCK WHERE oe-rell.company EQ oe-ord.company
        AND oe-rell.ord-no GE oe-ord.ord-no 
        AND oe-rell.upd-date EQ TODAY 
        BREAK BY oe-rell.i-no:
            
        IF FIRST-OF(oe-rell.i-no) THEN 
        DO:
            FIND FIRST itemfg EXCLUSIVE-LOCK
                WHERE itemfg.company EQ oe-rell.company
                AND itemfg.i-no    EQ oe-rell.i-no
                NO-ERROR.
            IF NOT AVAIL itemfg THEN 
                NEXT.
      
    
            RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT iq-alloc,
                OUTPUT iq-back).
            IF itemfg.q-alloc NE iq-alloc OR itemfg.q-back NE iq-back THEN 
            DO:
                IF ipcLogFile GT "" THEN DO:
                    DISP 
                        STRING(TODAY) @ cDate 
                        STRING(time, "HH:MM:SS") @ cTime
                        itemfg.i-no      @ cItem  
                        ""               @ cLoc
                        itemfg.q-alloc   @ iDispAlloc 
                        itemfg.q-back    @ iDispBack
                        iq-alloc         @ iDispAllocCalc
                        iq-back          @ iDispBackCalc 
                        WITH FRAME f-exceptions WIDTH 200 STREAM-IO.
                    DOWN WITH FRAME f-exceptions.
                END.
                IF iplUpdate THEN DO:
                    FIND FIRST bf-itemfg EXCLUSIVE-LOCK 
                      WHERE ROWID(bf-itemfg) EQ ROWID(itemfg) 
                      NO-ERROR.
                    IF AVAILABLE bf-itemfg THEN DO:
                        ASSIGN bf-itemfg.q-alloc = iq-alloc
                               bf-itemfg.q-back  = iq-back
                               bf-itemfg.q-avail = bf-itemfg.q-onh +
                                 (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE bf-itemfg.q-ono) -
                                 bf-itemfg.q-alloc.
                    END.
                END. /* doing update */
                
            END. /* Quantities don't match */
    
            FOR EACH itemfg-loc NO-LOCK WHERE itemfg-loc.company EQ itemfg.company
                AND itemfg-loc.i-no    EQ itemfg.i-no
                :
                RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT iLoc-q-alloc,
                    OUTPUT iLoc-q-back).
                IF itemfg-loc.q-back NE iLoc-q-back OR itemfg-loc.q-alloc NE iLoc-q-alloc THEN 
                DO:
                    IF ipcLogFile GT "" THEN DO:
                        DISP 
                            STRING(TODAY) @ cDate 
                            STRING(time, "HH:MM:SS") @ cTime
                            itemfg-loc.i-no      @ cItem  
                            itemfg-loc.loc       @ cLoc
                            itemfg-loc.q-alloc   @ iDispAlloc 
                            itemfg-loc.q-back    @ iDispBack
                            iLoc-q-alloc         @ iDispAllocCalc
                            iLoc-q-back          @ iDispBackCalc 
                            WITH FRAME f-exceptions.
                        DOWN WITH FRAME f-exceptions.
                    END.
                    IF iplUpdate THEN DO:
                        FIND FIRST bf-itemfg-loc EXCLUSIVE-LOCK
                          WHERE ROWID(bf-itemfg-loc) EQ ROWID(itemfg-loc)
                          NO-ERROR.
                        IF AVAILABLE bf-itemfg-loc THEN DO:
                            ASSIGN bf-itemfg-loc.q-alloc = iLoc-q-alloc
                                   bf-itemfg-loc.q-back  = iLoc-q-back
                                   bf-itemfg-loc.q-avail = bf-itemfg-loc.q-onh +
                                              (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE bf-itemfg-loc.q-ono) -
                                              bf-itemfg-loc.q-alloc.
                        END.
                    END. /* If updating */
                END. /* Quantities don't match */                
            END. /* Each itemfg-loc */
        END. /* First-of i-no */
    END. /* each oe-rell */
END. /* each company */

