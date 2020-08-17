
/*------------------------------------------------------------------------
    File        : actRelMergTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Rahul Rawat
    Created     : Wed Jul 29 02:02:27 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{custom/globdefs.i}
{sys/inc/var.i}
{sys/inc/varasgn.i}

DEFINE INPUT PARAMETER ipriOerel AS ROWID NO-UNDO.

DEFINE VARIABLE lRellFound           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iOeRellRno           AS INTEGER   NO-UNDO.
DEFINE VARIABLE rOeRelRow            AS ROWID     NO-UNDO.
DEFINE VARIABLE rOeRellRow2          AS ROWID     NO-UNDO.
DEFINE VARIABLE rOeRelhRow2          AS ROWID     NO-UNDO.
DEFINE VARIABLE vcRelMergeCalc       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNK1RelMerge         AS CHARACTER NO-UNDO INIT ",AllOrders,SameOrderOnly,SamePo#Only,AllOrders&ShipFromWhse,SamePO#&ShipFromWhse,SameOrder&SameShipFrom,SameOrder&SameShipFrom&SamePO,AllOrders&NotRunShip".
DEFINE VARIABLE cCustNo              AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount               AS INTEGER   NO-UNDO.

DEFINE BUFFER bf-oe-relh      FOR oe-relh .
DEFINE BUFFER b2-oe-rell      FOR oe-rell.
DEFINE BUFFER b-oe-rel        FOR oe-rel.
          
&scoped-define for-each1 ~
    FOR EACH b-oe-rel NO-LOCK ~
         WHERE b-oe-rel.company  EQ oe-rel.company  ~
           AND b-oe-rel.po-no    EQ oe-rel.po-no    ~
           AND b-oe-rel.r-no     NE oe-rel.r-no     ~
           AND b-oe-rel.rel-date EQ oe-rel.rel-date ~
           AND b-oe-rel.cust-no  EQ oe-rel.cust-no
           
&SCOPED-DEFINE for-each2 ~
    EACH b2-oe-rell NO-LOCK ~
        WHERE b2-oe-rell.company EQ b-oe-rel.company ~
          AND b2-oe-rell.ord-no  EQ b-oe-rel.ord-no  ~
          AND b2-oe-rell.i-no    EQ b-oe-rel.i-no 

&scoped-define for-each3 ~
    EACH oe-relh NO-LOCK ~
        WHERE oe-relh.company  EQ cocode ~
          AND oe-relh.rel-date EQ oe-rel.rel-date ~
          AND oe-relh.cust-no  EQ cCustNo ~
          AND oe-relh.ship-id  EQ oe-rel.ship-id  ~
          AND oe-relh.posted   EQ NO ~
          AND oe-relh.deleted  EQ NO ~
          AND (oe-relh.printed EQ NO OR relmerge-log) ~
          AND oe-relh.r-no     EQ b2-oe-rell.r-no ~
          USE-INDEX r-no  ~
          BY oe-relh.upd-date DESCENDING ~
          BY oe-relh.upd-time DESCENDING               
               
&scoped-define for-each4 ~
    FIND FIRST bf-oe-ordl NO-LOCK ~
        WHERE bf-oe-ordl.company EQ b-oe-rel.company ~
          AND bf-oe-ordl.ord-no  EQ b-oe-rel.ord-no ~
          AND bf-oe-ordl.line    EQ b-oe-rel.line ~
        NO-ERROR.
        
DO TRANSACTION:
  {sys/inc/relmerge.i}
END.
          

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    FIND FIRST oe-rel WHERE ROWID(oe-rel) EQ ipriOerel NO-LOCK NO-ERROR.
    
    DEFINE STREAM s1.
    DEFINE STREAM s2.
    
    OUTPUT STREAM s1 TO "C:\Tmp\NewOeRel.txt" APPEND.
    OUTPUT STREAM s2 TO "C:\Tmp\OldOeRel.txt" APPEND.
    
    cCustNo = oe-rel.cust-no.
    DO icount = 1 TO NUM-ENTRIES(cNK1RelMerge):
        vcRelMergeCalc = ENTRY(icount,cNK1RelMerge).

        lRellFound = NO.
        
        RUN pFindMatchingOeRellNew (
            OUTPUT lRellFound,
            OUTPUT iOeRellRno, 
            OUTPUT rOeRelRow,
            OUTPUT rOeRelRow,
            OUTPUT rOeRelhRow2
            ).
        IF lRellFound THEN DO:  
            DISPLAY STREAM s1 
            iOeRellRno          FORMAT ">>>>>9" COLUMN-LABEL "RNo."
            STRING(rOeRelRow)   FORMAT "X(18)"  COLUMN-LABEL "RowID OeRell"
            STRING(rOeRelhRow2) FORMAT "X(18)"  COLUMN-LABEL "RowID OeRelh"
            vcRelMergeCalc      FORMAT "X(30)"  COLUMN-LABEL "Merge Calc"
            oe-rel.rel-no
            oe-rel.ord-no
            oe-rel.i-no
            oe-rel.line
            SKIP 
            WITH DOWN WIDTH 200 FRAME a.
            DOWN WITH FRAME a.         
                     
        END. 
         
        lRellFound = NO.
        RUN pFindMatchingOeRellOld (
            OUTPUT lRellFound,
            OUTPUT iOeRellRno, 
            OUTPUT rOeRelRow,
            OUTPUT rOeRelRow,
            OUTPUT rOeRelhRow2
            ).
        IF lRellFound THEN DO:  
            DISPLAY STREAM s2 
            iOeRellRno          FORMAT ">>>>>9" COLUMN-LABEL "RNo."
            STRING(rOeRelRow)   FORMAT "X(18)"  COLUMN-LABEL "RowID OeRell"
            STRING(rOeRelhRow2) FORMAT "X(18)"  COLUMN-LABEL "RowID OeRelh"
            vcRelMergeCalc      FORMAT "X(30)"  COLUMN-LABEL "Merge Calc"
            oe-rel.rel-no
            oe-rel.ord-no
            oe-rel.i-no
            oe-rel.line
            SKIP 
            WITH DOWN WIDTH 200 FRAME b.
            DOWN WITH FRAME b.         
                     
        END.           
    END.
OUTPUT CLOSE.

/* **********************  Internal Procedures  *********************** */


PROCEDURE pFindMatchingOeRellOld PRIVATE:
    DEFINE OUTPUT PARAMETER oplOeRellFound   AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiOeRellRno     AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeRellRow     AS ROWID   NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeRellRow2    AS ROWID   NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeRelhRow2    AS ROWID   NO-UNDO.
    
    DEFINE VARIABLE lRunShip AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE VARIABLE lRellFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iOeRellRno AS INTEGER NO-UNDO.

    FIND FIRST bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ oe-rel.company
          AND bf-oe-ordl.ord-no EQ oe-rel.ord-no
          AND bf-oe-ordl.line EQ oe-rel.line
        NO-ERROR.
    IF AVAILABLE bf-oe-ordl THEN lRunShip = bf-oe-ordl.whsed.    

    lRellFound = NO.
    REL-MATCH:
    FOR EACH b-oe-rel NO-LOCK
        WHERE b-oe-rel.company  EQ oe-rel.company
          AND b-oe-rel.po-no    EQ oe-rel.po-no
          AND b-oe-rel.r-no     NE oe-rel.r-no
          AND b-oe-rel.rel-date EQ oe-rel.rel-date
          AND b-oe-rel.cust-no  EQ oe-rel.cust-no
          AND (IF INDEX(vcRelMergeCalc, "SameOrder") GT 0 THEN 
               b-oe-rel.ord-no EQ oe-rel.ord-no ELSE TRUE)   
        ,
        EACH b2-oe-rell NO-LOCK
            WHERE b2-oe-rell.company EQ b-oe-rel.company
              AND b2-oe-rell.ord-no  EQ b-oe-rel.ord-no
              AND b2-oe-rell.i-no    EQ b-oe-rel.i-no
              AND (IF vcRelMergeCalc EQ "SameOrder&SameShipFrom&SamePO" THEN 
                   b2-oe-rell.loc EQ oe-rel.spare-char-1 ELSE TRUE)          
        ,
        EACH oe-relh NO-LOCK
            WHERE oe-relh.company  EQ oe-rel.company
              AND oe-relh.rel-date EQ oe-rel.rel-date
              AND oe-relh.cust-no  EQ cCustNo
              AND oe-relh.ship-id  EQ oe-rel.ship-id                    
              AND oe-relh.posted   EQ NO
              AND oe-relh.deleted  EQ NO
              AND (oe-relh.printed EQ NO OR relmerge-log) 
              AND oe-relh.r-no     EQ b2-oe-rell.r-no
            BY oe-relh.upd-date DESCENDING 
            BY oe-relh.upd-time DESCENDING:
        FIND FIRST bf-oe-ordl NO-LOCK
            WHERE bf-oe-ordl.company EQ b-oe-rel.company
              AND bf-oe-ordl.ord-no EQ b-oe-rel.ord-no
              AND bf-oe-ordl.line EQ b-oe-rel.line
            NO-ERROR.
     
        IF vcRelMergeCalc EQ "AllOrders&NotRunShip" AND AVAIL(bf-oe-ordl) THEN 
        DO:
            /* Criteria for AllOrders&ShipFromWhse, Run & ship can't be merged with any other */
            IF  bf-oe-ordl.whsed = TRUE OR lRunShip EQ TRUE THEN 
                NEXT REL-MATCH.                  
        END.
               
        ASSIGN 
            lRellFound    = TRUE
            iOeRellRno    = b2-oe-rell.r-no
            oprOeRellRow  = ROWID(b-oe-rel)
            oprOeRellRow2 = ROWID(b-oe-rel)
            oprOeRelhRow2 = ROWID(oe-relh)
            .

        LEAVE.
    END.

    ASSIGN
        oplOeRellFound = lRellFound
        opiOeRellRno = iOeRellRno
        .
END.    

PROCEDURE pFindMatchingOeRellNew PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOeRellFound   AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiOeRellRno     AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeRellRow     AS ROWID   NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeRellRow2    AS ROWID   NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeRelhRow2    AS ROWID   NO-UNDO.
    
    DEFINE VARIABLE lRunShip AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE VARIABLE lRellFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iOeRellRno AS INTEGER NO-UNDO.

    FIND FIRST bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ oe-rel.company
          AND bf-oe-ordl.ord-no EQ oe-rel.ord-no
          AND bf-oe-ordl.line EQ oe-rel.line
        NO-ERROR.
    IF AVAILABLE bf-oe-ordl THEN lRunShip = bf-oe-ordl.whsed.    

    lRellFound = NO.
    
     IF vcRelMergeCalc EQ "SameOrder&SameShipFrom&SamePO" THEN DO:
        REL-MATCH:
            {&for-each1}
                AND b-oe-rel.ord-no EQ oe-rel.ord-no,
            {&for-each2}
                AND b2-oe-rell.loc EQ oe-rel.spare-char-1,
            {&for-each3}:
            {&for-each4}
                   
            ASSIGN 
                lRellFound    = TRUE
                iOeRellRno    = b2-oe-rell.r-no
                oprOeRellRow  = ROWID(b-oe-rel)
                oprOeRellRow2 = ROWID(b-oe-rel)
                oprOeRelhRow2 = ROWID(oe-relh)
                .
    
            LEAVE.
        END.
    END.
    ELSE IF vcRelMergeCalc NE "SameOrder&SameShipFrom&SamePO" AND 
         INDEX(vcRelMergeCalc, "SameOrder") GT 0 THEN DO:  
              
        REL-MATCH1:
            {&for-each1}
                AND b-oe-rel.ord-no EQ oe-rel.ord-no,
            {&for-each2},
            {&for-each3}:
            {&for-each4}
           
            ASSIGN 
                lRellFound    = TRUE
                iOeRellRno    = b2-oe-rell.r-no
                oprOeRellRow  = ROWID(b-oe-rel)
                oprOeRellRow2 = ROWID(b-oe-rel)
                oprOeRelhRow2 = ROWID(oe-relh)
                .
    
            LEAVE.
        END.    
    END.
    ELSE DO:
       REL-MATCH2:
           {&for-each1},
           {&for-each2},        
           {&for-each3}:
            {&for-each4}
         
            IF vcRelMergeCalc EQ "AllOrders&NotRunShip" AND AVAIL(bf-oe-ordl) THEN 
            DO:
                /* Criteria for AllOrders&ShipFromWhse, Run & ship can't be merged with any other */
                IF  bf-oe-ordl.whsed = TRUE OR lRunShip EQ TRUE THEN 
                    NEXT REL-MATCH2.                  
            END.
                   
            ASSIGN 
                lRellFound    = TRUE
                iOeRellRno    = b2-oe-rell.r-no
                oprOeRellRow  = ROWID(b-oe-rel)
                oprOeRellRow2 = ROWID(b-oe-rel)
                oprOeRelhRow2 = ROWID(oe-relh)
                .
    
            LEAVE.
        END.        
    END.    
 
    ASSIGN
        oplOeRellFound = lRellFound
        opiOeRellRno   = iOeRellRno
        .
END PROCEDURE.


        
        