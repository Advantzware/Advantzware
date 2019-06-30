

/*------------------------------------------------------------------------
    File        : dsEntryIOrd.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order Entry Maintenance

    Author(s)   : Sewa Singh
    Created     : Sat August 25 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttEntryIOrd NO-UNDO
    BEFORE-TABLE beforeOrder
    
    
    FIELD vcustName       AS CHAR FORMAT "X(30)"
    FIELD Prod            AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD onhandqty       AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD actrelqty       AS INTEGER FORMAT "->>,>>>,>>>"
    FIELD wipqty          AS INTEGER FORMAT "->>,>>>,>>>"
    FIELD oupct           AS INTEGER FORMAT "->>>>>%"
    FIELD vOrder          AS INTEGER FORMAT ">>>>>9"  
    FIELD vEst            AS CHAR FORMAT "X(5)"      
    FIELD VJob            AS CHAR FORMAT "X(6)"      
    FIELD VJob2           AS INTEGER FORMAT ">9"     
   
    FIELD vCust           LIKE oe-ord.cust-no
   
    FIELD vReqDate        AS DATE
    FIELD vPrice          AS DECIMAL FORMAT ">>,>>>,>>9.99<<<<"
    FIELD vUom            AS CHAR FORMAT "x(4)"
    FIELD vExtPrice       AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD vItem           AS CHAR FORMAT "x(15)"
    FIELD vItemName       AS CHAR FORMAT "x(30)"
    FIELD vPart           AS CHAR FORMAT "x(15)"
    FIELD vPoNum          AS CHAR FORMAT "x(15)"
    FIELD vOrdDate        AS DATE
    FIELD vStatus         AS CHAR FORMAT "x(1)"
    FIELD vOrdqty         AS DECIMAL FORMAT "->>,>>>,>>9.9<<"
    FIELD vqno AS INT 
    FIELD vinvqty         AS DECIMAL FORMAT "->>>,>>9.99"
    FIELD vshipqty        AS DECIMAL FORMAT "->>>,>>9.99"

    FIELD vReqcode       AS CHAR
    FIELD vPromCode       AS CHAR
    FIELD vPromDate       AS DATE
    FIELD vOver           AS DECIMAL
    FIELD vUnder          AS DECIMAL
    FIELD vsman1          AS CHAR
    FIELD vsamn2          AS CHAR
    FIELD vsman3          AS CHAR
    FIELD vspct1          AS DEC
    FIELD vspct2          AS DEC
    FIELD vspct3          AS DEC
    FIELD vscomm          AS DEC
    FIELD vscomm2         AS DEC
    FIELD vscomm3         AS DEC
    FIELD sname1          AS CHAR
    FIELD sname2          AS CHAR
    FIELD sname3          AS CHAR
    FIELD vtype AS CHAR
    FIELD disc AS DECIMAL
    FIELD tax AS CHAR
    
    FIELD vqno2 AS INT
    FIELD vRecKey AS CHAR
     FIELD VUserid         AS CHAR
     FIELD vCustDate       AS DATE
     FIELD LINE AS INTEGER
    FIELD shipqty         AS DECIMAL FORMAT "->>,>>>,>>9.99" 
    FIELD invqty          AS DECIMAL FORMAT "->>,>>>,>>9.99"  
    FIELD company AS CHAR
    FIELD vCustpo         AS CHARACTER FORMAT "X(15)"
    

    .
        
DEFINE DATASET dsEntryIOrd FOR ttEntryIOrd.
DEFINE QUERY q-OrderEntryQuery FOR ttEntryIOrd.
DEFINE DATA-SOURCE src-orderentry  FOR QUERY q-OrderEntryQuery.
BUFFER ttEntryIOrd :ATTACH-DATA-SOURCE(DATA-SOURCE src-orderentry  :HANDLE).


DEF VAR li-prod AS INT NO-UNDO.
DEF VAR li-bal AS INT NO-UNDO.
DEF VAR li-wip AS INT NO-UNDO.
DEF VAR li-pct AS INT NO-UNDO.
DEF VAR li-qoh AS INT NO-UNDO.
DEF VAR li-act-rel-qty AS INT NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.


FUNCTION CalcQuoteDate RETURNS DATE:
    DEFINE VAR OutQuoteDate AS DATE.
    
    ASSIGN outQuoteDate = (TODAY - 365).
    RETURN OutQuoteDate.   /* Function return value. */
END FUNCTION.


FUNCTION GetCurrentUser RETURNS CHARACTER (wuser AS CHARACTER ):
    
    return "".       
END FUNCTION.

FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

FUNCTION CanAccess RETURNS LOGICAL ( INPUT prmUser AS CHARACTER, INPUT ModNum AS CHARACTER ) :
   
    RETURN TRUE.
END FUNCTION.

/*****************************************************************************//*mod 1 */
FUNCTION get-prod RETURNS INTEGER
  (OUTPUT op-bal AS INT) :
   
   DEF VAR li AS INT NO-UNDO.
  IF AVAIL oe-ordl THEN
  DO:
     IF oe-ordl.job-no NE "" THEN
        FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
           WHERE fg-rcpth.company   EQ cocode
             AND fg-rcpth.job-no    EQ oe-ordl.job-no
             AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
             AND fg-rcpth.i-no      EQ oe-ordl.i-no
             AND fg-rcpth.rita-code EQ "R"
           USE-INDEX job,
           EACH fg-rdtlh FIELDS(qty) NO-LOCK
           WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
             AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
             li = li + fg-rdtlh.qty.
        END.
     ELSE
     DO:
        FOR EACH job-hdr FIELDS(job-no job-no2) WHERE
            job-hdr.company EQ cocode AND
            job-hdr.ord-no EQ oe-ordl.ord-no AND
            job-hdr.i-no EQ oe-ordl.i-no
            USE-INDEX ord-no
            NO-LOCK,
            EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
           WHERE fg-rcpth.company   EQ cocode

             AND fg-rcpth.job-no    EQ job-hdr.job-no
             AND fg-rcpth.job-no2   EQ job-hdr.job-no2
             AND fg-rcpth.i-no      EQ oe-ordl.i-no
             AND fg-rcpth.rita-code EQ "R"
           USE-INDEX job,
           EACH fg-rdtlh FIELDS(qty) NO-LOCK
           WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
             AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
             li = li + fg-rdtlh.qty.
        END.
     END.
  END.

  op-bal = li.
 
  RETURN li.   /* Function return value. */

END FUNCTION.

FUNCTION get-bal RETURNS INTEGER
  (OUTPUT op-qoh AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

  IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
  FOR EACH fg-bin NO-LOCK
      WHERE fg-bin.company EQ oe-ordl.company
      AND fg-bin.job-no  EQ oe-ordl.job-no
      AND fg-bin.job-no2 EQ oe-ordl.job-no2 
       AND fg-bin.i-no    EQ oe-ordl.i-no :
    li = li + fg-bin.qty.
  END.
  op-qoh = li.
  RETURN li.    /* Function return value. */

END FUNCTION.
FUNCTION get-act-rel-qty RETURNS INTEGER
    () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  IF AVAIL oe-ordl THEN
      FOR EACH oe-rel WHERE 
         oe-rel.company EQ cocode AND
         oe-rel.ord-no  EQ oe-ordl.ord-no AND
         oe-rel.i-no    EQ oe-ordl.i-no AND
         oe-rel.line    EQ oe-ordl.line
         NO-LOCK:

         RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat ).

         IF INDEX("W,B,P",lv-stat) > 0 THEN
            li = li + oe-rel.qty.
      END.
     
  RETURN li.


END FUNCTION.
/**********************************************************/
FUNCTION get-wip RETURNS INTEGER() :
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.
  DEF BUFFER b-oe-ordl FOR oe-ordl.           
  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK NO-ERROR.
  rtnValue = oe-ordl.qty - (li-qoh + oe-ordl.ship-qty).
  IF rtnValue LT 0 OR
     rtnValue LT oe-ordl.qty * b-oe-ordl.under-pct / 100 THEN
  rtnValue = 0.
  RETURN rtnValue.

END FUNCTION.
 /***********************************************************/
FUNCTION get-pct RETURNS INTEGER(ipBal AS INTEGER) :
    
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.
    IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:
        rtnValue = ((ipBal / oe-ordl.qty) - 1) * 100 .
        IF rtnValue EQ 0 THEN rtnValue = 100.
        IF rtnValue EQ -100 THEN rtnValue = 0.
        END.
  RETURN rtnValue.
END FUNCTION.







