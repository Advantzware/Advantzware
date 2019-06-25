
/*------------------------------------------------------------------------
    File        : order_enq.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order Enquiry Maintenance

    Author(s)   : Sewa Singh
    Created     : Sat August 25 2007
    Notes       :
    Mod         : 1. produced qty without job-no  25-08-2008
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrder NO-UNDO /*LIKE oe-ordl */
BEFORE-TABLE beforeOrder
    FIELD ord-no     LIKE oe-ordl.ord-no
    FIELD LINE       LIKE oe-ordl.LINE
    FIELD rec_key    LIKE oe-ordl.rec_key
    FIELD cust-no    LIKE oe-ordl.cust-no
    FIELD req-date   LIKE oe-ordl.req-date
    FIELD price      LIKE oe-ordl.price
    FIELD pr-uom     LIKE oe-ordl.pr-uom
    FIELD t-price    LIKE oe-ordl.t-price
    FIELD i-no       LIKE oe-ordl.i-no
    FIELD i-name     LIKE oe-ordl.i-name
    FIELD part-no    LIKE oe-ordl.part-no
    FIELD po-no      LIKE oe-ordl.po-no
    FIELD q-no       LIKE oe-ordl.q-no
    FIELD stat       LIKE oe-ordl.stat
    FIELD qty        LIKE oe-ordl.qty
    FIELD req-code   LIKE oe-ordl.req-code
    FIELD prom-code  LIKE oe-ordl.prom-code 
    FIELD prom-date  LIKE oe-ordl.prom-date
    FIELD over-pct   LIKE oe-ordl.over-pct
    FIELD under-pct  LIKE oe-ordl.under-pct
    FIELD s-man      AS CHAR EXTENT 3
    FIELD s-pct      AS DECIMAL EXTENT 3
    FIELD s-comm     AS DECIMAL EXTENT 3
    FIELD type-code  LIKE oe-ordl.type-code
    FIELD tax        LIKE oe-ordl.tax 



    FIELD due-date LIKE oe-ord.due-date
    FIELD ord-date LIKE oe-ord.ord-date
    FIELD cust-name       AS CHAR FORMAT "X(30)"
    FIELD Prod            AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD onhandqty       AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD actrelqty       AS INTEGER FORMAT "->>,>>>,>>>"
    FIELD wipqty          AS INTEGER FORMAT "->>,>>>,>>>"
    FIELD oupct           AS INTEGER FORMAT "->>>>>%"
    FIELD vCustpo         AS CHARACTER FORMAT "X(15)"
    FIELD vinvqty         AS DECIMAL FORMAT "->>>,>>9.99"
    FIELD vshipqty        AS DECIMAL FORMAT "->>>,>>9.99"
    FIELD sname1          AS CHAR
    FIELD sname2          AS CHAR
    FIELD sname3          AS CHAR
    FIELD vEst            AS CHAR FORMAT "X(7)"      
    FIELD VJob            AS CHAR FORMAT "X(8)"      
    FIELD VJob2           AS INTEGER FORMAT ">9"
    .
        
DEFINE DATASET dsOrder FOR ttOrder.

DEFINE VARIABLE q-OrderQuery AS HANDLE.
DEFINE VARIABLE src-Order AS HANDLE.

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

/***********************************************************/

FUNCTION get-price-disc RETURNS DECIMAL
  ( /* parameter-definitions */ ) :

  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR ld AS DEC NO-UNDO.

  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

  ld = b-oe-ordl.price * (1 - (b-oe-ordl.disc / 100)).

  FOR EACH ar-invl FIELDS(inv-no unit-pr disc) WHERE
      ar-invl.company EQ cocode AND
      ar-invl.ord-no EQ b-oe-ordl.ord-no AND
      ar-invl.i-no EQ b-oe-ordl.i-no
      NO-LOCK
      BY ar-invl.inv-no DESC:

      ld = ar-invl.unit-pr * (1 - (ar-invl.disc / 100)).
      LEAVE.
  END.

  RETURN ld.

END FUNCTION.

/*********************************************/
