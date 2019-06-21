

/*------------------------------------------------------------------------
    File        :JobVariance.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Job Variance

    Author(s)   : Jyoti Bajaj
    Created     : Jan 28, 2008
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE VARIABLE custPart AS CHARACTER NO-UNDO.
    DEFINE VARIABLE orderQty AS INTEGER NO-UNDO.
    DEFINE VARIABLE producedQty AS INTEGER NO-UNDO.
    DEFINE VARIABLE onHandQty AS INTEGER NO-UNDO.
    DEFINE VARIABLE qtyOnHand AS INTEGER NO-UNDO.
    DEFINE VARIABLE shipQty AS INTEGER NO-UNDO.
    DEFINE VARIABLE invoiceQty AS INTEGER NO-UNDO.
   
    DEFINE VARIABLE overUnderPct AS INTEGER NO-UNDO.
    DEFINE VARIABLE fgItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE opBalance AS INTEGER NO-UNDO.
    

DEFINE TEMP-TABLE ttJobVariance NO-UNDO 
    BEFORE-TABLE beforeJobVariance
   
    FIELD job-no like job-hdr.job-no
    FIELD job-no2 like job-hdr.job-no2
    FIELD i-no like job-hdr.i-no
    FIELD est-no AS CHAR FORMAT "x(8)"
    FIELD ord-no AS INTEGER FORMAT ">>>>>9"
    FIELD cust-no AS CHAR FORMAT "x(8)"
    FIELD StartDt AS DATE FORMAT "99/99/9999"
    FIELD CloseDt AS DATE FORMAT "99/99/9999"
    FIELD Statu   AS CHAR FORMAT "x(1)"
    FIELD Part    AS CHAR FORMAT "x(15)"
    FIELD JobQty  AS DECIMAL FORMAT ">,>>>,>>9"
    FIELD OrdQty  AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD ProdQty AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD OnHand  AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD QtyHand AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD ShipQty AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD InvQty  AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD WipQty  AS DECIMAL FORMAT "->>,>>>,>>>"
    FIELD OUPct   AS DECIMAL FORMAT "->>>%"
    FIELD FgItem  AS CHAR FORMAT "x(15)"
    . 
DEFINE DATASET dsJobVariance FOR ttJobVariance .

DEFINE QUERY q-JobVarianceQuery FOR ttJobVariance.

DEFINE DATA-SOURCE src-JobVariance  FOR QUERY q-JobVarianceQuery.

BUFFER ttJobVariance :ATTACH-DATA-SOURCE(DATA-SOURCE src-JobVariance  :HANDLE).


/***********************************************************************************************************/

FUNCTION custPart RETURNS CHARACTER():
  IF AVAIL job-hdr THEN
  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ job-hdr.i-no
      NO-ERROR.

  RETURN IF AVAIL itemfg THEN itemfg.part-no ELSE "".

END FUNCTION.
/***********************************************************************************************************/

FUNCTION orderQty RETURNS INTEGER():
  
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN
    rtnValue = oe-ordl.qty.
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.
/***********************************************************************************************************/
FUNCTION shipQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.
  DEF VAR li-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
  DEF VAR li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN DO:
      RUN oe/ordlsqty.p (ROWID(oe-ordl),
                         OUTPUT li-inv-qty, OUTPUT li-ship-qty).
      
      rtnValue = li-ship-qty.
    END.
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.

/***********************************************************************************************************/

FUNCTION wipQty RETURNS INTEGER():
  
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN DO:
      FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
      rtnValue = oe-ordl.qty - (qtyOnHand + oe-ordl.ship-qty).
      IF rtnValue LT 0 OR
         rtnValue LT oe-ordl.qty *
                     (IF AVAIL oe-ord THEN oe-ordl.under-pct ELSE 100) / 100 THEN
        rtnValue = 0.
    END. /* avail oe-ordl */
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.

/***********************************************************************************************************/
FUNCTION invoiceQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :

  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN
    rtnValue = oe-ordl.inv-qty.
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.
/*****************************************************************************************************************/
FUNCTION overUnderPct RETURNS INTEGER
  (ipBalance AS INTEGER) :

  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:
    rtnValue = ((ipBalance / oe-ordl.qty) - 1) * 100.
    IF rtnValue EQ 0 THEN rtnValue = 100.
    IF rtnValue EQ -100 THEN rtnValue = 0.
    END. /* avail oe-ordl */
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.


/******************************************************************************************************************************/    
FUNCTION producedQty RETURNS INTEGER
  (OUTPUT opBalance AS INTEGER) :

  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl AND oe-ordl.job-no NE '' THEN
    FOR EACH fg-rcpth NO-LOCK
        WHERE fg-rcpth.company EQ oe-ordl.company
          AND fg-rcpth.job-no EQ oe-ordl.job-no
          AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
          AND fg-rcpth.i-no EQ oe-ordl.i-no
          AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
        EACH fg-rdtlh NO-LOCK
        WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
      rtnValue = rtnValue + fg-rdtlh.qty.
    END. /* each fg-rcpth */
  END. /* avail job-hdr */
  opBalance = rtnValue.
  RETURN rtnValue.

END FUNCTION.
/*********************************************/
FUNCTION onHandQty RETURNS INTEGER
  (OUTPUT opQtyOnHand AS INTEGER) :
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl AND oe-ordl.job-no NE '' THEN
    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ oe-ordl.company
          AND fg-bin.job-no EQ oe-ordl.job-no
          AND fg-bin.job-no2 EQ oe-ordl.job-no2
          AND fg-bin.i-no EQ oe-ordl.i-no:
      rtnValue = rtnValue + fg-bin.qty.
    END. /* each fg-bin */
  END. /* avail job-hdr */
  opQtyOnHand = rtnValue.
  RETURN rtnValue.

END FUNCTION.

/********************************************************************************************/
