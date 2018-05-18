/* -------------------------------------------------- oe/oe-level.p 3/96 fwk  */
/* modified by BV to remove shared variables and buffers and execute program in PriceProcs.p                             */
/* 
    Purpose:  return the appropriate starting level based on customer (.cust-level) and (??combined order quantity??)    */
/* -------------------------------------------------------------------------- */
DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
DEFINE OUTPUT PARAMETER opiLevel AS INTEGER NO-UNDO.   

DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.

DEFINE VARIABLE dQty          AS DECIMAL NO-UNDO.
DEFINE VARIABLE lMatrixExists AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdPriceProcs  AS HANDLE.
{oe/ttPriceHold.i "NEW SHARED"}
RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
                                             
FOR EACH bf-oe-ordl NO-LOCK  
    WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
    AND bf-oe-ordl.ord-no EQ ipbf-oe-ord.ord-no 
    :
    ASSIGN 
        dQty = dQty + bf-oe-ordl.qty.
END.
 
FIND FIRST bf-oe-ordl NO-LOCK 
    WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
    AND bf-oe-ordl.ord-no EQ ipbf-oe-ord.ord-no
    NO-ERROR.

RUN GetPriceMatrixLevel IN hdPriceProcs (ipbf-oe-ord.company, bf-oe-ordl.i-no, ipbf-oe-ord.cust-no, ipbf-oe-ord.ship-id,
    dQty,
    OUTPUT opiLevel).
  

