/*------------------------------------------------------------------------
  File: r-mchord.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Daily Order Report.rpa */
{aoa/tempTable/ttMachineOrdersbyDueDate.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttMachineOrdersbyDueDate.
{aoa/includes/pMachineOrdersbyDueDate.i}

/* local variables */
DEFINE VARIABLE dQtyOnHand AS DECIMAL NO-UNDO.

/* subject business logic */
DEFINE QUERY hQuery FOR job-hdr, oe-ord, cust, job, job-mch, oe-ordl, itemfg, ef SCROLLING.

OPEN QUERY hQuery
FOR EACH job-hdr NO-LOCK
    WHERE job-hdr.company EQ "001"
      AND job-hdr.opened  EQ YES
      AND job-hdr.ord-no  GT 0,
    FIRST oe-ord NO-LOCK
    WHERE oe-ord.company EQ job-hdr.company
      AND oe-ord.ord-no  EQ job-hdr.ord-no,
    FIRST cust OF job-hdr NO-LOCK,
    EACH job OF job-hdr NO-LOCK,
    EACH job-mch NO-LOCK
    WHERE job-mch.company EQ job.company
      AND job-mch.job     EQ job.job
      AND job-mch.job-no  EQ job.job-no
      AND job-mch.job-no2 EQ job.job-no2
      AND job-mch.run-complete EQ NO,
    FIRST oe-ordl OF oe-ord NO-LOCK
    WHERE oe-ordl.i-no EQ job-mch.i-no,
    FIRST itemfg NO-LOCK
    WHERE itemfg.company EQ job-mch.company
      AND itemfg.i-no    EQ job-mch.i-no,
    FIRST ef NO-LOCK OUTER-JOIN
    WHERE ef.company EQ job.company
      AND ef.est-no  EQ job.est-no
      AND ef.form-no EQ job-mch.frm
    BY job-mch.m-code
    BY oe-ord.due-date
    .
GET FIRST hQuery.
DO WHILE AVAILABLE job-hdr:
    dQtyOnHand = 0.
    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ job-mch.company
          AND fg-bin.i-no    EQ job-mch.i-no
          AND fg-bin.job-no  EQ job-mch.job-no
          AND fg-bin.job-no2 EQ job-mch.job-no2
        :
        dQtyOnHand = dQtyOnHand + fg-bin.qty.
    END. /* each fg-bin */
    CREATE ttMachineOrdersbyDueDate.
    ASSIGN
        ttMachineOrdersbyDueDate.machine       = job-mch.m-code
        ttMachineOrdersbyDueDate.orderNo       = job-hdr.ord-no
        ttMachineOrdersbyDueDate.dueDate       = oe-ord.due-date
        ttMachineOrdersbyDueDate.fgItem        = itemfg.i-name
        ttMachineOrdersbyDueDate.inventory     = job-mch.job-no BEGINS "S"
        ttMachineOrdersbyDueDate.custName      = cust.name
        ttMachineOrdersbyDueDate.qtyOrdered    = job-mch.run-qty
        ttMachineOrdersbyDueDate.numberOut     = IF ef.n-out NE ? THEN ef.n-out ELSE 0
        ttMachineOrdersbyDueDate.sheetsOrdered = oe-ordl.po-no-po NE 0
        ttMachineOrdersbyDueDate.qtyOnHand     = dQtyOnHand
        .
    GET NEXT hQuery.
END. /* do while */
