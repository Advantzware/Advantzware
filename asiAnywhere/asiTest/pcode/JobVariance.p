

/*------------------------------------------------------------------------
    File        : JobVariance.p
    Purpose     : JobVariance

    Syntax      :

    Description : Return a Dataset of all JobVariance

    Author(s)   : Jyoti Bajaj
    Created     : Jan 28 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{JobVariance.i}

DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.

DEFINE INPUT PARAMETER prmJob    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob2 as Character no-undo.
DEFINE INPUT PARAMETER prmCustomer    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEst as Character no-undo.
DEFINE INPUT PARAMETER prmOpen    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmClosed as CHAR no-undo.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsJobVariance .
DEFINE VARIABLE vJob   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vEst   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE opQtyOnHand AS INTEGER NO-UNDO.
DEFINE VARIABLE OuPct AS INTEGER NO-UNDO.
DEFINE VARIABLE wipQty AS INTEGER NO-UNDO.
IF prmAction = ?  THEN ASSIGN prmAction = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF  prmItemNum = ? THEN ASSIGN prmItemNum  = "".

IF prmJob = ? THEN ASSIGN prmJob = "".
IF  prmJob2 = ? THEN ASSIGN prmJob2  = "".
IF prmCustomer = ? THEN ASSIGN prmCustomer = "".
IF  prmEst = ? THEN ASSIGN prmEst  = "".
IF prmuser = ? THEN prmUser = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

    /* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
FOR EACH ttJobVariance :
    DELETE ttJobVariance.
END.

IF prmAction = "Select" THEN DO:
    

/*    FOR EACH job-hdr WHERE prmComp EQ job-hdr.company AND
        job-hdr.i-no = prmItemNum 
                       OR job-hdr.job = 0
                       AND job-hdr.opened  NO-LOCK, 
        FIRST job OF job-hdr WHERE job.opened NO-LOCK:*/
 
        FOR EACH job-hdr  WHERE job-hdr.company = prmComp  AND 
            ((job-hdr.opened EQ YES AND prmOpen = "Yes" ) OR (job-hdr.opened EQ NO AND prmClosed = "No"))
             AND (job-hdr.cust-no   BEGINS prmCustomer OR prmCustomer = "" )
             AND (job-hdr.i-no      BEGINS prmItemNum OR  prmItemNum = "")
             AND  (job-hdr.est-no    BEGINS FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) OR prmEst = "" )
             AND (job-hdr.job-no    BEGINS prmJob OR prmJob = "")
             AND (job-hdr.job-no2  EQ int(prmJob2)  OR prmJob2 EQ "")
             AND  (job-hdr.ord-no = INT(prmOrderNum) OR prmOrderNum = "") NO-LOCK,

            FIRST job  WHERE job.company EQ job-hdr.company 
            AND job.job     EQ job-hdr.job 
            AND job.job-no  EQ job-hdr.job-no
            AND job.job-no2 EQ job-hdr.job-no2
            AND ((job.opened EQ YES AND prmOpen = "Yes") OR (job.opened EQ NO AND prmClosed = "No"))  NO-LOCK:

            create ttJobVariance.
            assign
                ttJobVariance.job-no            = job-hdr.job-no
                ttJobVariance.job-no2           = job-hdr.job-no2
                ttJobVariance.i-no              = job-hdr.i-no
                ttJobVariance.est-no            = job-hdr.est-no
                ttJobVariance.ord-no            = job-hdr.ord-no
                ttJobVariance.cust-no           = job-hdr.cust-no
                ttJobVariance.StartDt           = job-hdr.start-date
                ttJobVariance.CloseDt           = job-hdr.start-date
                ttJobVariance.Statu             = Job.stat
                ttJobVariance.Part              = custPart()
                ttJobVariance.JobQty            = job-hdr.qty
                ttJobVariance.OrdQty            = orderQty()
                ttJobVariance.ProdQty           = producedQty(opBalance)
                /*ttJobVariance.OnHand            = onHandQty(opQtyOnHand)*/
                ttJobVariance.ShipQty           = shipQty()
                ttJobVariance.InvQty            = invoiceQty()
               
                
                ttJobVariance.FgItem             = job-hdr.i-no
                .
           
            IF AVAILABLE job-hdr THEN DO:
                FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                            AND oe-ordl.i-no EQ job-hdr.i-no
                                            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
                IF AVAILABLE oe-ordl AND oe-ordl.job-no NE '' THEN
                    FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ oe-ordl.company
                                              AND fg-bin.job-no EQ oe-ordl.job-no
                                              AND fg-bin.job-no2 EQ oe-ordl.job-no2
                                              AND fg-bin.i-no EQ oe-ordl.i-no:
                    opQtyOnHand = opQtyOnHand + fg-bin.qty.
                    ASSIGN ttJobVariance.QtyHand = opQtyOnHand.
                    END. /* each fg-bin */
            END. /* avail job-hdr */

            IF AVAILABLE job-hdr THEN DO:
                FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                             AND oe-ordl.i-no EQ job-hdr.i-no
                                             AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
                IF AVAILABLE oe-ordl THEN DO:
                    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
                    wipQty = oe-ordl.qty - (opQtyOnHand + oe-ordl.ship-qty).
                    IF wipQty LT 0 OR
                        wipQty LT oe-ordl.qty *
                        (IF AVAIL oe-ord THEN oe-ordl.under-pct ELSE 100) / 100 THEN
                            wipQty = 0.
                            ASSIGN ttJobVariance.WipQty            = wipQty.

                END. /* avail oe-ordl */
            END. /* avail job-hdr */

            IF AVAILABLE job-hdr THEN DO:
                FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                             AND oe-ordl.i-no EQ job-hdr.i-no
                                             AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
                IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:
                    OuPct = ((opQtyOnHand / oe-ordl.qty) - 1) * 100.
                    IF OuPct EQ 0 THEN OuPct = 100.
                    IF OuPct EQ -100 THEN OuPct = 0.
                    ASSIGN ttJobVariance.OUPct             = OuPct.
                END. /* avail oe-ordl */
            END. /* avail job-hdr */
              
        END.
END. /*IF prmAction = "Select" THEN DO:*/

    
/*****************************************************************************************************************************************************/
    
 /*needs dynamic query*/  
  
    
 IF prmAction = "Search" THEN DO:
     ASSIGN vJob = FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob).
    ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) .
  
    IF (prmJob <> "") OR (prmItemNum <> "") OR (prmEst <> "") OR (prmCustomer <> "") THEN  DO:
    
        FOR EACH job-hdr WHERE job-hdr.company EQ prmComp AND
                           (job-hdr.i-no BEGINS prmItemNum OR prmItemNum = "")
                           AND (job-hdr.job-no BEGINS vJob  OR prmJob = "")
                           AND (job-hdr.job-no2 = int(prmJob2) OR prmJob2 = "" )
                           AND (job-hdr.est-no BEGINS (vEst) OR prmEst = "") 
                           AND (job-hdr.cust-no BEGINS prmCustomer OR prmCustomer = "") 
                           AND ((job-hdr.opened = YES AND prmOpen = "yes")  OR  ( job-hdr.opened = NO AND prmClosed = "yes"))
                           NO-LOCK,
           
           
      


        FIRST job WHERE job.company EQ job-hdr.company 
                    AND job.job     EQ job-hdr.job     
                    AND job.job-no  EQ job-hdr.job-no  
                    AND job.job-no2 EQ job-hdr.job-no2 
                    AND (job.opened = YES AND prmOpen = "yes")  OR  ( job.opened = NO AND prmClosed = "yes")
                    NO-LOCK:

            create ttJobVariance.
            assign
                 ttJobVariance.job-no            = job-hdr.job-no
                 ttJobVariance.job-no2           = job-hdr.job-no2
                 ttJobVariance.i-no              = job-hdr.i-no
                 ttJobVariance.est-no            = job-hdr.est-no
                 ttJobVariance.ord-no            = job-hdr.ord-no
                 ttJobVariance.cust-no           = job-hdr.cust-no
                 ttJobVariance.StartDt           = job-hdr.start-date
                 ttJobVariance.CloseDt           = job-hdr.start-date
                 ttJobVariance.Statu             = Job.stat
                 ttJobVariance.Part              = custPart()
                 ttJobVariance.JobQty            = job-hdr.qty
                 ttJobVariance.OrdQty            = orderQty()
                 ttJobVariance.ProdQty           = producedQty(opBalance)
                 /*ttJobVariance.OnHand            = onHandQty(opQtyOnHand)*/
                 ttJobVariance.ShipQty           = shipQty()
                 ttJobVariance.InvQty            = invoiceQty()


                 ttJobVariance.FgItem             = job-hdr.i-no
                 .

             IF AVAILABLE job-hdr THEN DO:
                 FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                             AND oe-ordl.i-no EQ job-hdr.i-no
                                             AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
                 IF AVAILABLE oe-ordl AND oe-ordl.job-no NE '' THEN
                     FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ oe-ordl.company
                                               AND fg-bin.job-no EQ oe-ordl.job-no
                                               AND fg-bin.job-no2 EQ oe-ordl.job-no2
                                               AND fg-bin.i-no EQ oe-ordl.i-no:
                     opQtyOnHand = opQtyOnHand + fg-bin.qty.
                     ASSIGN ttJobVariance.QtyHand = opQtyOnHand.
                     END. /* each fg-bin */
             END. /* avail job-hdr */

             IF AVAILABLE job-hdr THEN DO:
                 FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                              AND oe-ordl.i-no EQ job-hdr.i-no
                                              AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
                 IF AVAILABLE oe-ordl THEN DO:
                     FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
                     wipQty = oe-ordl.qty - (opQtyOnHand + oe-ordl.ship-qty).
                     IF wipQty LT 0 OR
                         wipQty LT oe-ordl.qty *
                         (IF AVAIL oe-ord THEN oe-ordl.under-pct ELSE 100) / 100 THEN
                             wipQty = 0.
                             ASSIGN ttJobVariance.WipQty            = wipQty.

                 END. /* avail oe-ordl */
             END. /* avail job-hdr */

             IF AVAILABLE job-hdr THEN DO:
                 FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                              AND oe-ordl.i-no EQ job-hdr.i-no
                                              AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
                 IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:
                     OuPct = ((opQtyOnHand / oe-ordl.qty) - 1) * 100.
                     IF OuPct EQ 0 THEN OuPct = 100.
                     IF OuPct EQ -100 THEN OuPct = 0.
                     ASSIGN ttJobVariance.OUPct             = OuPct.
                 END. /* avail oe-ordl */
             END. /* avail job-hdr */
        END.
    END.
    ELSE IF (prmJob = "") OR (prmItemNum = "") OR (prmEst = "") OR (prmCustomer = "") THEN  DO:
        
        FOR EACH job-hdr WHERE job-hdr.company EQ prmComp AND
                         (job-hdr.ord-no = int(prmOrderNum  ) )
                           AND ((job-hdr.opened = YES AND prmOpen = "yes")  OR  ( job-hdr.opened = NO AND prmClosed = "yes"))
                           NO-LOCK,


        FIRST job WHERE job.company EQ job-hdr.company 
                    AND job.job     EQ job-hdr.job     
                    AND job.job-no  EQ job-hdr.job-no  
                    AND job.job-no2 EQ job-hdr.job-no2 
                    AND (job.opened = YES AND prmOpen = "yes")  OR  ( job.opened = NO AND prmClosed = "yes")
                    NO-LOCK:

            assign
                 ttJobVariance.job-no            = job-hdr.job-no
                 ttJobVariance.job-no2           = job-hdr.job-no2
                 ttJobVariance.i-no              = job-hdr.i-no
                 ttJobVariance.est-no            = job-hdr.est-no
                 ttJobVariance.ord-no            = job-hdr.ord-no
                 ttJobVariance.cust-no           = job-hdr.cust-no
                 ttJobVariance.StartDt           = job-hdr.start-date
                 ttJobVariance.CloseDt           = job-hdr.start-date
                 ttJobVariance.Statu             = Job.stat
                 ttJobVariance.Part              = custPart()
                 ttJobVariance.JobQty            = job-hdr.qty
                 ttJobVariance.OrdQty            = orderQty()
                 ttJobVariance.ProdQty           = producedQty(opBalance)
                 /*ttJobVariance.OnHand            = onHandQty(opQtyOnHand)*/
                 ttJobVariance.ShipQty           = shipQty()
                 ttJobVariance.InvQty            = invoiceQty()


                 ttJobVariance.FgItem             = job-hdr.i-no
                 .

             IF AVAILABLE job-hdr THEN DO:
                 FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                             AND oe-ordl.i-no EQ job-hdr.i-no
                                             AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
                 IF AVAILABLE oe-ordl AND oe-ordl.job-no NE '' THEN
                     FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ oe-ordl.company
                                               AND fg-bin.job-no EQ oe-ordl.job-no
                                               AND fg-bin.job-no2 EQ oe-ordl.job-no2
                                               AND fg-bin.i-no EQ oe-ordl.i-no:
                     opQtyOnHand = opQtyOnHand + fg-bin.qty.
                     ASSIGN ttJobVariance.QtyHand = opQtyOnHand.
                     END. /* each fg-bin */
             END. /* avail job-hdr */

             IF AVAILABLE job-hdr THEN DO:
                 FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                              AND oe-ordl.i-no EQ job-hdr.i-no
                                              AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
                 IF AVAILABLE oe-ordl THEN DO:
                     FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
                     wipQty = oe-ordl.qty - (opQtyOnHand + oe-ordl.ship-qty).
                     IF wipQty LT 0 OR
                         wipQty LT oe-ordl.qty *
                         (IF AVAIL oe-ord THEN oe-ordl.under-pct ELSE 100) / 100 THEN
                             wipQty = 0.
                             ASSIGN ttJobVariance.WipQty            = wipQty.

                 END. /* avail oe-ordl */
             END. /* avail job-hdr */

             IF AVAILABLE job-hdr THEN DO:
                 FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                              AND oe-ordl.i-no EQ job-hdr.i-no
                                              AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
                 IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:
                     OuPct = ((opQtyOnHand / oe-ordl.qty) - 1) * 100.
                     IF OuPct EQ 0 THEN OuPct = 100.
                     IF OuPct EQ -100 THEN OuPct = 0.
                     ASSIGN ttJobVariance.OUPct             = OuPct.
                 END. /* avail oe-ordl */
             END. /* avail job-hdr */
        END.

    END.
 END. /*IF prmAction = "Search" THEN*/
        


