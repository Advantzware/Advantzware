/*------------------------------------------------------------------------
    File        : api\inbound\InventoryReceiptProcs.p
    Purpose     : common procs for creating and posting receipts
    Syntax      :

    Description : common procs for creating and posting receipts

    Author(s)   : Vishnu Vellanki
    Created     : Mon Nov 25 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{jc/jcgl-sh.i NEW}

{api\inbound\ttRctd.i}
{api\inbound\ttMat.i}
{rm\ttBoardToWIP.i}
    
PROCEDURE InventoryReceipt_GetCostsFromPO:
/*------------------------------------------------------------------------------
 Purpose: Gets costs from PO values
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPONumber         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty              AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM       AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotalFreight AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dCostPerEA        AS DECIMAL.
    DEFINE VARIABLE dCostFreight      AS DECIMAL.
    DEFINE VARIABLE dCostFreightPerEA AS DECIMAL.
    DEFINE VARIABLE lFound            AS LOGICAL.
    DEFINE VARIABLE lFGPOFrt          AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cReturn           AS CHARACTER.
    DEFINE VARIABLE hdCostProcs       AS HANDLE  NO-UNDO.
    
    RUN system\CostProcs.p PERSISTENT SET hdCostProcs.
    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, /* Company Code */
        INPUT "FGPOFRT",  /* sys-ctrl name */
        INPUT "L",        /* Output return value */
        INPUT NO,         /* Use ship-to */
        INPUT NO,         /* ship-to vendor */
        INPUT "",         /* ship-to vendor value */
        INPUT "",         /* shi-id value */
        OUTPUT cReturn, 
        OUTPUT lFound
        ).

    lFGPOFrt = lFound AND cReturn EQ "YES".
    
    RUN GetCostForPOLine IN hdCostProcs (
        INPUT ipcCompany, 
        INPUT ipiPONumber, 
        INPUT ipiPOLine, 
        INPUT ipcFGItemID, 
        OUTPUT opdCostPerUOM, 
        OUTPUT opcCostUOM, 
        OUTPUT dCostFreight, 
        OUTPUT lFound
        ).
    
    ASSIGN
        //dCostPerEA          = DYNAMIC-FUNCTION('fConvert' IN hdCostProcs, opcCostUOM, "EA",0,0,0,0,1,1, opdCostPerUOM)
        //dCostFreightPerEA   = DYNAMIC-FUNCTION('fConvert' IN hdCostProcs, opcCostUOM, "EA",0,0,0,0,1,1, dCostFreight)
        dCostPerEA = DYNAMIC-FUNCTION('fConvertCostForItem':U IN hdCostProcs,
            ipcCompany, 
            ipcFGItemID, 
            "FG", 
            opdCostPerUOM, 
            opcCostUOM, 
            "EA", 
            0, /*BasisWeight*/
            0, /*Length override - leave as 0 if not in UI or on Order/PO*/
            0, /*Width override - leave as 0 if not in UI or on Order/PO*/
            0, /*Depth override - leave as 0 if not in UI or on Order/PO*/
            0, /*Case Count override - leave as 0 if not in UI or on Order/PO*/
            ipdQty, /*Lot Quantity - leave as 0 if not in UI or on Order/PO*/
            "EA" /*Lot Quantity UOM - leave as "" if not in UI or on PO*/
            )
        dCostFreightPerEA = DYNAMIC-FUNCTION('fConvertCostForItem':U IN hdCostProcs,
            ipcCompany, 
            ipcFGItemID, 
            "FG", 
            dCostFreight, 
            opcCostUOM, 
            "EA", 
            0, /*BasisWeight*/
            0, /*Length override - leave as 0 if not in UI or on Order/PO*/
            0, /*Width override - leave as 0 if not in UI or on Order/PO*/
            0, /*Depth override - leave as 0 if not in UI or on Order/PO*/
            0, /*Case Count override - leave as 0 if not in UI or on Order/PO*/
            ipdQty, /*Lot Quantity - leave as 0 if not in UI or on Order/PO*/
            "EA" /*Lot Quantity UOM - leave as "" if not in UI or on PO*/
            )
        opdCostTotal        = ipdQty * dCostPerEA
        opdCostTotalFreight = ipdQty * dCostFreightPerEA
        opdCostTotal        = IF lFGPOFrt THEN 
                                  opdCostTotal + opdCostTotalFreight
                              ELSE
                                  0
        .
    DELETE PROCEDURE hdCostProcs.
END PROCEDURE.

PROCEDURE InventoryReceipt_RMIssueCreation:
/*------------------------------------------------------------------------------
 Purpose: Creates new rm-rctd record for Issue
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttRctd.
    DEFINE INPUT        PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcTag      AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQty1    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQty2    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cUOMEach AS CHARACTER NO-UNDO INITIAL "EA" /* each */.
    
    DEFINE BUFFER bf-ttRctd FOR ttRctd.

    EMPTY TEMP-TABLE ttMat.
    
    FIND FIRST ttRctd NO-LOCK
         WHERE ttRctd.company EQ ipcCompany
           AND ttRctd.tag     EQ ipcTag
         NO-ERROR.
    IF NOT AVAILABLE ttRctd THEN DO:
        ASSIGN
            ioplSuccess = NO
            opcMessage  = "Temp table records are not available"
            .
            
        RETURN.
    END.   
    IF ttRctd.job-no NE "" AND ttRctd.s-num EQ ? THEN
        FIND FIRST job NO-LOCK
             WHERE job.company EQ ttRctd.company
               AND job.job-no  EQ ttRctd.job-no
               AND job.job-no2 EQ ttRctd.job-no2
        NO-ERROR.
        
    IF AVAILABLE job THEN DO:
        FOR EACH  job-mat NO-LOCK
            WHERE job-mat.company EQ job.company
              AND job-mat.job     EQ job.job
              AND job-mat.job-no  EQ job.job-no
              AND job-mat.job-no2 EQ job.job-no2
              AND job-mat.rm-i-no EQ ttRctd.i-no
            BY job-mat.frm:

            CREATE ttMat.
            ASSIGN
                ttMat.frm = job-mat.frm
                ttMat.qty = job-mat.qty
                dQty1     = dQty1 + job-mat.qty
                .
        END.
        
        FOR EACH ttMat:
            ttMat.qty = IF dQty1 NE 0 THEN
                            ttRctd.qty * (ttMat.qty / dQty1)
                        ELSE
                            0.
 
            IF ttRctd.pur-uom EQ cUOMEach THEN DO:
                IF (ttMat.qty - INT(ttMat.qty)) > 0 THEN 
                    ttMat.qty = INT(ttMat.qty) + 1.
                ELSE 
                    ttMat.qty = INT(ttMat.qty).
            END.
            
            dQty2 = dQty2 + ttMat.qty.
        END.

        IF dQty2 NE ttRctd.qty THEN
            FOR FIRST ttMat:
                ttMat.qty = ttMat.qty + (ttRctd.qty - dQty2).
            END.
      END.
      ELSE DO:
          CREATE ttMat.
          ASSIGN
              ttMat.frm = ttRctd.s-num
              ttMat.qty = ttRctd.qty
              .
      END.
      
      FOR EACH ttMat:
          CREATE bf-ttRctd.
          BUFFER-COPY ttRctd EXCEPT rec_key TO bf-ttRctd
          ASSIGN
              bf-ttRctd.rita-code   = "I" /* Issues */
              bf-ttRctd.ttRctdRowID = ROWID(ttRctd)
              bf-ttRctd.SeqNo       = 2
              bf-ttRctd.s-num       = ttMat.frm
              bf-ttRctd.qty         = ttMat.qty 
              .
          FIND FIRST po-ord NO-LOCK 
               WHERE po-ord.company EQ ttRctd.company 
                 AND po-ord.po-no   EQ INT(ttRctd.po-no)
               NO-ERROR.
            
          IF AVAILABLE po-ord AND po-ord.TYPE NE "S" THEN
              bf-ttRctd.po-no = ""  .
          
          DELETE ttMat.
      END.
      
    RELEASE bf-ttRctd. 
END PROCEDURE.

PROCEDURE InventoryReceipt_ConvertVendCompCurr:
/*------------------------------------------------------------------------------
 Purpose: Converts currency of cost
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdCost   AS DECIMAL DECIMALS 4 NO-UNDO.
    
    FIND FIRST vend NO-LOCK
         WHERE vend.company EQ po-ord.company 
           AND vend.vend-no EQ po-ord.vend-no
         NO-ERROR.

    IF AVAILABLE vend THEN DO:
        FIND FIRST company NO-LOCK 
             WHERE company.company EQ ipcCompany
             NO-ERROR.

        IF vend.curr-code NE company.curr-code THEN DO:
            FIND FIRST currency NO-LOCK
                 WHERE currency.company EQ po-ord.company 
                   AND currency.c-code  EQ vend.curr-code
                 NO-ERROR.

            IF AVAILABLE currency THEN DO:
                iopdCost = iopdCost * currency.ex-rate.

                RELEASE currency.
            END.
        END.

        RELEASE company.
        RELEASE vend.
    END.
END PROCEDURE.

PROCEDURE InventoryReceipt_PostRMItems:
/*------------------------------------------------------------------------------
 Purpose: Posts RM items
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttRctd.
    DEFINE INPUT        PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPONo       AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplSuccess   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage    AS CHARACTER NO-UNDO.
 
    DEFINE BUFFER bf-rm-rctd  FOR rm-rctd.
    DEFINE BUFFER bf-rm-bin   FOR rm-bin.
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.

    DEFINE VARIABLE dCvtQty        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iTrNum         AS INTEGER NO-UNDO.
    DEFINE VARIABLE dBinQty        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCost          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iIndex         AS INTEGER NO-UNDO.
    DEFINE VARIABLE dReceiptQty    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOutQty        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dBwt           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dLen           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWid           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDep           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE riRecid        AS RECID   NO-UNDO.
    DEFINE VARIABLE riPOOrdl       AS ROWID   NO-UNDO.
    DEFINE VARIABLE iInc           AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttBoardToWIP.

    transblok:
    FOR EACH ttRctd
        WHERE CAN-FIND(FIRST item 
                       WHERE item.company EQ ipcCompany
                         AND item.i-no    EQ ttRctd.i-no)
        BREAK BY ttRctd.seqNo
              BY ttRctd.i-no
              BY ttRctd.r-no:
              
        FIND FIRST rm-rctd EXCLUSIVE-LOCK
             WHERE ROWID(rm-rctd) EQ ttRctd.rmrctdRowID
             NO-ERROR NO-WAIT.

        IF NOT AVAILABLE rm-rctd THEN DO:
            ioplSuccess = NO.
            IF LOCKED(rm-rctd) THEN
                opcMessage  = "Receipt Record is in use.Can Not Update for PO Number (" + ttRctd.po-no + ")".
            ELSE
                opcMessage  = "Receipt record is not available to post for PO Number (" + ttRctd.po-no + ")".
  
            RETURN.
        END.
 
        FIND FIRST item EXCLUSIVE-LOCK
             WHERE item.company EQ rm-rctd.company
               AND item.i-no    EQ rm-rctd.i-no
             NO-ERROR NO-WAIT.

        IF NOT AVAILABLE item THEN DO:
            ioplSuccess = NO.
            
            IF LOCKED(item) THEN
                opcMessage  = "Item Record is in use.Can Not Update for PO Number (" + rm-rctd.po-no + ")".
            ELSE
                opcMessage  = "Item record is not available to post for PO Number (" + rm-rctd.po-no + ")".
  
            RETURN.
        END.

        IF rm-rctd.rita-code EQ "I" /* Issue */ AND INT(rm-rctd.po-no) NE 0 THEN
            FOR EACH bf-rm-rctd NO-LOCK
                WHERE bf-rm-rctd.company   EQ ipcCompany
                  AND bf-rm-rctd.i-no      EQ rm-rctd.i-no
                  AND bf-rm-rctd.rita-code EQ "R" /* Receipts */
                  AND bf-rm-rctd.po-no     EQ rm-rctd.po-no
                  AND bf-rm-rctd.r-no      LT rm-rctd.r-no:
  
                UNDO transblok, NEXT transblok.
            END.
  
        FIND FIRST job NO-LOCK
             WHERE job.company EQ rm-rctd.company
               AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no))) + TRIM(rm-rctd.job-no) /* e.g. W98201 */ 
               AND job.job-no2 EQ rm-rctd.job-no2
             NO-ERROR.
  
        /** Find Bin & if not avail then create it **/
        FIND FIRST rm-bin
             WHERE rm-bin.company EQ rm-rctd.company
               AND rm-bin.loc     EQ rm-rctd.loc
               AND rm-bin.i-no    EQ rm-rctd.i-no
               AND rm-bin.loc-bin EQ rm-rctd.loc-bin
               AND rm-bin.tag     EQ rm-rctd.tag
             NO-ERROR.
        IF NOT AVAILABLE rm-bin THEN DO:
            CREATE rm-bin.
            ASSIGN
                rm-bin.company = rm-rctd.company
                rm-bin.loc     = rm-rctd.loc
                rm-bin.loc-bin = rm-rctd.loc-bin
                rm-bin.tag     = rm-rctd.tag
                rm-bin.i-no    = rm-rctd.i-no
                .
        END.
  
        dCvtQty= rm-rctd.qty.
  
        IF NOT rm-rctd.rita-code  EQ "T" /* Transfers */ AND
           NOT (rm-rctd.rita-code EQ "A" /* Adjustments */ AND rm-rctd.qty LT 0) THEN
            rm-bin.po-no = ipiPONo.
  
        IF rm-rctd.pur-uom NE item.cons-uom AND item.cons-uom NE "" THEN
            RUN sys/ref/convquom.p (
                INPUT  rm-rctd.pur-uom,
                INPUT  item.cons-uom,
                INPUT  item.basis-w,
                INPUT  (IF item.r-wid EQ 0 THEN item.s-len ELSE 12),
                INPUT  (IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid),
                INPUT  item.s-dep,
                INPUT  dCvtQty,
                OUTPUT dCvtQty
                ).
  
        IF rm-rctd.rita-code EQ "R" THEN DO: /** RECEIPTS **/
            RUN pRMPost (
                INPUT        rm-bin.qty,
                INPUT-OUTPUT rm-bin.cost,
                INPUT        rm-rctd.qty,
                INPUT-OUTPUT rm-rctd.cost
                ).
  
            /* If total quantity was zero, rm-bin.cost will be ? */
            IF rm-bin.cost EQ ? THEN 
	         rm-bin.cost = 0.
				
            ASSIGN
                rm-bin.qty     = rm-bin.qty + dCvtQty
                item.last-cost = rm-rctd.cost
                item.q-onh     = item.q-onh + dCvtQty
                .
  
            IF INT(rm-rctd.po-no) NE 0 THEN
            update-po2: 
            DO:
                /* Check to see if po entered through the purchasing module. If
                so, verify all charaters are number type because po number
                in purchasing is a integer field. */
                DO iInc = 1 TO LENGTH(rm-rctd.po-no):
                    IF ASC(SUBSTRING(rm-rctd.po-no,iInc,1)) LT 48 /* zero */ OR
                       ASC(SUBSTRING(rm-rctd.po-no,iInc,1)) GT 57 /* nine */ THEN
                        LEAVE update-po2.
                END.
      
                FIND FIRST po-ord EXCLUSIVE-LOCK
                     WHERE po-ord.company EQ item.company
                       AND po-ord.po-no   EQ INT(rm-rctd.po-no)
                     NO-WAIT NO-ERROR.

                IF NOT AVAILABLE po-ord AND LOCKED po-ord THEN DO:
                    ASSIGN
                        ioplSuccess = NO
                        opcMessage  = "Purchase Order Record is in use.Can Not Update for PO Number (" + rm-rctd.po-no + ")"
                        .
      
                    RETURN.
                END.
      
                FOR EACH po-ordl NO-LOCK
                    WHERE po-ordl.company   EQ item.company
                      AND po-ordl.i-no      EQ rm-rctd.i-no
                      AND po-ordl.po-no     EQ int(rm-rctd.po-no)
                      AND po-ordl.deleted   EQ NO
                      AND po-ordl.item-type EQ YES
                      AND po-ordl.job-no    EQ rm-rctd.job-no
                      AND po-ordl.job-no2   EQ rm-rctd.job-no2
                    BREAK BY po-ordl.s-num DESCENDING:
      
                    riPOOrdl = ROWID(po-ordl).
      
                    IF LAST(po-ordl.s-num) OR po-ordl.s-num EQ rm-rctd.s-num THEN
                        LEAVE.
                END.
                
                FIND FIRST po-ordl EXCLUSIVE-LOCK
                     WHERE ROWID(po-ordl) EQ riPOOrdl
                     NO-WAIT NO-ERROR.
  
                IF NOT AVAILABLE po-ordl THEN DO:
                    ioplSuccess = NO.

                    IF LOCKED po-ordl THEN
                        opcMessage  = "Purchase Order Line Record is in use. Can Not Update for PO Number (" + rm-rctd.po-no + ")".
                    ELSE
                        opcMessage  = "Purchase Order Line Record not found. Can Not Update for PO Number (" + rm-rctd.po-no + ")".
       
                    RETURN.
                END.
                dCvtQty = rm-rctd.qty.
                
                IF rm-rctd.pur-uom NE po-ordl.cons-uom THEN
                    RUN sys/ref/convquom.p (
                        INPUT rm-rctd.pur-uom,
                        INPUT po-ordl.cons-uom,
                        INPUT item.basis-w,
                        INPUT po-ordl.s-len,
                        INPUT po-ordl.s-wid,
                        INPUT item.s-dep,
                        INPUT dCvtQty,
                        OUTPUT dCvtQty
                        ).
                ASSIGN
                    po-ord.received   = YES
                    po-ordl.t-rec-qty = po-ordl.t-rec-qty + dCvtQty
                    .
                    
                RUN rm/polclose.p (
                    INPUT ROWID(po-ordl),
                    INPUT rm-rctd.qty,
                    INPUT rm-rctd.pur-uom
                    ).
            END.
            item.q-avail = item.q-onh + item.q-ono - item.q-comm.
        END. /* R */
        ELSE IF rm-rctd.rita-code EQ "I" THEN DO:  /** ISSUES **/
  
            IF rm-rctd.tag NE "" THEN
                FOR EACH  bf-rm-rdtlh FIELDS(r-no rita-code tag2) NO-LOCK
                    WHERE bf-rm-rdtlh.company   EQ ipcCompany
                      AND bf-rm-rdtlh.tag       EQ rm-rctd.tag
                      AND bf-rm-rdtlh.loc       EQ rm-rctd.loc
                      AND bf-rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin
                      AND bf-rm-rdtlh.rita-code EQ "R"  /* Receipts */
                      AND bf-rm-rdtlh.tag2      NE "",
                    FIRST bf-rm-rcpth NO-LOCK
                    WHERE bf-rm-rcpth.r-no      EQ bf-rm-rdtlh.r-no
                      AND bf-rm-rcpth.rita-code EQ bf-rm-rdtlh.rita-code
                      AND bf-rm-rcpth.i-no      EQ rm-rctd.i-no:
                    rm-rctd.tag2 = bf-rm-rdtlh.tag2.
                END.
  
            IF AVAILABLE job AND job.job-no NE "" THEN DO:
                RUN rm/mkjobmat.p (
                    INPUT RECID(rm-rctd),
                    INPUT rm-rctd.company,
                    OUTPUT riRecid
                    ).
  
                FIND FIRST job-mat
                     WHERE RECID(job-mat) EQ riRecid NO-ERROR.

                IF NOT AVAILABLE job-mat THEN DO:
                    ASSIGN
                        ioplSuccess = NO
                        opcMessage  = "Job Mat Record not found for " + STRING(job.job-no) + "-" + STRING(job.job-no2,"99") + "  " + rm-rctd.i-no
                        .
  
                    RETURN.
                END.
  
                ASSIGN
                    dBwt = job-mat.basis-w
                    dLen = job-mat.len
                    dWid = job-mat.wid
                    dDep = item.s-dep
                    .
  
                IF dLen EQ 0 THEN
                    dLen = item.s-len.
  
                IF dWid EQ 0 THEN
                    dWid = IF item.r-wid NE 0 THEN
                               item.r-wid
                           ELSE
                               item.s-wid.
  
                IF dBwt EQ 0 THEN
                    dBwt = item.basis-w.
  
                IF INDEX("RL",job.stat) NE 0 THEN /* Checks job status whether it is "Released" or "Last Material" */
                    job.stat = "W" /* Work in process */.
  
                CREATE mat-act.
                ASSIGN
                    mat-act.company   = ipcCompany
                    mat-act.mat-date  = TODAY
                    mat-act.job       = job.job
                    mat-act.job-no    = job-mat.job-no
                    mat-act.job-no2   = job-mat.job-no2
                    mat-act.s-num     = job-mat.frm
                    mat-act.b-num     = job-mat.blank-no
                    mat-act.i-no      = job-mat.i-no
                    mat-act.i-name    = rm-rctd.i-name
                    mat-act.rm-i-no   = job-mat.i-no
                    mat-act.rm-i-name = rm-rctd.i-name
                    mat-act.pass      = rm-rctd.pass
                    mat-act.tag       = rm-rctd.tag
                    mat-act.loc       = rm-rctd.loc
                    mat-act.loc-bin   = rm-rctd.loc-bin
                    mat-act.opn       = yes
                    mat-act.mat-time  = time
                    .
  
                dOutQty = rm-rctd.qty.
                
                IF rm-rctd.pur-uom NE job-mat.qty-uom AND rm-rctd.pur-uom NE "" THEN
                    RUN sys/ref/convquom.p(
                        INPUT rm-rctd.pur-uom,
                        INPUT job-mat.qty-uom,
                        INPUT dBwt,
                        INPUT dLen,
                        INPUT dWid,
                        INPUT dDep,
                        INPUT rm-rctd.qty,
                        OUTPUT dOutQty
                        ).
  
                dCost = rm-rctd.cost.
  
                IF rm-rctd.pur-uom NE job-mat.sc-uom AND rm-rctd.pur-uom NE "" THEN
                    RUN sys/ref/convcuom.p(
                        INPUT rm-rctd.pur-uom,
                        INPUT job-mat.sc-uom,
                        INPUT dBwt,
                        INPUT dLen,
                        INPUT dWid,
                        INPUT dDep,
                        INPUT rm-rctd.cost,
                        OUTPUT dCost
                        ).
  
                ASSIGN
                    mat-act.qty-uom = job-mat.qty-uom
                    mat-act.cost    = dCost
                    mat-act.qty     = mat-act.qty     + dOutQty
                    job-mat.qty-iss = job-mat.qty-iss + dOutQty
                    job-mat.qty-all = job-mat.qty-all - dOutQty
                    //item.q-comm     = item.q-comm     - rm-rctd.qty handled in write trigger of job-mat
                    .
  
                RUN sys/ref/convquom.p(
                    INPUT rm-rctd.pur-uom,
                    INPUT job-mat.sc-uom,
                    INPUT dBwt,
                    INPUT dLen,
                    INPUT dWid,
                    INPUT dDep,
                    INPUT rm-rctd.qty,
                    OUTPUT dOutQty
                    ).
  
                mat-act.ext-cost = mat-act.ext-cost + (dCost * dOutQty).
  
                IF job-mat.qty-all LT 0 THEN DO:
                
                    RUN sys/ref/convquom.p(
                        INPUT job-mat.qty-uom,
                        INPUT rm-rctd.pur-uom,
                        INPUT dBwt,
                        INPUT dLen,
                        INPUT dWid,
                        INPUT dDep,
                        INPUT job-mat.qty-all,
                        OUTPUT dOutQty
                        ).
                        
                    ASSIGN
                        job-mat.qty-all = 0
                        //item.q-comm     = item.q-comm - dOutQty
                        .
                END.
  
                IF item.q-comm LT 0 THEN
                    item.q-comm = 0.
                    
                IF item.mat-type EQ "B" THEN
                    RUN rm/rm-addcr.p (
                        INPUT ROWID(rm-rctd)
                        ).
            END.
  
            FIND FIRST rm-bin
                 WHERE rm-bin.company EQ rm-rctd.company
                   AND rm-bin.loc     EQ rm-rctd.loc
                   AND rm-bin.i-no    EQ rm-rctd.i-no
                   AND rm-bin.loc-bin EQ rm-rctd.loc-bin
                   AND rm-bin.tag     EQ rm-rctd.tag
                 NO-ERROR.
  
            ASSIGN
                rm-bin.qty     = rm-bin.qty - dCvtQty
                item.q-onh     = item.q-onh - dCvtQty
                item.qlast-iss = rm-rctd.qty
                item.dlast-iss = rm-rctd.rct-date
                item.q-ytd     = item.q-ytd + rm-rctd.qty
                item.q-ptd     = item.q-ptd + rm-rctd.qty
                item.u-ptd     = item.u-ptd + (rm-rctd.cost * rm-rctd.qty)
                item.u-ytd     = item.u-ytd + (rm-rctd.cost * rm-rctd.qty)
                item.q-avail   = item.q-onh + item.q-ono - item.q-comm
                .
  
            IF rm-bin.qty EQ 0 THEN
                DELETE rm-bin.
  
        END.  /* I */
        IF rm-rctd.rita-code EQ "A" THEN DO:  /** ADJUSTMENTS **/
            IF rm-rctd.cost NE 0 THEN DO:
                RUN pRMPost (
                    INPUT        rm-bin.qty,
                    INPUT-OUTPUT rm-bin.cost,
                    INPUT        rm-rctd.qty,
                    INPUT-OUTPUT rm-rctd.cost
                    ).
            END.
  
            ASSIGN
                rm-bin.qty     = rm-bin.qty + dCvtQty
                item.last-cost = IF rm-rctd.cost NE 0 THEN
                                     rm-rctd.cost
                                 ELSE
                                     item.last-cost
                item.q-onh     = item.q-onh + dCvtQty
                item.q-avail   = item.q-onh + item.q-ono - item.q-comm
                .
        END. /* A */
        ELSE IF rm-rctd.rita-code EQ "T" THEN DO:  /** TRANSFERS **/
            ASSIGN
                rm-bin.qty   = rm-bin.qty - rm-rctd.qty
                rm-rctd.cost = rm-bin.cost
                .
  
            /* This code is to handel the Transfer to quantity to increase the BIN
            using a buffer record so current rm-bin record is not updated. */
  
            FIND FIRST bf-rm-bin
                 WHERE bf-rm-bin.company EQ rm-rctd.company
                   AND bf-rm-bin.loc     EQ rm-rctd.loc2
                   AND bf-rm-bin.i-no    EQ rm-rctd.i-no
                   AND bf-rm-bin.loc-bin EQ rm-rctd.loc-bin2
                   AND bf-rm-bin.tag     EQ rm-rctd.tag2
                 NO-ERROR.
            IF NOT AVAILABLE bf-rm-bin THEN DO:
                CREATE bf-rm-bin.
                ASSIGN
                    bf-rm-bin.company = rm-rctd.company
                    bf-rm-bin.loc     = rm-rctd.loc2
                    bf-rm-bin.loc-bin = rm-rctd.loc-bin2
                    bf-rm-bin.tag     = rm-rctd.tag2
                    bf-rm-bin.i-no    = rm-rctd.i-no
                    .
            END.
            RUN pRMPost (
                INPUT        bf-rm-bin.qty,
                INPUT-OUTPUT bf-rm-bin.cost,
                INPUT        rm-rctd.qty,
                INPUT-OUTPUT rm-rctd.cost
                ).
            bf-rm-bin.qty = bf-rm-bin.qty + rm-rctd.qty.
        END. /* T */
  
        IF TRIM(rm-rctd.tag) NE "" THEN
            FIND FIRST loadtag EXCLUSIVE-LOCK
                 WHERE loadtag.company     EQ rm-rctd.company
                   AND loadtag.item-type   EQ YES
                   AND loadtag.tag-no      EQ rm-rctd.tag
                   AND loadtag.i-no        EQ rm-rctd.i-no
                   AND loadtag.is-case-tag EQ NO
                 NO-ERROR.
  
            IF AVAILABLE loadtag THEN DO:
                ASSIGN
                    loadtag.loc     = rm-rctd.loc
                    loadtag.loc-bin = rm-rctd.loc-bin
                    .
        
            iIndex = INDEX("RI",rm-rctd.rita-code). /* checking rita-code for "RECEIPTS" and "ISSUES" */
  
            IF iIndex EQ 1 AND (NOT AVAILABLE rm-bin OR rm-bin.qty LT 0) THEN
                iIndex = 3.
  
            IF iIndex GT 0 THEN
                loadtag.sts = ENTRY(iIndex,"Received,Issued,Deleted").
        END.
  
        IF LAST-OF(ttRctd.i-no) THEN             /* Calculate average cost */
            FOR EACH rm-bin NO-LOCK
                WHERE rm-bin.company EQ rm-rctd.company
                  AND rm-bin.i-no    EQ rm-rctd.i-no
                BREAK BY rm-bin.i-no:
  
                IF FIRST(rm-bin.i-no) THEN
                    ASSIGN
                        dReceiptQty = 0
                        dCost       = 0
                        dBinQty     = rm-bin.qty
                        .
  
                IF dBinQty LT 0 THEN
                    dBinQty = dBinQty * -1.
  
                ASSIGN
                    dReceiptQty = dReceiptQty + dBinQty
                    dCost       = dCost    + (dBinQty * rm-bin.cost)
                    .
  
                IF dCost EQ ? THEN
                    dCost = 0.
  
                IF LAST(rm-bin.i-no) AND dReceiptQty NE 0 AND dCost NE 0
                    AND dReceiptQty NE ? AND dCost NE ? THEN
                    item.avg-cost = dCost / dReceiptQty.
  
            END. 
  
        RUN pAssignPrepInfo (
            INPUT  ipcCompany
            ).

        RUN pFinal (
            INPUT        ROWID(rm-rctd),
            INPUT        ipcCompany,
            INPUT-OUTPUT ioplSuccess,
            OUTPUT       opcMessage
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR OR NOT ioplSuccess THEN DO:
            ASSIGN
                ioplSuccess = NO
                opcMessage  = opcMessage + " " + ERROR-STATUS:GET-MESSAGE(1)
                .
                
            RETURN.
        END.    
        
        FIND CURRENT rm-rctd  NO-LOCK NO-ERROR.
        FIND CURRENT item     NO-LOCK NO-ERROR.
        FIND CURRENT rm-rcpth NO-LOCK NO-ERROR.
        FIND CURRENT rm-rdtlh NO-LOCK NO-ERROR.
        FIND CURRENT job-mat  NO-LOCK NO-ERROR.
        
        IF item.mat-type EQ "B" /* Board materials */ AND AVAILABLE rm-rctd THEN DO:
            CREATE ttBoardToWIP.
            ASSIGN
                ttBoardToWIP.rmrdtlhRowID = ROWID(rm-rdtlh)
                ttBoardToWIP.rmrcpthRowID = ROWID(rm-rcpth)
                ttBoardToWIP.rmbinRowID   = ROWID(rm-bin)
                ttBoardToWIP.jobmatRowID  = ROWID(job-mat)
                ttBoardToWIP.itemRowID    = ROWID(item)
                .
        END.
  
    END.
    
    FOR EACH rm-rctd EXCLUSIVE-LOCK
        WHERE rm-rctd.company   EQ ipcCompany
          AND rm-rctd.rita-code EQ "ADDER": 
        rm-rctd.rita-code = "I". /* Issues */    
    END.     

    DO TRANSACTION:
        REPEAT:  
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                 WHERE gl-ctrl.company EQ ipcCompany 
                 NO-ERROR NO-WAIT.
            
            IF LOCKED(gl-ctrl) THEN DO:
                ASSIGN
                    opcMessage  = "gl-ctrl is locked"
                    ioplSuccess = NO
                    .
                
                RETURN.
            END.
            IF AVAILABLE gl-ctrl THEN DO:
                ASSIGN
                    iTrNum        = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = iTrNum
                    .
  
                FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
  
                RUN pGLFromWork (
                    INPUT ipcCompany,
                    INPUT 1, /* Run */ 
                    INPUT iTrNum
                    ).
                RUN pGLFromWork (
                    INPUT ipcCompany,
                    INPUT 2, /* Run */
                    INPUT iTrNum
                    ).
                LEAVE.
            END.
        END.
    END.
  
    RUN pCreateWIPInventoryStock (
        INPUT ipcCompany
        ).
    
    RELEASE bf-rm-rctd.
    RELEASE bf-rm-rcpth.
    RELEASE bf-rm-rdtlh.
    RELEASE bf-rm-bin.
END PROCEDURE.

PROCEDURE pAssignPrepInfo PRIVATE:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttRctd FOR ttRctd.
    
    FOR EACH  bf-ttRctd   
        WHERE bf-ttRctd.SeqNo EQ ttRctd.SeqNo 
          AND bf-ttRctd.i-no  EQ ttRctd.i-no:
    
        FOR EACH  prep 
            WHERE prep.company EQ ipcCompany
              AND prep.CODE    EQ bf-ttRctd.i-no:
            ASSIGN
                prep.loc            = bf-ttRctd.loc
                prep.loc-bin        = bf-ttRctd.loc-bin
                prep.received-date  = bf-ttRctd.rct-date
                .
            IF bf-ttRctd.job-no NE "" THEN
                ASSIGN
                    prep.last-job-no    = bf-ttRctd.job-no
                    prep.last-job-no2   = bf-ttRctd.job-no2
                    .
        END.
    END.
    
    RELEASE bf-ttRctd.
END PROCEDURE.

PROCEDURE pFinal PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipriRMRctd   AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iSeqNo       AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyReceived AS DECIMAL NO-UNDO.
    DEFINE VARIABLE daPostDate   AS DATE    NO-UNDO.
    DEFINE VARIABLE lRMTags      AS LOGICAL NO-UNDO INITIAL YES .
     
    DEFINE BUFFER bf-ttrctd   FOR ttRctd.
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
    DEFINE BUFFER bf-rm-rctd  FOR rm-rctd.
   
    FIND FIRST bf-rm-rctd EXCLUSIVE-LOCK
         WHERE ROWID(bf-rm-rctd) EQ ipriRMRctd
         NO-WAIT NO-ERROR.
    IF NOT AVAILABLE bf-rm-rctd THEN DO:
        ASSIGN
            ioplSuccess = NO
            opcMessage  = "RM receipt is not available to create history record"
            .
            
        RETURN.
    END.
    IF bf-rm-rctd.rita-code EQ "I" /* Issues */ AND TRIM(bf-rm-rctd.tag) NE "" THEN
        FOR EACH bf-rm-rdtlh NO-LOCK
            WHERE bf-rm-rdtlh.company   EQ bf-rm-rctd.company
              AND bf-rm-rdtlh.tag       EQ bf-rm-rctd.tag
              AND bf-rm-rdtlh.rita-code EQ "R", /* Receipts */
            FIRST bf-rm-rcpth
            WHERE bf-rm-rcpth.r-no      EQ bf-rm-rdtlh.r-no
              AND bf-rm-rdtlh.rita-code EQ bf-rm-rdtlh.rita-code
            NO-LOCK:
    
            IF bf-rm-rctd.po-no EQ "" THEN 
                bf-rm-rctd.po-no = bf-rm-rcpth.po-no.
            
            IF bf-rm-rctd.job-no EQ "" THEN
                ASSIGN
                    bf-rm-rctd.job-no  = bf-rm-rcpth.job-no
                    bf-rm-rctd.job-no2 = bf-rm-rcpth.job-no2
                    .
            
            LEAVE.
        END.
       
    IF lRMTags AND TRIM(bf-rm-rctd.tag) NE "" THEN DO:
        FOR EACH wiptag EXCLUSIVE-LOCK
            WHERE wiptag.company   EQ bf-rm-rctd.company 
              AND wiptag.rm-tag-no EQ bf-rm-rctd.tag:
              
            wiptag.sts = "On Hand" .
        END.
    END.
    
    /* Creates history records */
    RUN pHistory (
        INPUT        ROWID(bf-rm-rctd),
        INPUT-OUTPUT ioplSuccess,
        OUTPUT       opcMessage
        ) NO-ERROR.
        
    IF ERROR-STATUS:ERROR OR NOT ioplSuccess THEN DO:
        ASSIGN
            opcMessage  = "Unable to create history records for PO Number (" + bf-rm-rctd.po-no + ") " + ERROR-STATUS:GET-MESSAGE(1) + " " + opcMessage
            ioplSuccess = NO
            .
            
        RETURN.
    END.
    
    DELETE bf-rm-rctd.
    
    FOR EACH bf-ttrctd WHERE bf-ttrctd.ttRctdRowID EQ ROWID(ttRctd):
        iSeqNo = 0.
        RUN sys/ref/asiseq.p (
            INPUT ipcCompany, 
            INPUT "rm_rcpt_seq", 
            OUTPUT iSeqNo
            ) NO-ERROR.
            
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN
                opcMessage  = "Could not obtain next sequence #, please contact ASI:for PO number (" + bf-rm-rctd.po-no + ") " +  ERROR-STATUS:GET-MESSAGE(1)
                ioplSuccess = NO
                .
                
            RETURN. 
        END.
    
        CREATE bf-rm-rctd.
        BUFFER-COPY bf-ttrctd TO bf-rm-rctd
        ASSIGN
             bf-rm-rctd.r-no        = iSeqNo
             bf-ttrctd.r-no         = bf-rm-rctd.r-no
             bf-ttrctd.ttRctdHasRec = YES
             bf-ttrctd.rmrctdRowID  = ROWID(bf-rm-rctd)
             .    
    END.
    
    DELETE  ttRctd.
    RELEASE bf-ttrctd.
    RELEASE bf-rm-rdtlh.
    RELEASE bf-rm-rcpth.
    RELEASE bf-rm-rctd.
END PROCEDURE.

PROCEDURE pGLFromWork :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiRun     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiTRNum   AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE dCredits   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dDebits    AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE daPostDate AS DATE      NO-UNDO.
    DEFINE VARIABLE cRMPost    AS CHARACTER NO-UNDO INITIAL "RMPOST".
    
    DEFINE BUFFER bf-work-gl FOR work-gl.
    
    daPostDate = TODAY.
    
    FIND FIRST period
         WHERE period.company EQ ipcCompany
           AND period.pst     LE daPostDate 
           AND period.pend    GE daPostDate 
         NO-LOCK.
    
    FOR EACH bf-work-gl NO-LOCK
        WHERE (ipiRun EQ 1 AND bf-work-gl.job-no NE "")
           OR (ipiRun EQ 2 AND bf-work-gl.job-no EQ "")
        BREAK BY bf-work-gl.actnum:

        ASSIGN
            dDebits  = dDebits  + bf-work-gl.debits
            dCredits = dCredits + bf-work-gl.credits
            .

        IF LAST-OF(bf-work-gl.actnum) THEN DO:
          RUN GL_SpCreateGLHist(ipcCompany,
                             bf-work-gl.actnum,
                             "cRMPost",
                             (IF bf-work-gl.job-no NE "" THEN "RM Issue to Job"
                                                         ELSE "RM Receipt"),
                             daPostDate,
                             (dDebits - dCredits),
                             ipiTRNum,
                             (IF AVAILABLE period THEN period.pnum
                                                  ELSE 0),
                             "A",
                             daPostDate,
                             "",
                             "RM").
            ASSIGN
                dDebits            = 0
                dCredits           = 0
                .
        END.
    END.

END PROCEDURE.

PROCEDURE pCreateWIPInventoryStock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRecValue          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hdInventoryProcs   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cSysCtrlRMIssueWIP AS CHARACTER NO-UNDO INITIAL "RMIssueWIP".
    
    RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    
    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,               /* Company Code */
        INPUT  cSysCtrlRMIssueWIP,       /* sys-ctrl name */
        INPUT  "L",                      /* Output return value I - int-fld, L - log-fld, C - char-fld, D - dec-fld, DT - date-fld */
        INPUT  FALSE,                    /* Use ship-to */
        INPUT  FALSE,                    /* ship-to vendor */
        INPUT  "",                       /* ship-to vendor value */
        INPUT  "",                       /* shi-id value */
        OUTPUT cRecValue,
        OUTPUT lRecFound
        ).
    
    IF NOT lRecFound THEN
        RETURN.
        
    IF NOT LOGICAL(cRecValue) THEN
        RETURN.
            
    IF VALID-HANDLE(hdInventoryProcs) THEN DO:
        FOR EACH ttBoardToWIP:
            RUN Inventory_CreateWIPInventoryStockForIssuedRM IN hdInventoryProcs (
                INPUT  ttBoardToWIP.rmrdtlhRowID,
                INPUT  ttBoardToWIP.rmrcpthRowID,
                INPUT  ttBoardToWIP.rmbinRowID,
                INPUT  ttBoardToWIP.jobmatRowID,
                INPUT  ttBoardToWIP.itemRowID,
                OUTPUT lSuccess,
                OUTPUT cMessage
                ) NO-ERROR.   
        END.
        DELETE PROCEDURE hdInventoryProcs.
    END.
END PROCEDURE.

PROCEDURE pRMPost PRIVATE:
/* ---------------------------------------------------  */
/* Rm Posting - Calculate new average cost for the bin                        */
/* -------------------------------------------------------------------------- */
    DEFINE INPUT        PARAMETER ipdBinQty       AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdBinCost     AS DECIMAL NO-UNDO.
    DEFINE INPUT        PARAMETER ipdReceiptQty   AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdReceiptCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dBinQty      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dReceiptQty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalQty    AS DECIMAL NO-UNDO.

    ASSIGN
        dBinQty     = ipdBinQty 
        dReceiptQty = ipdReceiptQty 
        .

    /* Takes first total cost + 2nd total cost / total qty to get avg. cost */    
    dTotalQty  = dBinQty + dReceiptQty.
   
    /* Case 1 - Transaction is positive and bin is positive */
    IF dReceiptQty GE 0 AND dBinQty GT 0 THEN 
      /* Takes first total cost + 2nd total cost / total qty to get avg. cost */
       iopdBinCost = ((dBinQty * iopdBinCost) + (dReceiptQty * iopdReceiptCost)) / dTotalQty.
    ELSE
    /* Case 2 - Transaction is positive, orig. bin qty is negative */
    /* Take the cost from the new transaction to avoid very large denominator */
    IF dReceiptQty GE 0 AND dBinQty LE 0 THEN 
        iopdBinCost = iopdReceiptCost.
    ELSE
    /* Case 3 - Transaction qty is negative, new bin quantity is positive */
    IF dReceiptQty LT 0 AND dReceiptQty + dBinQty GT 0 THEN 
        iopdBinCost = ((dBinQty * iopdBinCost) + (dReceiptQty * iopdReceiptCost)) / dTotalQty.
    ELSE
    /* Case 4 - Both transaction and bin quantities are negative */
    IF dReceiptQty LT 0 AND dReceiptQty + dBinQty LE 0 THEN 
      /* No change */
    IF iopdBinCost LT 0 THEN
        iopdBinCost  = iopdBinCost * -1.
    /* If total quantity was zero, dBinCost will be ? */
    IF iopdBinCost EQ ? THEN 
        iopdBinCost = 0.

END PROCEDURE.

PROCEDURE pHistory PRIVATE:
/*------------------------------------------------------------------------------
  Purpose:    Creates history records  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
    DEFINE INPUT        PARAMETER ipriRMRctd  AS ROWID     NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE daPostDate AS DATE NO-UNDO.
    
    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    
    daPostDate = TODAY.
    
    FIND FIRST rm-rctd NO-LOCK
         WHERE ROWID(rm-rctd) EQ ipriRMRctd
         NO-ERROR.

    FIND FIRST bf-rm-rcpth NO-LOCK
         WHERE bf-rm-rcpth.r-no EQ rm-rctd.r-no 
         NO-ERROR.
    IF AVAILABLE bf-rm-rcpth THEN DO:
        RUN sys/ref/asiseq.p (
            INPUT bf-rm-rcpth.company, 
            INPUT "rm_rcpt_seq", 
            OUTPUT bf-rm-rcpth.r-no
            ) NO-ERROR.
    END.

    CREATE bf-rm-rcpth.
    BUFFER-COPY rm-rctd EXCEPT rec_key user-id upd-date upd-time TO bf-rm-rcpth
    ASSIGN
        bf-rm-rcpth.trans-date = rm-rctd.rct-date
        bf-rm-rcpth.post-date  = daPostDate
        .

    CREATE bf-rm-rdtlh.
    BUFFER-COPY rm-rctd EXCEPT rec_key user-id upd-date upd-time TO bf-rm-rdtlh.

    IF rm-rctd.rita-code EQ "T" /* Transfers */ THEN DO:
        bf-rm-rdtlh.qty = rm-rctd.qty * -1.
        
        IF bf-rm-rdtlh.tag EQ bf-rm-rdtlh.tag2 THEN
            bf-rm-rdtlh.tag2 = "".
    
        CREATE bf-rm-rdtlh.
        BUFFER-COPY rm-rctd EXCEPT rec_key user-id upd-date upd-time TO bf-rm-rdtlh
        ASSIGN
            bf-rm-rdtlh.loc     = rm-rctd.loc2
            bf-rm-rdtlh.loc-bin = rm-rctd.loc-bin2
            bf-rm-rdtlh.tag     = rm-rctd.tag2
            .
        
        IF bf-rm-rdtlh.tag EQ bf-rm-rdtlh.tag2 THEN
            bf-rm-rdtlh.tag2 = "".
    END.
    
    RELEASE bf-rm-rdtlh.
    RELEASE bf-rm-rcpth.
END PROCEDURE.



    

