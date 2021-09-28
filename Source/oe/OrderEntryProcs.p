
/*------------------------------------------------------------------------
    File        : OrderEntryProcs.p
    Purpose     : Centralization of numerous common functions in the import and manual entry of orders

    Syntax      :

    Description : Holds procedures for entering, editing and processing orders

    Author(s)   : Sewa Singh
    Created     : Fri Set 03 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{oe\ttInputOrd.i}

DEFINE VARIABLE glFoamDate  AS LOGICAL NO-UNDO. 
DEFINE VARIABLE giFoamDate  AS INTEGER NO-UNDO. 
DEFINE VARIABLE gcLastShip  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gdLastShip  AS DECIMAL NO-UNDO.
DEFINE VARIABLE giLastShip  AS INTEGER NO-UNDO.
DEFINE VARIABLE gcCePrepPrice AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE nufile         AS LOG       NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetNk1Job RETURNS LOGICAL 
    (ipcCompany AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */


PROCEDURE OrderEntry_GetEstDetail:
    /*------------------------------------------------------------------------------
     Purpose: Returns the order evaluation parameters
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE INPUT PARAMETER TABLE FOR ttEstItem.
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimate  AS CHARACTER NO-UNDO.      
    DEFINE OUTPUT PARAMETER TABLE FOR ttInputOrdLine.
    DEFINE OUTPUT PARAMETER TABLE FOR ttInputOrd.
    
    RUN pGetEstDetail (
        INPUT TABLE ttEstItem, 
        INPUT  ipcCompany,
        INPUT  ipcEstimate,
        OUTPUT TABLE ttInputOrdLine,
        OUTPUT TABLE ttInputOrd
        ).
END.

PROCEDURE OrderEntry_SaveData:
    /*------------------------------------------------------------------------------
     Purpose: Returns the order evaluation parameters
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE INPUT PARAMETER TABLE FOR ttInputOrd.
    DEFINE INPUT PARAMETER TABLE FOR ttInputOrdLine.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oprwRowid AS ROWID NO-UNDO.
    
    RUN pSaveData (
        INPUT TABLE ttInputOrd,
        INPUT TABLE ttInputOrdLine,
        OUTPUT  oplError,
        OUTPUT  opcMessage,
        OUTPUT  oprwRowid
        ).
END.


PROCEDURE pGetEstDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the order evaluation date
     Notes:
    ------------------------------------------------------------------------------*/ 
    DEFINE INPUT PARAMETER TABLE FOR ttEstItem.
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimate  AS CHARACTER NO-UNDO.      
    DEFINE OUTPUT PARAMETER TABLE FOR ttInputOrdLine.
    DEFINE OUTPUT PARAMETER TABLE FOR ttInputOrd.
    
    DEFINE VARIABLE iLine    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.
    
    DEF VAR li-cnt AS INT NO-UNDO.
    DEF VAR lCreateJob AS LOG NO-UNDO.
    DEF VAR li-cases AS INT NO-UNDO.
    DEFINE VARIABLE dFactor AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER b-eb FOR eb.
    
    RUN pSetGlobalSettings (INPUT ipcCompany).
        
    ipcEstimate = FILL(" ",8 - LENGTH(TRIM(ipcEstimate))) + TRIM(ipcEstimate).
    
    FIND FIRST est NO-LOCK
        WHERE est.company EQ ipcCompany
        AND est.est-no  EQ ipcEstimate
        NO-ERROR.
         
    EMPTY TEMP-TABLE ttInputOrd .
    
    IF AVAILABLE est THEN
    DO:            
        EMPTY TEMP-TABLE ttInputOrdLine .
        iLine = 0.     
           
        FOR EACH eb NO-LOCK
            WHERE eb.company EQ ipcCompany
            AND eb.est-no EQ est.est-no,
            FIRST ttEstItem WHERE ttEstItem.isSelected
            AND ttEstItem.estRowid EQ ROWID(eb) NO-LOCK,
            FIRST cust NO-LOCK
            WHERE (cust.company = ipcCompany)
            AND cust.cust-no EQ eb.cust-no
            USE-INDEX cust
            BREAK BY eb.est-no BY eb.cust-no BY eb.form-no BY eb.blank-no:    
            
            IF est.est-type EQ 2 OR est.est-type EQ 6 THEN DO:
                 lCreateJob = CAN-FIND(FIRST b-eb WHERE b-eb.company EQ eb.company
                                         AND b-eb.est-no  EQ eb.est-no
                                         AND b-eb.pur-man EQ NO
                                         AND b-eb.form-no NE 0).
            FIND FIRST b-eb
                 WHERE b-eb.company EQ est.company
                 AND b-eb.est-no  EQ est.est-no
                 AND b-eb.form-no NE 0
               NO-LOCK NO-ERROR.
            END.

            ELSE DO:
                FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) NO-LOCK NO-ERROR.
                lCreateJob = NOT b-eb.pur-man.
            END.
            IF NOT lCreateJob THEN lCreateJob = fGetNk1Job(ipcCompany).
                    
            iLine = iLine + 1.               
                  
            IF FIRST(eb.est-no) THEN
            DO:
                CREATE ttInputOrd.
                ASSIGN
                    ttInputOrd.company  = ipcCompany
                    ttInputOrd.user-id  = USERID(LDBNAME(1))
                    ttInputOrd.cust-no  = eb.cust-no
                    ttInputOrd.ship-id  = eb.ship-id
                    ttInputOrd.sold-id  = eb.cust-no 
                    ttInputOrd.due-code = "On"
                    ttInputOrd.ord-date = TODAY
                    ttInputOrd.due-date = TODAY + cust.ship-days
                    ttInputOrd.est-no    = eb.est-no
                    ttInputOrd.TYPE      = "O"                                      
                    ttInputOrd.sman[1]   = eb.sman                     
                    ttInputOrd.carrier   = eb.carrier
                    ttInputOrd.frt-pay   = eb.chg-method
                    ttInputOrd.s-comm[1] = eb.comm   
                    ttInputOrd.procat    = eb.procat
                    ttInputOrd.s-pct[1]  = 100
                    ttInputOrd.est-type  = est.est-type                      
                    ttInputOrd.cust-name = cust.NAME
                    ttInputOrd.addr[1]   = cust.addr[1]
                    ttInputOrd.addr[2]   = cust.addr[2]
                    ttInputOrd.city      = cust.city
                    ttInputOrd.state     = cust.state
                    ttInputOrd.zip       = cust.zip
                    ttInputOrd.contact   = cust.contact
                    ttInputOrd.last-date = ttInputOrd.ord-date + cust.ship-days                     
                    ttInputOrd.terms     = cust.terms            
                    ttInputOrd.fob-code  = cust.fob-code
                    //ttInputOrd.f-bill    = (oe-ord.frt-pay EQ "B")
                    ttInputOrd.tax-gr    = cust.tax-gr 
                    dFactor             = IF est.est-type GE 1 AND est.est-type LE 4 THEN gdLastShip
                                           ELSE 1
                    .
                    
                    IF gcLastShip EQ "Fibre" THEN
                    ASSIGN
                         ttInputOrd.last-date = ttInputOrd.ord-date + (cust.ship-days * dFactor)
                         ttInputOrd.due-date  = ttInputOrd.ord-date + (giLastShip * dFactor).
             
            END.              
              
            CREATE ttInputOrdLine.
            ASSIGN
                ttInputOrdLine.company   = ipcCompany
                ttInputOrdLine.LINE      = ttEstItem.estLine
                ttInputOrdLine.cItemType = "Order Line"
                ttInputOrdLine.est-no    = eb.est-no
                ttInputOrdLine.i-no      = eb.stock-no
                ttInputOrdLine.part-no   = eb.part-no 
                ttInputOrdLine.po-no     = ttEstItem.estPo
                ttInputOrdLine.qty       = ttEstItem.estQty
                ttInputOrdLine.cQtyUom   = ttEstItem.estQtyUom 
                ttInputOrdLine.price     = ttEstItem.estPrice
                ttInputOrdLine.pr-uom    = ttEstItem.estPrUom
                ttInputOrdLine.req-date  = TODAY + cust.ship-days 
                ttInputOrdLine.lCreateJob = lCreateJob
                ttInputOrdLine.est-type   = eb.est-type
                ttInputOrdLine.form-no    = eb.form-no
                ttInputOrdLine.blank-no   = eb.blank-no
                ttInputOrdLine.cust-no    = eb.cust-no                
                .
                
             RUN est/getcscnt.p ( ROWID(eb),
                         OUTPUT li-cnt,OUTPUT li-cases).
                         
             ASSIGN   
                ttInputOrdLine.cas-cnt    = li-cnt
                ttInputOrdLine.cases-unit = li-cases
                ttInputOrdLine.s-pct[1]   = 100
                ttInputOrdLine.s-man[1]  = b-eb.sman
                ttInputOrdLine.s-comm[1] = b-eb.comm . 
           
           IF glFoamDate AND
               CAN-FIND(FIRST style WHERE style.company EQ b-eb.company
                                      AND style.style   EQ b-eb.style
                                      AND style.type    EQ "F") THEN DO:
               ttInputOrdLine.req-date = ttInputOrd.ord-date + giFoamDate.
            
               IF DATE(ttInputOrdLine.req-date)  GT
                  DATE(ttInputOrdLine.prom-date) THEN
                 ttInputOrdLine.prom-date = ttInputOrdLine.req-date.
           END.
           
           IF gcLastShip = "Stock/Custom" THEN DO:
              ASSIGN ttInputOrdLine.req-date = TODAY + INT(gdLastShip).              
              ASSIGN ttInputOrdLine.prom-date = oe-ordl.req-date.
           END.
                       
            RUN pGetOverUnderPct(INPUT ipcCompany, INPUT eb.cust-no, INPUT eb.ship-id, INPUT eb.stock-no,
                OUTPUT ttInputOrdLine.over-pct, OUTPUT ttInputOrdLine.Under-pct, OUTPUT ttInputOrdLine.cOverUnderTagDesc).        
                  
            FIND FIRST itemfg NO-LOCK 
                WHERE itemfg.company EQ eb.company
                AND itemfg.i-no    EQ eb.stock-no
                NO-ERROR.
     
            IF AVAILABLE itemfg THEN 
            DO:
                RUN Tax_GetTaxableAR  (ipcCompany, eb.cust-no, eb.ship-id, itemfg.i-no, OUTPUT lTaxable).
               
                ASSIGN
                    ttInputOrdLine.i-name     = itemfg.i-name
                    ttInputOrdLine.part-dscr2 = itemfg.part-dscr2
                    ttInputOrdLine.part-dscr3 = itemfg.part-dscr3
                    ttInputOrdLine.tax        = lTaxable                     
                    . 
            END. 
            ASSIGN
                ttInputOrdLine.i-name     = eb.part-dscr1 
                ttInputOrdLine.part-dscr1 = eb.part-dscr2 
                ttInputOrdLine.disc       = IF AVAIL itemfg AND itemfg.exempt-disc THEN 0 ELSE cust.disc. 
             
            ASSIGN 
                ttInputOrd.over-pct   = ttInputOrdLine.over-pct
                ttInputOrd.Under-pct  = ttInputOrdLine.Under-pct
                ttInputOrd.lCreateRel = ttInputOrdLine.lCreateRel
                ttInputOrd.lCreateJob = ttInputOrdLine.lCreateJob
                ttInputOrd.lCreatePo  = ttInputOrdLine.lCreatePo .
                
            RUN Conv_CalcTotalPrice(ipcCompany,            
                        ttInputOrdLine.i-no,
                        DECIMAL(ttInputOrdLine.qty),
                        DECIMAL(ttInputOrdLine.price),
                        ttInputOrdLine.pr-uom,
                        DECIMAL(ttInputOrdLine.disc),
                        DECIMAL(ttInputOrdLine.cas-cnt),    
                        OUTPUT ttInputOrdLine.t-price).                
        END.
        FOR EACH est-prep WHERE est-prep.company EQ ipcCompany
                      AND est-prep.est-no EQ est.est-no
                      AND est-prep.simon EQ "S" 
                      AND est-prep.orderID EQ ""  NO-LOCK :
                      
                      FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ ipcCompany NO-LOCK NO-ERROR.
                      FIND FIRST prep NO-LOCK
                           WHERE prep.company EQ ipcCompany 
                           AND prep.code = est-prep.CODE NO-ERROR.
                           
                      iLine = iLine + 1.    
                      CREATE ttInputOrdLine.
                      ASSIGN
                      ttInputOrdLine.cItemType = "Misc"
                      ttInputOrdLine.company = ipcCompany                           
                      ttInputOrdLine.line = iLine
                      ttInputOrdLine.i-no = est-prep.code
                      ttInputOrdLine.i-name = IF est-prep.dscr <> "" THEN est-prep.dscr ELSE prep.dscr
                      ttInputOrdLine.cAccount = IF AVAIL prep AND prep.actnum <> "" THEN prep.actnum ELSE ar-ctrl.sales
                      ttInputOrdLine.dAmount = IF gcCePrepPrice EQ "Profit" THEN
                                               (est-prep.cost * est-prep.qty) / (1 - (est-prep.mkup / 100)) * 
                                               (est-prep.amtz / 100)
                                               ELSE
                                               (est-prep.cost * est-prep.qty) * (1 + (est-prep.mkup / 100)) * 
                                               (est-prep.amtz / 100)
                      ttInputOrdLine.qty    = est-prep.qty                         
                      ttInputOrdLine.est-no = est-prep.est-no  
                      ttInputOrdLine.price  = est-prep.cost
                      ttInputOrdLine.cost = (est-prep.cost * est-prep.qty * (est-prep.amtz / 100))
                      ttInputOrdLine.cBill  = "Y"
                      ttInputOrdLine.form-no = est-prep.s-num 
                      ttInputOrdLine.blank-no = est-prep.b-num 
                      ttInputOrdLine.cQtyUom  = "EA"
                      ttInputOrdLine.pr-uom  = "EA".
                      RUN Tax_GetTaxableAR  (ipcCompany, ttInputOrd.cust-no, ttInputOrd.ship-id, "", OUTPUT lTaxable).
                      ttInputOrdLine.tax = lTaxable.
                
        END.       
          
    END.
    
    
END PROCEDURE.

PROCEDURE pCreateJob PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the order evaluation parameters
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.
    
    //DEFINE OUTPUT PARAMETER op-recid AS RECID NO-UNDO.

    DEFINE BUFFER v-ord-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-oe-ord FOR oe-ord.

    DEFINE VARIABLE v-job-job LIKE job.job NO-UNDO.
    DEFINE VARIABLE v-job-no  LIKE job.job-no NO-UNDO.
    DEFINE VARIABLE v-job-no2 LIKE job.job-no2 NO-UNDO.
    DEFINE VARIABLE li-j-no   AS INTEGER NO-UNDO.  
    
    FIND FIRST bf-oe-ord WHERE ROWID(bf-oe-ord) EQ iprwRowid NO-LOCK NO-ERROR.
              
    FIND LAST job WHERE job.company EQ bf-oe-ord.company NO-LOCK NO-ERROR.
    v-job-job = IF AVAILABLE job THEN job.job + 1 ELSE 1.
    ASSIGN
        v-job-no  = bf-oe-ord.job-no
        v-job-no2 = bf-oe-ord.job-no2.

    FOR EACH job
        WHERE job.company EQ bf-oe-ord.company
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2:
        DELETE job.
    END.
         
    CREATE job.
    ASSIGN 
        job.job       = v-job-job
        job.company   = bf-oe-ord.company
        job.loc       = bf-oe-ord.loc
        job.est-no    = bf-oe-ord.est-no
        job.job-no    = v-job-no
        job.job-no2   = v-job-no2
        job.stat      = "P"
        job.ordertype = bf-oe-ord.type
        //op-recid      = RECID(job)
        .

    FOR EACH oe-ordl WHERE oe-ordl.company EQ bf-oe-ord.company
        AND oe-ordl.ord-no  EQ bf-oe-ord.ord-no exclusive:
        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ bf-oe-ord.company
            AND job-hdr.job-no  EQ bf-oe-ord.job-no
            AND job-hdr.job-no2 EQ bf-oe-ord.job-no2
            AND job-hdr.ord-no  EQ bf-oe-ord.ord-no
            AND job-hdr.i-no    EQ oe-ordl.i-no
            NO-ERROR.

        IF NOT AVAILABLE job-hdr THEN 
        DO:
            FIND FIRST itemfg WHERE itemfg.company EQ oe-ordl.company
                AND itemfg.i-no    EQ oe-ordl.i-no
                NO-LOCK NO-ERROR.   

            CREATE job-hdr.
            ASSIGN 
                job-hdr.company  = bf-oe-ord.company
                job-hdr.loc      = bf-oe-ord.loc
                job-hdr.est-no   = bf-oe-ord.est-no
                job-hdr.i-no     = oe-ordl.i-no
                job-hdr.qty      = oe-ordl.qty 
                job-hdr.cust-no  = oe-ordl.cust-no
                job-hdr.ord-no   = oe-ordl.ord-no
                job-hdr.po-no    = oe-ordl.po-no
                job-hdr.blank-no = oe-ordl.blank-no.

            IF AVAILABLE itemfg THEN
                ASSIGN job-hdr.std-mat-cost = itemfg.std-mat-cost
                    job-hdr.std-lab-cost = itemfg.std-lab-cost
                    job-hdr.std-var-cost = itemfg.std-var-cost
                    job-hdr.std-fix-cost = itemfg.std-fix-cost.

            ASSIGN 
                job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
        END.

        ELSE
        DO WHILE TRUE:
            FIND v-ord-job-hdr WHERE ROWID(v-ord-job-hdr) EQ ROWID(job-hdr)
            EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAILABLE v-ord-job-hdr THEN 
            DO:
                FIND CURRENT v-ord-job-hdr NO-LOCK NO-ERROR.
                FIND CURRENT job-hdr NO-ERROR.
                LEAVE.
            END.
        END.

        ASSIGN 
            job-hdr.est-no  = bf-oe-ord.est-no
            job-hdr.job     = job.job
            job-hdr.job-no  = job.job-no
            job-hdr.job-no2 = job.job-no2
            oe-ordl.est-no  = job-hdr.est-no
            oe-ordl.job-no  = job-hdr.job-no
            oe-ordl.job-no2 = job-hdr.job-no2
            oe-ordl.j-no    = job-hdr.j-no.

        FIND CURRENT job-hdr NO-LOCK.
    END.
    
    FIND CURRENT job NO-LOCK.   
       
END PROCEDURE.

PROCEDURE pCreateOeOrdm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the order evaluation parameters
     Notes:
    ------------------------------------------------------------------------------*/ 
    DEFINE PARAMETER BUFFER bf-ttInputOrd FOR ttInputOrd.
    DEFINE PARAMETER BUFFER bf-ttInputOrdLine FOR ttInputOrdLine.
    
    DEFINE VARIABLE li-line  AS INTEGER   NO-UNDO.
    DEFINE BUFFER bf-ordm FOR oe-ordm.
    
    FIND LAST bf-ordm NO-LOCK
        WHERE bf-ordm.company EQ bf-ttInputOrd.company
        AND bf-ordm.ord-no  EQ bf-ttInputOrd.ord-no
        USE-INDEX oe-misc NO-ERROR.
    li-line = IF AVAILABLE bf-ordm THEN bf-ordm.line ELSE 0.
  
    CREATE oe-ordm.    
    ASSIGN
        oe-ordm.company   = bf-ttInputOrd.company
        oe-ordm.ord-no    = bf-ttInputOrd.ord-no
        oe-ordm.est-no    = bf-ttInputOrd.est-no
        oe-ordm.line      = li-line + 1
        oe-ordm.bill      = "Y"
        oe-ordm.s-man[1]  = bf-ttInputOrd.sman[1]
        oe-ordm.s-pct[1]  = bf-ttInputOrd.s-pct[1]
        oe-ordm.s-comm[1] = bf-ttInputOrd.s-comm[1]
        oe-ordm.s-man[2]  = bf-ttInputOrd.sman[2]
        oe-ordm.s-pct[2]  = bf-ttInputOrd.s-pct[2]
        oe-ordm.s-comm[2] = bf-ttInputOrd.s-comm[2]
        oe-ordm.s-man[3]  = bf-ttInputOrd.sman[3]
        oe-ordm.s-pct[3]  = bf-ttInputOrd.s-pct[3]
        oe-ordm.s-comm[3] = bf-ttInputOrd.s-comm[3]
        
        .

    FIND FIRST ar-ctrl WHERE ar-ctrl.company = bf-ttInputOrd.company NO-LOCK NO-ERROR.
    IF AVAILABLE ar-ctrl THEN oe-ordm.actnum = ar-ctrl.sales.
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ bf-ttInputOrd.company
         AND cust.cust-no EQ bf-ttInputOrd.cust-no NO-ERROR.

    FIND FIRST prep NO-LOCK 
            WHERE prep.company EQ bf-ttInputOrd.company 
            AND prep.code    EQ bf-ttInputOrdLine.i-no
            NO-ERROR.
    IF AVAILABLE prep AND NOT prep.commissionable THEN
        ASSIGN 
            oe-ordm.s-comm[1] = 0 
            oe-ordm.s-comm[2] = 0
            oe-ordm.s-comm[3] = 0
            .
    FIND FIRST est-prep NO-LOCK
         where est-prep.company eq bf-ttInputOrd.company
          and est-prep.est-no  eq bf-ttInputOrdLine.est-no
          and est-prep.s-num   eq bf-ttInputOrdLine.form-no
          and (est-prep.b-num  eq bf-ttInputOrdLine.blank-no or est-prep.b-num eq 0)
          and est-prep.simon   eq "S" 
          AND est-prep.orderID EQ ""
        NO-ERROR.        
    
    IF AVAIL est-prep THEN
    DO:
       ASSIGN
          oe-ordm.dscr = est-prep.dscr
          oe-ordm.cost = (est-prep.cost * est-prep.qty * (est-prep.amtz / 100))
          oe-ordm.estPrepLine = est-prep.LINE 
          oe-ordm.form-no  = est-prep.s-num
          oe-ordm.blank-no = est-prep.b-num 
          oe-ordm.estPrepEqty   = est-prep.eqty .
          
          /*IF ceprepprice-chr EQ "Profit" THEN
              oe-ordm.amt  = (est-prep.cost * est-prep.qty) / (1 - (est-prep.mkup / 100)) *
                             (est-prep.amtz / 100).
          ELSE
            oe-ordm.amt  = (est-prep.cost * est-prep.qty) * (1 + (est-prep.mkup / 100)) *
                           (est-prep.amtz / 100).
                           
          IF ceprep-cha EQ "Dollar" THEN DO:
             {sys/inc/roundup.i oe-ordm.amt}
          END.
          
          IF ceprep-cha EQ "FiveDollar" THEN DO:
             {sys/inc/roundupfive.i oe-ordm.amt}
          END.         */        
    END.
    ASSIGN oe-ordm.spare-char-1 = bf-ttInputOrd.tax-gr
           /*oe-ordm.tax          = fGetTaxableMisc(bf-ttInputOrd.company, bf-ttInputOrd.cust-no, bf-ttInputOrd.ship-id, bf-ttInputOrdLine.i-no)*/ .      
  
    /*IF i = 1 THEN 
    DO:
        IF AVAILABLE oe-ord THEN
            FIND FIRST bf-ordl OF oe-ord NO-LOCK NO-ERROR.
        IF AVAILABLE bf-ordl THEN
            ASSIGN
                oe-ordm.spare-char-2 = bf-ordl.i-no 
                oe-ordm.ord-i-no     = bf-ordl.job-no
                oe-ordm.ord-line     = bf-ordl.job-no2
                oe-ordm.spare-int-1  = bf-ordl.LINE .
    END.
    ELSE 
    DO:
        ASSIGN
            oe-ordm.spare-char-2 = v-fgitem .
        IF AVAILABLE oe-ord THEN
            FIND FIRST bf-ordl OF oe-ord 
                WHERE bf-ordl.i-no EQ v-fgitem NO-LOCK NO-ERROR.
        IF AVAILABLE bf-ordl THEN
            ASSIGN
                oe-ordm.ord-i-no = bf-ordl.job-no
                oe-ordm.ord-line = bf-ordl.job-no2
                oe-ordm.spare-int-1 = bf-ordl.LINE  .
        
    END.  */
    
    //est-prep.orderID = string(oe-ord.ord-no).
    FIND CURRENT oe-ordm NO-LOCK NO-ERROR.  
    
    

END PROCEDURE.    

PROCEDURE pSaveData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the order evaluation parameters
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE INPUT PARAMETER TABLE FOR ttInputOrd.
    DEFINE INPUT PARAMETER TABLE FOR ttInputOrdLine.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oprwRowid AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE VARIABLE iOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE cJobNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2 AS INTEGER NO-UNDO.
    
    FIND FIRST ttInputOrd NO-LOCK NO-ERROR.
            
    RUN sys/ref/asiseq.p (INPUT ttInputOrd.company, INPUT "order_seq", OUTPUT iOrderNo) NO-ERROR.
    DO WHILE CAN-FIND(FIRST oe-ord
                    WHERE oe-ord.company EQ ttInputOrd.company
                      AND oe-ord.ord-no  EQ iOrderNo):
    
      RUN sys/ref/asiseq.p (INPUT ttInputOrd.company, 
                          INPUT "order_seq", 
                          OUTPUT iOrderNo) NO-ERROR.        
    END.
         
    CREATE bf-oe-ord. 
    ASSIGN
        bf-oe-ord.ord-no   = iOrderNo
        bf-oe-ord.company  = ttInputOrd.company
        bf-oe-ord.loc      = ttInputOrd.loc
        bf-oe-ord.job-no   = IF ttInputOrd.lCreateJob THEN string(iOrderNo) ELSE ""
        oprwRowid          = ROWID(bf-oe-ord).
     
    BUFFER-COPY ttInputOrd EXCEPT ord-no company loc rec_key job-no TO bf-oe-ord.
         
    FOR EACH ttInputOrdLine NO-LOCK
        WHERE ttInputOrdLine.cItemType EQ "Order Line":
    
        CREATE bf-oe-ordl. 
        ASSIGN
            bf-oe-ordl.ord-no    = iOrderNo
            bf-oe-ordl.company   = ttInputOrd.company
            bf-oe-ordl.job-no    = IF ttInputOrd.lCreateJob THEN string(iOrderNo) ELSE ""
            bf-oe-ordl.type-code = "O".
            .
        BUFFER-COPY ttInputOrdLine EXCEPT ord-no company rec_key job-no job-no2 type-code TO bf-oe-ordl.
        
    END.  
    
    FOR EACH ttInputOrdLine NO-LOCK
        WHERE ttInputOrdLine.cItemType EQ "Misc":
    
        /*CREATE bf-oe-ordl. 
        ASSIGN
            bf-oe-ordl.ord-no    = iOrderNo
            bf-oe-ordl.company   = ttInputOrd.company
            bf-oe-ordl.job-no    = IF ttInputOrd.lCreateJob THEN string(iOrderNo) ELSE ""
            bf-oe-ordl.type-code = "O".
            .
        BUFFER-COPY ttInputOrdLine EXCEPT ord-no company rec_key job-no job-no2 type-code TO bf-oe-ordl. */
        RUN pCreateOeOrdm( BUFFER ttInputOrd, BUFFER ttInputOrdLine).
        
    END.  
    
    
    
    IF ttInputOrd.lCreateJob AND bf-oe-ord.job-no NE "" THEN
    DO:
        cJobNo  = FILL(" ",6 - length(TRIM(STRING(iOrderNo)))) + string(iOrderNo).
        iJobNo2 = 0.
        RUN jc/job-no.p (INPUT-OUTPUT cJobNo, INPUT-OUTPUT iJobNo2,INPUT ttInputOrd.procat,
                                INPUT ttInputOrd.est-no).
                                        
        IF cJobNo NE "" THEN 
        ASSIGN
            bf-oe-ord.job-no  = cJobNo
            bf-oe-ord.job-no2 = iJobNo2 .                                         
     
        FIND FIRST job NO-LOCK
             WHERE job.company EQ bf-oe-ord.company
             AND job.job-no  EQ bf-oe-ord.job-no
             AND job.job-no2 EQ bf-oe-ord.job-no2
             NO-ERROR.
        IF NOT AVAILABLE job THEN
        DO:
           RUN pCreateJob( ROWID(bf-oe-ord)) . 
        END.                
        
    END.
    
    EMPTY TEMP-TABLE ttInputOrd.
    EMPTY TEMP-TABLE ttInputOrdLine.
    
END.

PROCEDURE pGetOverUnderPct PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the order evaluation date
     Notes:
    ------------------------------------------------------------------------------*/ 
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipto    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItem    AS CHARACTER NO-UNDO.   
    DEFINE OUTPUT PARAMETER opdOverPer  AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdUnderPer AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTagDesc  AS CHARACTER NO-UNDO.
  
    RUN oe/GetOverUnderPct.p(ipcCompany,
        ipcCustomer ,
        ipcShipto,
        ipcFGItem, /* FG Item*/
        0,
        OUTPUT opdOverPer , OUTPUT opdUnderPer, OUTPUT opcTagDesc ) .

END PROCEDURE.

PROCEDURE pSetGlobalSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnChar AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound   AS LOGICAL NO-UNDO.          
   
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "FOAMDATE", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     glFoamDate = logical(cReturnChar) NO-ERROR.         
     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "FOAMDATE", "I" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     giFoamDate = integer(cReturnChar) NO-ERROR. 
     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "LASTSHIP", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     gcLastShip = STRING(cReturnChar) NO-ERROR.
     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "LASTSHIP", "D" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     gdLastShip = DECIMAL(cReturnChar) NO-ERROR. 
     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "LASTSHIP", "I" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     giLastShip = INTEGER(cReturnChar) NO-ERROR. 
     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "CEPREPPRICE", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     gcCePrepPrice = cReturnChar NO-ERROR. 
          
      
END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION fGetNk1Job RETURNS LOGICAL 
    (INPUT ipcCompany AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the NK1 OEAutoApproval log field value
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lReturnValue    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cResult         AS CHARACTER NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,
        INPUT  "JOB#", 
        INPUT  "I", 
        INPUT  YES,                 /* use shipto */ 
        INPUT  YES,                 /* use cust*/ 
        INPUT  "", 
        INPUT  "", 
        OUTPUT cResult, 
        OUTPUT lFound
        ).
    IF lFound THEN
        lReturnValue = IF integer(cResult) EQ 0 THEN TRUE ELSE FALSE NO-ERROR.   
       
    RETURN lReturnValue.     
END FUNCTION.

