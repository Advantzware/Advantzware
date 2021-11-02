
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
DEFINE VARIABLE gcCePrep    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcFreightCalculation AS CHARACTER NO-UNDO.
DEFINE VARIABLE glOeReleas  AS LOGICAL NO-UNDO.
DEFINE VARIABLE glOecredit AS LOGICAL NO-UNDO.
 
//DEFINE SHARED VARIABLE nufile         AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHARACTER NO-UNDO.
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.

DEFINE VARIABLE hdOrderProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobProcs     AS HANDLE    NO-UNDO.

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
                    ttInputOrd.csrUser_id = IF est.csrUser_id NE "" THEN est.csrUser_id ELSE cust.csrUser_id
                    dFactor               = IF est.est-type GE 1 AND est.est-type LE 4 THEN gdLastShip
                                           ELSE 1
                    .
                    
                    RUN pGetShiptoTaxCode(ipcCompany, cust.cust-no, eb.ship-id, INPUT-OUTPUT ttInputOrd.tax-gr).
                    
                    IF gcLastShip EQ "Fibre" THEN
                    ASSIGN
                         ttInputOrd.last-date = ttInputOrd.ord-date + (cust.ship-days * dFactor)
                         ttInputOrd.due-date  = ttInputOrd.ord-date + (giLastShip * dFactor).
                    IF AVAIL cust AND NOT cust.internal THEN DO:
                       RUN oe/creditck.p (ROWID(cust), NO).
                       FIND CURRENT cust NO-LOCK NO-ERROR.
                       IF AVAIL cust AND cust.cr-hold AND glOecredit THEN ttInputOrd.stat = "H".  
                    END.     
             
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
                ttInputOrdLine.lCreateRel = glOeReleas
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
                      AND est-prep.orderID EQ ""  NO-LOCK ,
                      FIRST ttEstItem WHERE ttEstItem.isSelected
                            AND ttEstItem.estRowid EQ ROWID(est-prep) NO-LOCK:
                      
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
    DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
    
    RUN oe/OrderProcs.p PERSISTENT SET hdOrderProcs. 
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs. 
    
    
    FIND FIRST ttInputOrd NO-LOCK NO-ERROR.
    
    ASSIGN
    cocode = ttInputOrd.company.
    locode = ttInputOrd.loc.
    
    RUN sys/ref/asiseq.p (INPUT ttInputOrd.company, INPUT "order_seq", OUTPUT iOrderNo) NO-ERROR.
    DO WHILE CAN-FIND(FIRST oe-ord
                    WHERE oe-ord.company EQ ttInputOrd.company
                      AND oe-ord.ord-no  EQ iOrderNo):
    
      RUN sys/ref/asiseq.p (INPUT ttInputOrd.company, 
                          INPUT "order_seq", 
                          OUTPUT iOrderNo) NO-ERROR.        
    END.
    
    IF ttInputOrd.lCreateJob THEN
    DO:
        cJobNo  = FILL(" ",6 - length(TRIM(STRING(iOrderNo)))) + string(iOrderNo).
        iJobNo2 = 0.
        RUN jc/job-no.p (INPUT-OUTPUT cJobNo, INPUT-OUTPUT iJobNo2,INPUT ttInputOrd.procat,
                                INPUT ttInputOrd.est-no).
                                        
        IF cJobNo NE "" THEN 
        ASSIGN
            ttInputOrd.job-no  = cJobNo
            ttInputOrd.job-no2 = iJobNo2 .
    END.        
             
    CREATE bf-oe-ord. 
    ASSIGN
        bf-oe-ord.ord-no   = iOrderNo
        bf-oe-ord.company  = ttInputOrd.company
        bf-oe-ord.loc      = ttInputOrd.loc
        bf-oe-ord.job-no   = IF ttInputOrd.lCreateJob THEN ttInputOrd.job-no ELSE ""
        bf-oe-ord.job-no2  = IF ttInputOrd.lCreateJob THEN ttInputOrd.job-no2 ELSE 0
        oprwRowid          = ROWID(bf-oe-ord)
        ttInputOrd.ord-no  = iOrderNo.
     
    BUFFER-COPY ttInputOrd EXCEPT ord-no company loc rec_key job-no job-no2 TO bf-oe-ord.
         
    FOR EACH ttInputOrdLine NO-LOCK
        WHERE ttInputOrdLine.cItemType EQ "Order Line":
    
        CREATE bf-oe-ordl. 
        ASSIGN
            bf-oe-ordl.ord-no    = iOrderNo
            bf-oe-ordl.company   = ttInputOrd.company
            bf-oe-ordl.job-no    = IF ttInputOrd.lCreateJob THEN ttInputOrd.job-no ELSE ""
            bf-oe-ordl.job-no2    = IF ttInputOrd.lCreateJob THEN ttInputOrd.job-no2 ELSE 0
            bf-oe-ordl.type-code = "O".
            .
        BUFFER-COPY ttInputOrdLine EXCEPT ord-no company rec_key job-no job-no2 type-code TO bf-oe-ordl.        
        
        IF ttInputOrdLine.lCreateRel THEN
        RUN pCreateRelease IN hdOrderProcs (
                        INPUT  bf-oe-ordl.company,
                        INPUT  bf-oe-ordl.ord-no,
                        INPUT  bf-oe-ordl.line,
                        INPUT  bf-oe-ord.ship-id,  /* ShipTo */
                        INPUT  "",                 /* ShipFrom */
                        OUTPUT lSuccess,
                        OUTPUT opcMessage       
                        ).
        
    END.  
           
    FOR EACH ttInputOrdLine NO-LOCK
        WHERE ttInputOrdLine.cItemType EQ "Misc":       
        
        RUN Order_CreateMiscSurcharge IN hdOrderProcs ( 
                                       INPUT bf-oe-ord.company,
                                       INPUT bf-oe-ord.ord-no,
                                       INPUT ttInputOrdLine.i-no,
                                       INPUT 1,
                                       OUTPUT oplError,
                                       OUTPUT opcMessage
                                       ).
    END.  
        
    IF ttInputOrd.lCreateJob AND bf-oe-ord.job-no NE "" THEN
    DO:            
        FIND FIRST job NO-LOCK
             WHERE job.company EQ bf-oe-ord.company
             AND job.job-no  EQ bf-oe-ord.job-no
             AND job.job-no2 EQ bf-oe-ord.job-no2
             NO-ERROR.
        IF NOT AVAILABLE job THEN
        DO:
            RUN job_CreateJob IN hdJobProcs(ROWID(bf-oe-ord)).
           
        END.                
        
    END. 
           
    RELEASE bf-oe-ordl.
    FOR EACH bf-oe-ordl OF bf-oe-ord  :
    
        IF (gcFreightCalculation EQ "ALL" OR gcFreightCalculation EQ "Order processing") THEN 
        DO:
            RUN oe/ordlfrat.p (ROWID(bf-oe-ordl), OUTPUT bf-oe-ordl.t-freight).
            bf-oe-ord.t-freight = bf-oe-ord.t-freight + bf-oe-ordl.t-freight.
        END. /* Freight calculation */
                
        IF bf-oe-ordl.job-no NE "" THEN
            RUN oe/palchk.p(ROWID(bf-oe-ord), bf-oe-ordl.i-no).
             
        fil_id = RECID(bf-oe-ordl).
        //RUN oe/ordlup.p.         /* Update Inventory and Job Costing */
    END.
    
    IF (gcFreightCalculation EQ "ALL" OR gcFreightCalculation EQ "Order processing") THEN
    RUN oe/ordfrate.p (ROWID(bf-oe-ord)). /* strange problem with freight */
               
    RUN oe/calcordt.p (ROWID(bf-oe-ord)). 
                 
    EMPTY TEMP-TABLE ttInputOrd.
    EMPTY TEMP-TABLE ttInputOrdLine.
    DELETE OBJECT hdOrderProcs.
    DELETE OBJECT hdJobProcs.
    
END.

PROCEDURE pGetShiptoTaxCode PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the order evaluation date
     Notes:
    ------------------------------------------------------------------------------*/ 
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipto    AS CHARACTER NO-UNDO.    
    DEFINE INPUT-OUTPUT PARAMETER iopcTaxCode  AS CHARACTER NO-UNDO.
    
    FIND FIRST shipto NO-LOCK
         WHERE shipto.company EQ ipcCompany
         AND shipto.cust-no EQ ipcCustomer
         AND shipto.ship-id EQ ipcShipto NO-ERROR. 
         
    IF AVAIL shipto AND shipto.tax-code NE "" THEN
        iopcTaxCode = shipto.tax-code .     
    
END PROCEDURE.    

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
     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "CEPREP", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     gcCePrep = cReturnChar NO-ERROR. 
          
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "FreightCalculation", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     gcFreightCalculation = cReturnChar NO-ERROR.
     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "OERELEAS", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     glOeReleas = LOGICAL(cReturnChar) NO-ERROR. 
     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "OERELEAS", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     glOecredit = LOGICAL(cReturnChar) NO-ERROR.
     
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

