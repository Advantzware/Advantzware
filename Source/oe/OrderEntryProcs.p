
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
                    ttInputOrd.cust-no  = eb.cust-no
                    ttInputOrd.ship-id  = eb.ship-id
                    ttInputOrd.sold-id  = eb.cust-no 
                    ttInputOrd.due-code = "On"
                    ttInputOrd.ord-date = TODAY
                    ttInputOrd.due-date = TODAY + cust.ship-days.                    
            END.              
              
            CREATE ttInputOrdLine.
            ASSIGN
                ttInputOrdLine.company   = ipcCompany
                ttInputOrdLine.LINE      = iLine
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
                ttInputOrdLine.part-dscr1 = eb.part-dscr2  . 
             
            ASSIGN 
                ttInputOrd.over-pct  = ttInputOrdLine.over-pct
                ttInputOrd.Under-pct = ttInputOrdLine.Under-pct.
                
            RUN Conv_CalcTotalPrice(ipcCompany,            
                        ttInputOrdLine.i-no,
                        DECIMAL(ttInputOrdLine.qty),
                        DECIMAL(ttInputOrdLine.price),
                        ttInputOrdLine.pr-uom,
                        DECIMAL(ttInputOrdLine.disc),
                        DECIMAL(ttInputOrdLine.cas-cnt),    
                        OUTPUT ttInputOrdLine.t-price).                  
              
        END.
          
    END.
    
    
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

