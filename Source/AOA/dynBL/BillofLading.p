/*------------------------------------------------------------------------
  File:         AOA/dynBL/BillofLading.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 07.28.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttBillofLading
DEFINE TEMP-TABLE ttBillofLading NO-UNDO
    FIELD bol-no        AS INTEGER   FORMAT ">>>>>>>9" LABEL "Bol#"
    FIELD ord-no        AS INTEGER   FORMAT ">>>>>>>9" LABEL "Order#"
    FIELD po-no         AS CHARACTER FORMAT "x(15)" LABEL "Customer PO"
    FIELD cust-no       AS CHARACTER FORMAT "x(8)" LABEL "Cust #"
    FIELD cust-name     AS CHARACTER FORMAT "x(30)" LABEL "Cust Name"
    FIELD cust-part     AS CHARACTER FORMAT "x(15)" LABEL "Cust Part#"
    FIELD i-no          AS CHARACTER FORMAT "x(15)" LABEL "FG Item#"
    FIELD i-name        AS CHARACTER FORMAT "x(30)" LABEL "FG Item Name"
    FIELD ship-id       AS CHARACTER FORMAT "x(8)" LABEL "Ship To"
    FIELD bol-date      AS DATE      FORMAT "99/99/9999" LABEL "Bol Date"         
    FIELD rel-no        AS INTEGER   FORMAT ">>>>>9" LABEL "Release"
    FIELD stat          AS CHARACTER FORMAT "x(1)" LABEL "Bol Status"
    FIELD carrier       AS CHARACTER FORMAT "x(10)" LABEL "Carrier"
    FIELD trailer       AS CHARACTER FORMAT "x(20)" LABEL "Trailer#"
    FIELD frt-pay       AS CHARACTER FORMAT "x(20)" LABEL "Freight Terms"
    FIELD airway-bill   AS CHARACTER FORMAT "x(20)" LABEL "Seal#"
    FIELD freight-cost  AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Freight Cost"
    FIELD cwt           AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Rate/100 Wt"
    FIELD tot-wt        AS INTEGER   FORMAT ">>>>9" LABEL "Total Weight"
    FIELD tot-pallets   AS INTEGER   FORMAT ">,>>9" LABEL "Total Pallets"
    FIELD user-id       AS CHARACTER FORMAT "x(10)" LABEL "Added/Updated By"    
    FIELD upd-date      AS DATE      FORMAT "99/99/9999" LABEL "Upd Date"
    FIELD cust-addr1    AS CHARACTER FORMAT "x(30)" LABEL "Cust Addr1"
    FIELD cust-addr2    AS CHARACTER FORMAT "x(30)" LABEL "Cust Addr2"
    FIELD cust-city     AS CHARACTER FORMAT "x(20)" LABEL "Cust city"
    FIELD cust-state    AS CHARACTER FORMAT "x(2)" LABEL "Cust State"   
    FIELD cust-zip      AS CHARACTER FORMAT "x(20)" LABEL "Cust Zip"
    FIELD ship-name     AS CHARACTER FORMAT "x(30)" LABEL "Ship Name"
    FIELD ship-addr1    AS CHARACTER FORMAT "x(30)" LABEL "Ship Addr1"
    FIELD ship-addr2    AS CHARACTER FORMAT "x(30)" LABEL "Ship Addr2"
    FIELD ship-city     AS CHARACTER FORMAT "x(20)" LABEL "Ship City"
    FIELD ship-state    AS CHARACTER FORMAT "x(2)" LABEL "Ship State"
    FIELD ship-zip      AS CHARACTER FORMAT "x(20)" LABEL "Ship Zip"
    FIELD tag           AS CHARACTER FORMAT "x(20)" LABEL "Tag"
    FIELD loc           AS CHARACTER FORMAT "x(8)" LABEL "Whse"
    FIELD loc-bin       AS CHARACTER FORMAT "x(8)" LABEL "Bin"
    FIELD job-no        AS CHARACTER FORMAT "x(6)" LABEL "Job NO"
    FIELD job-no2       AS INTEGER   FORMAT ">>9" LABEL "Job no2"
    FIELD cases         AS INTEGER   FORMAT "->>>,>>9" LABEL "Units"
    FIELD qty-case      AS INTEGER   FORMAT "->>>,>>9" LABEL "Qty/Unit"    
    FIELD partial       AS INTEGER   FORMAT ">>>,>>9" LABEL "Partial"
    FIELD qty           AS INTEGER   FORMAT "->>,>>>,>>9" LABEL "Qty Shipped"
    FIELD weight        AS INTEGER   FORMAT ">>>>9" LABEL "Weight"
    FIELD freight       AS DECIMAL   FORMAT ">>,>>9.99" LABEL "Freight"
    FIELD p-c           AS LOGICAL   FORMAT "Partial/Complete" LABEL "P/C"    
    FIELD lot-no        AS CHARACTER FORMAT "x(15)" LABEL "Customer Lot#"
    FIELD unit-pallet   AS INTEGER   FORMAT ">>>,>>9" LABEL "Unts/Pallet"
    FIELD tot-sqft      AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Total Shipped Sq Ft"
    FIELD est-no        AS CHARACTER FORMAT "x(8)" LABEL "Estimate No"
    FIELD est-rm-item   AS CHARACTER FORMAT "x(20)" LABEL "Est RM item No"
    FIELD posted        AS LOGICAL   FORMAT "Yes/No" LABEL "Posted/Unposted"     
    FIELD quotedFreight AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Quoted Freight"
    FIELD quoteNote     AS CHARACTER FORMAT "x(32)" LABEL "Quoted Note"        
    .
DEFINE BUFFER bf-oe-bolh FOR oe-bolh .
DEFINE BUFFER bf-oe-boll FOR oe-boll .
DEFINE VARIABLE cocode AS CHARACTER NO-UNDO.
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 67
{AOA/includes/subjectID{&subjectID}Defs.i}

cocode = cCompany .
{ce/msfcalc.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic: 
    DEFINE VARIABLE iQtyShipped AS INTEGER NO-UNDO .
    DEFINE VARIABLE dWeight     AS DECIMAL NO-UNDO .
    DEFINE VARIABLE dFreight    AS DECIMAL NO-UNDO .
    DEFINE VARIABLE dTotSqft    AS DECIMAL NO-UNDO .
    DEFINE VARIABLE dTotalSqft  AS DECIMAL NO-UNDO .

    
    FOR EACH  bf-oe-bolh WHERE bf-oe-bolh.company EQ cCompany 
        AND bf-oe-bolh.deleted EQ NO             
        AND( (bf-oe-bolh.posted  EQ NO  AND lUnpost = TRUE) 
        OR (bf-oe-bolh.posted  EQ YES  AND lPost = TRUE) )
        AND bf-oe-bolh.bol-no GE iStartBOL
        AND bf-oe-bolh.bol-no LE iEndBOL
        AND bf-oe-bolh.cust-no GE cStartCustNo
        AND bf-oe-bolh.cust-no LE cEndCustNo
        AND (bf-oe-bolh.bol-date GE date(dtStartBOLDate) AND bf-oe-bolh.bol-date LE date(dtEndBOLDate))  NO-LOCK,
        EACH bf-oe-boll USE-INDEX b-no NO-LOCK     
        WHERE bf-oe-boll.company   EQ bf-oe-bolh.company  
        AND bf-oe-boll.b-no        EQ bf-oe-bolh.b-no    
        AND bf-oe-boll.i-no      GE cStartFGItem      
        AND bf-oe-boll.i-no      LE cEndFGItem      
        AND (bf-oe-boll.ord-no   GE iStartOrderNo ) 
        AND (bf-oe-boll.ord-no   LE iEndOrderNo ) 
        AND bf-oe-boll.po-no     GE cStartCustPoNo 
        AND bf-oe-boll.po-no     LE cEndCustPoNo BREAK BY bf-oe-bolh.bol-no DESCENDING BY bf-oe-boll.i-no  :
    
               
        IF cSummary-Details EQ "1" THEN 
        DO:   
    
            IF FIRST-OF(bf-oe-boll.i-no) THEN
                ASSIGN iQtyShipped = 0
                    dWeight     = 0
                    dFreight    = 0
                    dTotSqft    = 0.   
    
            iQtyShipped = iQtyShipped + bf-oe-boll.qty .
            dWeight = dWeight + bf-oe-boll.weight .
            dFreight = dFreight + bf-oe-boll.freight.
            IF dWeight EQ ? THEN dWeight = 0 .
            IF dFreight EQ ? THEN dFreight = 0 .
        
            IF LAST-OF(bf-oe-boll.i-no) THEN 
            DO:
                CREATE ttBillofLading.
                BUFFER-COPY bf-oe-bolh TO ttBillofLading.
              
                RUN pAssignValues( ROWID(bf-oe-bolh),ROWID(bf-oe-boll), BUFFER ttBillofLading) .             
            
            
                FIND FIRST oe-ord NO-LOCK
                    WHERE oe-ord.company EQ cCompany
                    AND oe-ord.ord-no EQ bf-oe-boll.ord-no
                    NO-ERROR.
                IF AVAILABLE oe-ord AND oe-ord.est-no NE "" THEN 
                    FIND FIRST eb NO-LOCK
                        WHERE eb.company EQ cCompany 
                        AND eb.est-no EQ oe-ord.est-no
                        AND eb.stock-no EQ bf-oe-boll.i-no NO-ERROR .
                                   
                FIND FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ cCompany
                    AND itemfg.i-no EQ bf-oe-boll.i-no
                    NO-ERROR. 
                IF AVAILABLE oe-ord AND oe-ord.est-no NE "" AND AVAILABLE eb THEN 
                DO:
                    dTotSqft = DECIMAL((IF v-corr THEN (eb.t-sqin * .007)
                    ELSE (eb.t-sqin / 144)) * iQtyShipped )    .
                END.
                ELSE IF AVAILABLE itemfg THEN 
                    DO:
                        RUN fg/GetFGArea.p (ROWID(itemfg), "SF", OUTPUT dTotalSqft).
                        ASSIGN 
                            dTotSqft = DECIMAL(dTotalSqft * iQtyShipped )    .
                    END.
                    ELSE dTotSqft = 0 .
            
             
             
                ttBillofLading.qty =   iQtyShipped .
                ttBillofLading.weight =  dWeight .
                ttBillofLading.freight = dFreight.
                ttBillofLading.tot-sqft = dTotSqft .
             
            
            END.   /* LAST-OF(oe-boll.i-no)*/        
        
        END.
        ELSE 
        DO:
       
            CREATE ttBillofLading.
            BUFFER-COPY bf-oe-bolh TO ttBillofLading.
              
            RUN pAssignValues( ROWID(bf-oe-bolh),ROWID(bf-oe-boll), BUFFER ttBillofLading) .
            
            FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no EQ bf-oe-boll.ord-no
                NO-ERROR.
            IF AVAIL oe-ord AND oe-ord.est-no NE "" THEN 
                FIND FIRST eb NO-LOCK
                    WHERE eb.company EQ cocode 
                    AND eb.est-no EQ oe-ord.est-no
                    AND eb.stock-no EQ bf-oe-boll.i-no NO-ERROR .

            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no EQ bf-oe-boll.i-no
                NO-ERROR. 
            IF AVAIL oe-ord AND oe-ord.est-no NE "" AND AVAIL eb THEN 
            do:
                ttBillofLading.tot-sqft = ((IF v-corr THEN (eb.t-sqin * .007)
                ELSE (eb.t-sqin / 144)) * bf-oe-boll.qty )    .
            END.
            ELSE IF AVAIL itemfg THEN 
                do:
                    RUN fg/GetFGArea.p (ROWID(itemfg), "SF", OUTPUT dTotalSqft).
                    ASSIGN 
                        ttBillofLading.tot-sqft = (dTotalSqft * bf-oe-boll.qty )    .
                END.
                ELSE ttBillofLading.tot-sqft = 0 .
    
        END.   /* detail */       
        
        
    END. /* each item */
END PROCEDURE.


PROCEDURE pAssignValues: 
    DEFINE INPUT PARAMETER ipRowid AS ROWID NO-UNDO.   
    DEFINE INPUT PARAMETER ipRowid2 AS ROWID NO-UNDO.   
    DEFINE PARAMETER BUFFER ipb-ttBillofLading FOR ttBillofLading.
    DEFINE           BUFFER b-oe-bolh          FOR oe-bolh .
    DEFINE           BUFFER b-oe-boll          FOR oe-boll .
    FIND FIRST  b-oe-bolh NO-LOCK
        WHERE ROWID(b-oe-bolh) EQ ipRowid NO-ERROR .
         
    FIND FIRST  b-oe-boll NO-LOCK
        WHERE ROWID(b-oe-boll) EQ ipRowid2 NO-ERROR .     
    
    ASSIGN 
        ipb-ttBillofLading.i-no     = b-oe-boll.i-no
        ipb-ttBillofLading.ord-no   = b-oe-boll.ord-no
        ipb-ttBillofLading.po-no    = b-oe-boll.po-no
        ipb-ttBillofLading.tag      = b-oe-boll.tag
        ipb-ttBillofLading.loc      = b-oe-boll.loc
        ipb-ttBillofLading.loc-bin  = b-oe-boll.loc-bin
        ipb-ttBillofLading.job-no   = b-oe-boll.job-no
        ipb-ttBillofLading.job-no2  = b-oe-boll.job-no2
        ipb-ttBillofLading.cases    = b-oe-boll.cases
        ipb-ttBillofLading.qty-case = b-oe-boll.qty-case
        ipb-ttBillofLading.partial  = b-oe-boll.partial
        ipb-ttBillofLading.qty      = b-oe-boll.qty
        ipb-ttBillofLading.weight   = b-oe-boll.weight
        ipb-ttBillofLading.freight  = b-oe-boll.freight
        ipb-ttBillofLading.p-c      = b-oe-boll.p-c
        ipb-ttBillofLading.lot-no   = b-oe-boll.lot-no .
             
    FIND FIRST fg-bin NO-LOCK WHERE fg-bin.company EQ b-oe-boll.company
        AND fg-bin.job-no EQ b-oe-boll.job-no
        AND fg-bin.job-no2 EQ b-oe-boll.job-no2
        AND fg-bin.i-no EQ b-oe-boll.i-no
        AND fg-bin.loc EQ b-oe-boll.loc
        AND fg-bin.loc-bin EQ b-oe-boll.loc-bin
        AND fg-bin.tag EQ b-oe-boll.tag NO-ERROR.
               
    ipb-ttBillofLading.unit-pallet =  IF AVAILABLE fg-bin THEN fg-bin.cases-unit ELSE 0.
           
    FIND itemfg WHERE itemfg.company EQ cCompany
        AND itemfg.i-no EQ b-oe-boll.i-no NO-LOCK NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        ASSIGN 
            ipb-ttBillofLading.cust-part = STRING(itemfg.part-no)
            ipb-ttBillofLading.i-name    = STRING(itemfg.i-name) .
    END.
    ELSE ASSIGN ipb-ttBillofLading.cust-part = ""
            ipb-ttBillofLading.i-name    = "" .
                
    FIND cust WHERE cust.company EQ cCompany
        AND cust.cust-no EQ b-oe-bolh.cust-no NO-LOCK NO-ERROR.
    IF AVAILABLE cust THEN 
    DO:
        ASSIGN 
            ipb-ttBillofLading.cust-name  = STRING(cust.NAME,"x(30)")  
            ipb-ttBillofLading.cust-addr1 = STRING(cust.addr[1])    
            ipb-ttBillofLading.cust-addr2 = STRING(cust.addr[2])  
            ipb-ttBillofLading.cust-city  = STRING(cust.city)
            ipb-ttBillofLading.cust-state = STRING(cust.state)    
            ipb-ttBillofLading.cust-zip   = STRING(cust.zip)    .
    END.
    ELSE ASSIGN ipb-ttBillofLading.cust-name  = ""
            ipb-ttBillofLading.cust-addr1 = ""
            ipb-ttBillofLading.cust-addr2 = ""
            ipb-ttBillofLading.cust-city  = ""
            ipb-ttBillofLading.cust-state = ""
            ipb-ttBillofLading.cust-zip   = "".
            
    FIND FIRST oe-relh WHERE oe-relh.company = cCompany
        AND oe-relh.r-no = b-oe-boll.r-no NO-LOCK NO-ERROR.
            
    ipb-ttBillofLading.rel-no = IF AVAILABLE oe-relh THEN INTEGER(oe-relh.release#) ELSE 0.
                
    FIND shipto WHERE shipto.company EQ cCompany
        AND shipto.cust-no EQ b-oe-bolh.cust-no
        AND shipto.ship-id = b-oe-bolh.ship-id NO-LOCK NO-ERROR.
    IF AVAILABLE shipto THEN 
    DO:
        ASSIGN 
            ipb-ttBillofLading.ship-name  = STRING(shipto.ship-name)  
            ipb-ttBillofLading.ship-addr1 = STRING(shipto.ship-addr[1])    
            ipb-ttBillofLading.ship-addr2 = STRING(shipto.ship-addr[2])
            ipb-ttBillofLading.ship-city  = STRING(shipto.ship-city) 
            ipb-ttBillofLading.ship-state = STRING(shipto.ship-state)   
            ipb-ttBillofLading.ship-zip   = STRING(shipto.ship-zip).
    END.
    ELSE ASSIGN ipb-ttBillofLading.ship-name  = ""
            ipb-ttBillofLading.ship-addr1 = ""
            ipb-ttBillofLading.ship-addr2 = "" 
            ipb-ttBillofLading.ship-city  = ""
            ipb-ttBillofLading.ship-state = "" 
            ipb-ttBillofLading.ship-zip   = "".    
            
    FIND FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ cCompany
        AND oe-ord.ord-no EQ b-oe-boll.ord-no
        NO-ERROR.
    IF AVAILABLE oe-ord THEN 
    DO:
        ASSIGN 
            ipb-ttBillofLading.est-no = STRING(oe-ord.est-no)    .
        FIND FIRST ef NO-LOCK
            WHERE ef.company EQ cCompany 
            AND ef.est-no EQ oe-ord.est-no NO-ERROR .
        IF AVAILABLE ef THEN 
            ASSIGN 
                ipb-ttBillofLading.est-rm-item = STRING(ef.board)    .
        ELSE 
            ipb-ttBillofLading.est-rm-item = "" .
    END.
    ELSE ASSIGN ipb-ttBillofLading.est-no      = "" 
            ipb-ttBillofLading.est-rm-item = "No Estimate".
    
    
    
END PROCEDURE.
    
