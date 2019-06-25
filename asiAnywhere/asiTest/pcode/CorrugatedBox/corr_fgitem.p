            /*------------------------------------------------------------------------
    File        : corr-e-item.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all AdderLook

    Author(s)   : 
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFgItemVend NO-UNDO 
    FIELD vITEM           AS CHAR 
    FIELD vStdum           AS CHAR
    FIELD vVend-item       AS CHAR 
    FIELD vend-no         AS CHAR 
    FIELD vend-name       AS CHAR
    FIELD vCust           AS CHAR 
    FIELD vRun-qty         AS DECIMAL EXTENT 10  
    FIELD vSetups          AS DECIMAL EXTENT 10
    FIELD vRun-cost        AS DECIMAL EXTENT 10
    FIELD vRoll-w          AS DECIMAL EXTENT 4
    FIELD vRowid-eitem     AS CHAR 
    FIELD vRowid-vend      AS CHAR 
    FIELD vMarkup          AS DECIMAL 
    FIELD vShowitem        AS CHAR 
    FIELD tb_sel           AS CHAR
    FIELD tb_sel-1           AS CHAR
    FIELD tb_sel-2           AS CHAR
    FIELD tb_sel-3           AS CHAR
    FIELD tb_sel-4           AS CHAR
    FIELD tb_sel-5           AS CHAR
    FIELD tb_sel-6           AS CHAR
    FIELD tb_sel-7           AS CHAR
    FIELD tb_sel-8           AS CHAR
    FIELD tb_sel-9           AS CHAR
    FIELD tb_sel-10           AS CHAR
    .

DEFINE DATASET dsFgItemVend FOR ttFgItemVend.

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEst          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBlanck       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmItem         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmStdum         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmVendItem     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmVendNO       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmRunQty1      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunQty2      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunQty3      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunQty4      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunQty5      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunQty6      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunQty7      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunQty8      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunQty9      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunQty10     AS DECIMAL NO-UNDO.

DEFINE INPUT PARAMETER prmSetups1      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSetups2      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSetups3      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSetups4      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSetups5      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSetups6      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSetups7      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSetups8      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSetups9      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSetups10     AS DECIMAL NO-UNDO.

DEFINE INPUT PARAMETER prmRunCost1      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunCost2      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunCost3      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunCost4      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunCost5      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunCost6      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunCost7      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunCost8      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunCost9      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRunCost10     AS DECIMAL NO-UNDO.

DEFINE INPUT PARAMETER prmRollw1       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw2       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw3       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw4       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRowid1      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmRowid2      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMarkup      AS DECIMAL NO-UNDO.

DEFINE INPUT PARAMETER prmTbSel       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTbSel1       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTbSel2       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTbSel3       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTbSel4       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTbSel5       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTbSel6       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTbSel7       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTbSel8       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTbSel9       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTbSel10      AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFgItemVend.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.
       
DEF VAR prmComp AS CHAR NO-UNDO.
ASSIGN prmItem = "FHER10000115A" .

        IF prmAction    = ? THEN ASSIGN prmAction    = "".          
        IF prmUser      = ? THEN ASSIGN prmUser      = "".
        IF prmEst       = ? THEN ASSIGN prmEst       = "".
        IF prmFormNo    = ? THEN ASSIGN prmFormNo    = 0.
        IF prmBlanck    = ? THEN ASSIGN prmBlanck    = 0.
        IF prmItem      = ? THEN ASSIGN prmItem      = "".
        IF prmVendItem  = ? THEN ASSIGN prmVendItem  = "".
        IF prmVendNO    = ? THEN ASSIGN prmVendNO    = "".
        IF prmCust      = ? THEN ASSIGN prmCust      = "".
        IF prmRunQty1   = ? THEN ASSIGN prmRunQty1   = 0.
        IF prmRunQty2   = ? THEN ASSIGN prmRunQty2   = 0.
        IF prmRunQty3   = ? THEN ASSIGN prmRunQty3   = 0.
        IF prmRunQty4   = ? THEN ASSIGN prmRunQty4   = 0.
        IF prmRunQty5   = ? THEN ASSIGN prmRunQty5   = 0.
        IF prmRunQty6   = ? THEN ASSIGN prmRunQty6   = 0.
        IF prmRunQty7   = ? THEN ASSIGN prmRunQty7   = 0.
        IF prmRunQty8   = ? THEN ASSIGN prmRunQty8   = 0.
        IF prmRunQty9   = ? THEN ASSIGN prmRunQty9   = 0.
        IF prmRunQty10  = ? THEN ASSIGN prmRunQty10  = 0.
      
        IF prmSetups1   = ? THEN ASSIGN prmSetups1   = 0.
        IF prmSetups2   = ? THEN ASSIGN prmSetups2   = 0.
        IF prmSetups3   = ? THEN ASSIGN prmSetups3   = 0.
        IF prmSetups4   = ? THEN ASSIGN prmSetups4   = 0.
        IF prmSetups5   = ? THEN ASSIGN prmSetups5   = 0.   
        IF prmSetups6   = ? THEN ASSIGN prmSetups6   = 0.   
        IF prmSetups7   = ? THEN ASSIGN prmSetups7   = 0.   
        IF prmSetups8   = ? THEN ASSIGN prmSetups8   = 0.   
        IF prmSetups9   = ? THEN ASSIGN prmSetups9   = 0.   
        IF prmSetups10  = ? THEN ASSIGN prmSetups10  = 0.   
       
        IF prmRunCost1  = ? THEN ASSIGN prmRunCost1  = 0.   
        IF prmRunCost2  = ? THEN ASSIGN prmRunCost2  = 0.   
        IF prmRunCost3  = ? THEN ASSIGN prmRunCost3  = 0.   
        IF prmRunCost4  = ? THEN ASSIGN prmRunCost4  = 0.   
        IF prmRunCost5  = ? THEN ASSIGN prmRunCost5  = 0.   
        IF prmRunCost6  = ? THEN ASSIGN prmRunCost6  = 0.   
        IF prmRunCost7  = ? THEN ASSIGN prmRunCost7  = 0.   
        IF prmRunCost8  = ? THEN ASSIGN prmRunCost8  = 0.   
        IF prmRunCost9  = ? THEN ASSIGN prmRunCost9  = 0.   
        IF prmRunCost10 = ? THEN ASSIGN prmRunCost10 = 0.   
         
        IF prmRollw1    = ? THEN ASSIGN prmRollw1    = 0.   
        IF prmRollw2    = ? THEN ASSIGN prmRollw2    = 0.   
        IF prmRollw3    = ? THEN ASSIGN prmRollw3    = 0.   
        IF prmRollw4    = ? THEN ASSIGN prmRollw4    = 0.   
       
        IF prmMarkup    = ? THEN ASSIGN prmMarkup    = 0.
        
IF INDEX(prmItem ,'&quot;',1) > 0 THEN ASSIGN
            prmItem  = REPLACE(prmItem ,'&quot;','"').

DEF BUFFER bf-itemfg FOR itemfg.
    DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
    DEF BUFFER b-reftable-1 FOR reftable.
    def temp-table tmpfile field siz as dec
                       field qty as dec
                       field setups as dec.
    def var lv-roll-w like e-itemfg.roll-w no-undo.

    DEFINE VAR i AS INT NO-UNDO.
    def var uom-list as cha init ["M,EA,L,CS,C"] no-undo.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "AddVendor" THEN DO:
    
    FIND FIRST itemfg WHERE
    ITEMfg.company = prmComp AND
    ITEMfg.i-no = prmItem
    NO-LOCK NO-ERROR.

  FIND FIRST e-itemfg WHERE
      e-itemfg.company = itemfg.company AND
      e-itemfg.i-no = prmItem AND e-itemfg.rec_key = prmRowid1
      NO-LOCK NO-ERROR.
     
    FIND FIRST e-itemfg-vend WHERE
         e-itemfg-vend.company = itemfg.company AND
         e-itemfg-vend.i-no = prmItem AND
         e-itemfg-vend.rec_key = prmRowid2
         NO-LOCK NO-ERROR.

    FIND FIRST vend WHERE vend.company EQ e-itemfg.company
                        AND vend.vend-no EQ prmVendNO NO-LOCK NO-ERROR.
    IF NOT AVAIL vend THEN DO:
        ASSIGN
            cError = "Vendor: " +   prmVendNO  + " is invalid, try help".
            RETURN.
    END.
       

    IF prmCust NE "" AND NOT CAN-FIND(FIRST cust WHERE
             cust.company EQ prmComp AND
             cust.cust-no EQ prmCust) THEN  DO:
        ASSIGN
            cError =  "Invalid Cust. #, try help..." .
            RETURN .
    END.

     /*FIND FIRST bf-evend NO-LOCK
                  WHERE bf-evend.company   EQ e-item.company
                    AND bf-evend.i-no      EQ e-item.i-no
                    AND bf-evend.item-type EQ YES
                    AND bf-evend.vend-no   EQ prmVendNO
                    AND (ROWID(bf-evend)   NE ROWID(e-itemfg-vend))  NO-ERROR.
     IF AVAIL bf-evend THEN DO:
         ASSIGN
             cError = "Vendor: " + prmVendNO + " already exists...".
             RETURN.
     END.*/

   /* run sys/ref/uom-rm.p (ITEM.mat-type, output uom-list). 
   
   IF prmvStdum <> "" AND LOOKUP(prmvStdum,uom-list) <= 0
    then do:
       cError =  "Invalid UOM. Try Help." .       
       return .
    end.   */ 

    prmStdum = CAPS(prmStdum).

    RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).

    IF LOOKUP(prmStdum,uom-list) LE 0 OR (prmStdum EQ "MSF" AND
        NOT CAN-FIND(FIRST bf-itemfg
                     WHERE bf-itemfg.company EQ e-itemfg.company
                       AND bf-itemfg.i-no    EQ e-itemfg.i-no
                       AND ((bf-itemfg.t-len NE 0 AND bf-itemfg.t-wid NE 0) OR
                            bf-itemfg.t-sqin NE 0                           OR
                            bf-itemfg.t-sqft NE 0)))     THEN DO:

      IF prmStdum EQ "MSF" THEN DO:
        cError = "When " + prmStdum  + " is MSF, " +
                "FG Item must have valid Blank Length & Width, SqIn, or SqFt..." .
            RETURN .
       END.
      ELSE DO:
        cError =  TRIM(prmStdum) + " must be " + TRIM(uom-list) .
          RETURN.
      END.

     END.

      

END.

IF prmAction = "AddVendor" THEN DO:

    FIND FIRST ITEMfg WHERE
    ITEMfg.company = prmComp AND
    ITEMfg.i-no = prmItem
    NO-LOCK NO-ERROR.

  IF AVAILABLE ITEMfg THEN DO:

     FIND FIRST e-itemfg WHERE
      e-itemfg.company = itemfg.company AND
      e-itemfg.i-no = prmItem AND e-itemfg.rec_key = prmRowid1
      EXCLUSIVE-LOCK NO-ERROR.
     
    FIND FIRST e-itemfg-vend WHERE
         e-itemfg-vend.company = itemfg.company AND
         e-itemfg-vend.i-no = prmItem AND
         e-itemfg-vend.rec_key = prmRowid2
         EXCLUSIVE-LOCK NO-ERROR.
   find bf-itemfg where recid(bf-itemfg) = recid(itemfg).
  
  ASSIGN
      e-itemfg-vend.company               = e-itemfg.company
      e-itemfg-vend.i-no                  = e-itemfg.i-no
      e-itemfg.std-uom                    =   prmStdum        
      /*e-itemfg-vend.setup                 = bf-itemfg.min-sqft*/
      e-itemfg-vend.item-type             = NO
      e-itemfg-vend.vend-item            =   prmVendItem   
      e-itemfg-vend.vend-no               =   prmVendNO  
      e-itemfg-vend.cust-no               =  prmCust
      e-itemfg-vend.Run-qty[1]            =   prmRunQty1    
      e-itemfg-vend.Run-qty[2]            =   prmRunQty2 
      e-itemfg-vend.Run-qty[3]            =   prmRunQty3
      e-itemfg-vend.Run-qty[4]            =   prmRunQty4
      e-itemfg-vend.Run-qty[5]            =   prmRunQty5
      e-itemfg-vend.Run-qty[6]            =   prmRunQty6
      e-itemfg-vend.Run-qty[7]            =   prmRunQty7
      e-itemfg-vend.Run-qty[8]            =   prmRunQty8
      e-itemfg-vend.Run-qty[9]            =   prmRunQty9
      e-itemfg-vend.Run-qty[10]           =   prmRunQty10
      e-itemfg-vend.Run-cost[1]           =   prmRunCost1 
      e-itemfg-vend.Run-cost[2]           =   prmRunCost2 
      e-itemfg-vend.Run-cost[3]           =   prmRunCost3 
      e-itemfg-vend.Run-cost[4]           =   prmRunCost4 
      e-itemfg-vend.Run-cost[5]           =   prmRunCost5 
      e-itemfg-vend.Run-cost[6]           =   prmRunCost6 
      e-itemfg-vend.Run-cost[7]           =   prmRunCost7 
      e-itemfg-vend.Run-cost[8]           =   prmRunCost8 
      e-itemfg-vend.Run-cost[9]           =   prmRunCost9 
      e-itemfg-vend.Run-cost[10]          =   prmRunCost10
      e-itemfg-vend.Setups[1]             =   prmSetups1  
      e-itemfg-vend.Setups[2]             =   prmSetups2  
      e-itemfg-vend.Setups[3]             =   prmSetups3  
      e-itemfg-vend.Setups[4]             =   prmSetups4  
      e-itemfg-vend.Setups[5]             =   prmSetups5  
      e-itemfg-vend.Setups[6]             =   prmSetups6  
      e-itemfg-vend.Setups[7]             =   prmSetups7  
      e-itemfg-vend.Setups[8]             =   prmSetups8  
      e-itemfg-vend.Setups[9]             =   prmSetups9  
      e-itemfg-vend.Setups[10]            =   prmSetups10 

      e-itemfg-vend.Roll-w[27]             =   prmRollw1 
      e-itemfg-vend.Roll-w[28]             =   prmRollw2 
      e-itemfg-vend.Roll-w[29]             =   prmRollw3 
      e-itemfg-vend.Roll-w[30]             =   prmRollw4 
        .
    
        ASSIGN
            prmRowid2 = e-itemfg-vend.rec_key.

     FIND FIRST eb WHERE eb.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
            AND eb.form-no = prmFormNo AND eb.blank-no = prmBlanck  NO-LOCK NO-ERROR.
       IF AVAIL eb THEN
        ASSIGN e-itemfg-vend.est-no = eb.est-no
               e-itemfg-vend.eqty = eb.eqty
               e-itemfg-vend.form-no = eb.form-no
               e-itemfg-vend.blank-no = eb.blank-no.
       
            FIND FIRST reftable WHERE 
                reftable.reftable EQ 'e-itemfg-vend.markup' AND
                reftable.company EQ e-itemfg-vend.company AND
                reftable.loc EQ e-itemfg-vend.i-no AND
                reftable.code EQ e-itemfg-vend.vend-no
                EXCLUSIVE-LOCK NO-ERROR.

            IF NOT AVAIL reftable THEN DO:
               CREATE reftable.
               ASSIGN
                    reftable.reftable = 'e-itemfg-vend.markup'
                    reftable.company = e-itemfg-vend.company
                    reftable.loc = e-itemfg-vend.i-no
                    reftable.code = e-itemfg-vend.vend-no.
               END.
               IF AVAIL  reftable  THEN DO:
                   ASSIGN
                       reftable.val[1] = prmMarkup    . 
                      
               end.

        END. /* avail item and mat-type */
          

      
        ASSIGN
            prmAction = "View" .

      END. /* end of save*/

IF prmAction = "Select" THEN DO:

          
          FIND FIRST ITEMfg WHERE ITEMfg.company = prmComp AND
              ITEMfg.i-no = prmItem  NO-LOCK NO-ERROR.

          IF AVAILABLE ITEMfg THEN DO:
              FIND FIRST e-itemfg WHERE
                  e-itemfg.company = itemfg.company AND
                  e-itemfg.i-no = prmItem
                  EXCLUSIVE-LOCK NO-ERROR.

            if not avail e-itemfg then do:
                create e-itemfg.
                assign e-itemfg.company = itemfg.company
                       e-itemfg.i-no = itemfg.i-no
                       e-itemfg.std-uom = itemfg.pur-uom.
            end.

            FIND FIRST e-itemfg-vend WHERE
                e-itemfg-vend.company = itemfg.company AND
                e-itemfg-vend.i-no = prmItem AND
                e-itemfg-vend.vend-no = ""
                EXCLUSIVE-LOCK NO-ERROR.
   
    IF NOT AVAILABLE e-itemfg-vend THEN DO:
      create e-itemfg-vend.
      assign e-itemfg-vend.company = e-itemfg.company
             e-itemfg-vend.i-no = e-itemfg.i-no
             e-itemfg-vend.vend-no = ""
             e-itemfg-vend.item-type = NO .
    END.

    find bf-itemfg where recid(bf-itemfg) = recid(itemfg).

    do i = 1 to 10:
     assign e-itemfg-vend.run-qty[i] = IF AVAIL e-itemfg THEN e-itemfg.run-qty[i] ELSE 0
            e-itemfg-vend.run-cost[i] = IF AVAIL e-itemfg THEN e-itemfg.run-cost[i] ELSE 0 .
    end.
   
   CREATE ttFgItemVend .
    ASSIGN
        ttFgItemVend.vITEM            = e-itemfg-vend.i-no     
        ttFgItemVend.vStdum           = e-itemfg.std-uom
        ttFgItemVend.vVend-item       = e-itemfg-vend.vend-item
        ttFgItemVend.vend-no          = e-itemfg-vend.vend-no
        ttFgItemVend.vCust            = STRING(e-itemfg-vend.cust-no)
        ttFgItemVend.vRun-qty[1]      = e-itemfg-vend.Run-qty[1]
        ttFgItemVend.vRun-qty[2]      = e-itemfg-vend.Run-qty[2]
        ttFgItemVend.vRun-qty[3]      = e-itemfg-vend.Run-qty[3]
        ttFgItemVend.vRun-qty[4]      = e-itemfg-vend.Run-qty[4]
        ttFgItemVend.vRun-qty[5]      = e-itemfg-vend.Run-qty[5]
        ttFgItemVend.vRun-qty[6]      = e-itemfg-vend.Run-qty[6]
        ttFgItemVend.vRun-qty[7]      = e-itemfg-vend.Run-qty[7]
        ttFgItemVend.vRun-qty[8]      = e-itemfg-vend.Run-qty[8]
        ttFgItemVend.vRun-qty[9]      = e-itemfg-vend.Run-qty[9]
        ttFgItemVend.vRun-qty[10]     = e-itemfg-vend.Run-qty[10]
        ttFgItemVend.vSetups[1]       = e-itemfg-vend.Setups[1]
        ttFgItemVend.vSetups[2]       = e-itemfg-vend.Setups[2]
        ttFgItemVend.vSetups[3]       = e-itemfg-vend.Setups[3]
        ttFgItemVend.vSetups[4]       = e-itemfg-vend.Setups[4]
        ttFgItemVend.vSetups[5]       = e-itemfg-vend.Setups[5]
        ttFgItemVend.vSetups[6]       = e-itemfg-vend.Setups[6]
        ttFgItemVend.vSetups[7]       = e-itemfg-vend.Setups[7]
        ttFgItemVend.vSetups[8]       = e-itemfg-vend.Setups[8]
        ttFgItemVend.vSetups[9]       = e-itemfg-vend.Setups[9]
        ttFgItemVend.vSetups[10]      = e-itemfg-vend.Setups[10]

        ttFgItemVend.vRun-cost[1]     = e-itemfg-vend.Run-cost[1]
        ttFgItemVend.vRun-cost[2]     = e-itemfg-vend.Run-cost[2]
        ttFgItemVend.vRun-cost[3]     = e-itemfg-vend.Run-cost[3]
        ttFgItemVend.vRun-cost[4]     = e-itemfg-vend.Run-cost[4]
        ttFgItemVend.vRun-cost[5]     = e-itemfg-vend.Run-cost[5]
        ttFgItemVend.vRun-cost[6]     = e-itemfg-vend.Run-cost[6]
        ttFgItemVend.vRun-cost[7]     = e-itemfg-vend.Run-cost[7]
        ttFgItemVend.vRun-cost[8]     = e-itemfg-vend.Run-cost[8]
        ttFgItemVend.vRun-cost[9]     = e-itemfg-vend.Run-cost[9]
        ttFgItemVend.vRun-cost[10]    = e-itemfg-vend.Run-cost[10]
        ttFgItemVend.vRoll-w[1]       = e-itemfg-vend.Roll-w[1]
        ttFgItemVend.vRoll-w[2]       = e-itemfg-vend.Roll-w[2]
        ttFgItemVend.vRoll-w[3]       = e-itemfg-vend.Roll-w[3]
        ttFgItemVend.vRoll-w[4]       = e-itemfg-vend.Roll-w[4]
         
          
       ttFgItemVend.vRowid-eitem      = STRING(e-itemfg.rec_key)
       ttFgItemVend.vRowid-vend       = STRING(e-itemfg-vend.rec_key) .
     
  END.
END.



IF prmAction = "View" THEN DO:

    FIND FIRST ITEMfg WHERE
    ITEMfg.company = prmComp AND
    ITEMfg.i-no = prmItem
    NO-LOCK NO-ERROR.

  IF AVAILABLE ITEMfg THEN DO:

    FIND FIRST e-itemfg WHERE
      e-itemfg.company = itemfg.company AND
      e-itemfg.i-no = prmItem AND e-itemfg.rec_key = prmRowid1
      EXCLUSIVE-LOCK NO-ERROR.
   
    
    FIND FIRST e-itemfg-vend WHERE
         e-itemfg-vend.company = itemfg.company AND
         e-itemfg-vend.i-no = prmItem AND e-itemfg-vend.rec_key = prmRowid2
                EXCLUSIVE-LOCK NO-ERROR.
    
    
    CREATE ttFgItemVend .
    ASSIGN
        ttFgItemVend.vITEM            = e-itemfg-vend.i-no     
        ttFgItemVend.vStdum           = e-itemfg.std-uom
        ttFgItemVend.vVend-item       = e-itemfg-vend.vend-item
        ttFgItemVend.vend-no          = e-itemfg-vend.vend-no
        ttFgItemVend.vCust            = STRING(e-itemfg-vend.cust-no )
        ttFgItemVend.vRun-qty[1]      = e-itemfg-vend.Run-qty[1]
        ttFgItemVend.vRun-qty[2]      = e-itemfg-vend.Run-qty[2]
        ttFgItemVend.vRun-qty[3]      = e-itemfg-vend.Run-qty[3]
        ttFgItemVend.vRun-qty[4]      = e-itemfg-vend.Run-qty[4]
        ttFgItemVend.vRun-qty[5]      = e-itemfg-vend.Run-qty[5]
        ttFgItemVend.vRun-qty[6]      = e-itemfg-vend.Run-qty[6]
        ttFgItemVend.vRun-qty[7]      = e-itemfg-vend.Run-qty[7]
        ttFgItemVend.vRun-qty[8]      = e-itemfg-vend.Run-qty[8]
        ttFgItemVend.vRun-qty[9]      = e-itemfg-vend.Run-qty[9]
        ttFgItemVend.vRun-qty[10]     = e-itemfg-vend.Run-qty[10]
        ttFgItemVend.vSetups[1]       = e-itemfg-vend.Setups[1]
        ttFgItemVend.vSetups[2]       = e-itemfg-vend.Setups[2]
        ttFgItemVend.vSetups[3]       = e-itemfg-vend.Setups[3]
        ttFgItemVend.vSetups[4]       = e-itemfg-vend.Setups[4]
        ttFgItemVend.vSetups[5]       = e-itemfg-vend.Setups[5]
        ttFgItemVend.vSetups[6]       = e-itemfg-vend.Setups[6]
        ttFgItemVend.vSetups[7]       = e-itemfg-vend.Setups[7]
        ttFgItemVend.vSetups[8]       = e-itemfg-vend.Setups[8]
        ttFgItemVend.vSetups[9]       = e-itemfg-vend.Setups[9]
        ttFgItemVend.vSetups[10]      = e-itemfg-vend.Setups[10]

        ttFgItemVend.vRun-cost[1]     = e-itemfg-vend.Run-cost[1]
        ttFgItemVend.vRun-cost[2]     = e-itemfg-vend.Run-cost[2]
        ttFgItemVend.vRun-cost[3]     = e-itemfg-vend.Run-cost[3]
        ttFgItemVend.vRun-cost[4]     = e-itemfg-vend.Run-cost[4]
        ttFgItemVend.vRun-cost[5]     = e-itemfg-vend.Run-cost[5]
        ttFgItemVend.vRun-cost[6]     = e-itemfg-vend.Run-cost[6]
        ttFgItemVend.vRun-cost[7]     = e-itemfg-vend.Run-cost[7]
        ttFgItemVend.vRun-cost[8]     = e-itemfg-vend.Run-cost[8]
        ttFgItemVend.vRun-cost[9]     = e-itemfg-vend.Run-cost[9]
        ttFgItemVend.vRun-cost[10]    = e-itemfg-vend.Run-cost[10]
        
        ttFgItemVend.vRoll-w[1]       = e-itemfg-vend.Roll-w[27]
        ttFgItemVend.vRoll-w[2]       = e-itemfg-vend.Roll-w[28]
        ttFgItemVend.vRoll-w[3]       = e-itemfg-vend.Roll-w[29]
        ttFgItemVend.vRoll-w[4]       = e-itemfg-vend.Roll-w[30]
        
        ttFgItemVend.vRowid-eitem      = STRING(e-itemfg.rec_key)
        ttFgItemVend.vRowid-vend       = STRING(e-itemfg-vend.rec_key) .

    FIND FIRST reftable WHERE
            reftable.reftable EQ 'e-itemfg-vend.markup' AND
            reftable.company EQ e-itemfg-vend.company AND
            reftable.loc EQ e-itemfg-vend.i-no AND
            reftable.code EQ e-itemfg-vend.vend-no
            NO-LOCK NO-ERROR.

    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
          ASSIGN
             reftable.reftable = 'e-itemfg-vend.markup'
             reftable.company = e-itemfg-vend.company
             reftable.loc = e-itemfg-vend.i-no
             reftable.code = e-itemfg-vend.vend-no.
    END.
    IF AVAIL  reftable  THEN DO:
        ASSIGN
            ttFgItemVend.vMarkup   =   reftable.val[1] .
    END.

    
     FIND FIRST vend NO-LOCK
          WHERE vend.company EQ prmComp
          AND vend.vend-no EQ e-itemfg-vend.vend-no NO-ERROR.
          ttFgItemVend.vend-name = IF AVAIL vend THEN vend.name ELSE "".

  END.
END.
