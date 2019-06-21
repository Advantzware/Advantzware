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

DEFINE TEMP-TABLE ttEItemVendor NO-UNDO 
    FIELD vITEM           AS CHAR 
    FIELD stdum           AS CHAR
    FIELD vend-item       AS CHAR 
    FIELD vend-no         AS CHAR 
    FIELD vend-name       AS CHAR
    FIELD updated-date    AS CHAR 
    FIELD run-qty         AS DECIMAL EXTENT 10  
    FIELD setups          AS DECIMAL EXTENT 10
    FIELD run-cost        AS DECIMAL EXTENT 10
    FIELD roll-w          AS DECIMAL EXTENT 30
    FIELD rowid-eitem     AS CHAR 
    FIELD rowid-vend      AS CHAR 
    FIELD width-min       AS DECIMAL
    FIELD length-min      AS DECIMAL
    FIELD width-cst       AS DECIMAL
    FIELD length-cst      AS DECIMAL
    FIELD showitem        AS CHAR 
    .

DEFINE DATASET dsEItemVendor FOR ttEItemVendor.

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEst          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBlanck       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmItem         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmStdum         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmVendItem     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmVendNO       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDate         AS CHAR NO-UNDO.
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
DEFINE INPUT PARAMETER prmRollw5       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw6       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw7       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw8       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw9       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw10      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw11      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw12      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw13      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw14      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw15      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw16      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw17      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw18      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw19      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw20      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw21      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw22      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw23      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw24      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw25      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw26      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw27      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw28      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw29      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRollw30      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmRowid1      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmRowid2      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmWidthmin      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmLengthmin      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmWidthcst     AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmLengthcst      AS DECIMAL NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEItemVendor.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.
       
DEF VAR prmComp AS CHAR NO-UNDO.

        IF prmAction    = ? THEN ASSIGN prmAction    = "".          
        IF prmUser      = ? THEN ASSIGN prmUser      = "".
        IF prmEst       = ? THEN ASSIGN prmEst       = "".
        IF prmFormNo    = ? THEN ASSIGN prmFormNo    = 0.
        IF prmBlanck    = ? THEN ASSIGN prmBlanck    = 0.
        IF prmItem      = ? THEN ASSIGN prmItem      = "".
        IF prmVendItem  = ? THEN ASSIGN prmVendItem  = "".
        IF prmVendNO    = ? THEN ASSIGN prmVendNO    = "".
        IF prmDate      = ? THEN ASSIGN prmDate      = "".
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
        IF prmRollw5    = ? THEN ASSIGN prmRollw5    = 0.   
        IF prmRollw6    = ? THEN ASSIGN prmRollw6    = 0.   
        IF prmRollw7    = ? THEN ASSIGN prmRollw7    = 0.   
        IF prmRollw8    = ? THEN ASSIGN prmRollw8    = 0.   
        IF prmRollw9    = ? THEN ASSIGN prmRollw9    = 0.   
        IF prmRollw10   = ? THEN ASSIGN prmRollw10   = 0.   
        IF prmRollw11   = ? THEN ASSIGN prmRollw11   = 0.   
        IF prmRollw12   = ? THEN ASSIGN prmRollw12   = 0.   
        IF prmRollw13   = ? THEN ASSIGN prmRollw13   = 0.   
        IF prmRollw14   = ? THEN ASSIGN prmRollw14   = 0.   
        IF prmRollw15   = ? THEN ASSIGN prmRollw15   = 0.   
        IF prmRollw16   = ? THEN ASSIGN prmRollw16   = 0.   
        IF prmRollw17   = ? THEN ASSIGN prmRollw17   = 0.   
        IF prmRollw18   = ? THEN ASSIGN prmRollw18   = 0.   
        IF prmRollw19   = ? THEN ASSIGN prmRollw19   = 0.   
        IF prmRollw20   = ? THEN ASSIGN prmRollw20   = 0.   
        IF prmRollw21   = ? THEN ASSIGN prmRollw21   = 0.   
        IF prmRollw22   = ? THEN ASSIGN prmRollw22   = 0.   
        IF prmRollw23   = ? THEN ASSIGN prmRollw23   = 0.   
        IF prmRollw24   = ? THEN ASSIGN prmRollw24   = 0.   
        IF prmRollw25   = ? THEN ASSIGN prmRollw25   = 0.   
        IF prmRollw26   = ? THEN ASSIGN prmRollw26   = 0.   
        IF prmRollw27   = ? THEN ASSIGN prmRollw27   = 0.   
        IF prmRollw28   = ? THEN ASSIGN prmRollw28   = 0.
        IF prmRollw29   = ? THEN ASSIGN prmRollw29   = 0.
        IF prmRollw30   = ? THEN ASSIGN prmRollw30   = 0.
        IF prmWidthmin  = ? THEN ASSIGN prmWidthmin  = 0.
        IF prmLengthmin = ? THEN ASSIGN prmLengthmin = 0.
        IF prmWidthcst  = ? THEN ASSIGN prmWidthcst  = 0.
        IF prmLengthcst = ? THEN ASSIGN prmLengthcst = 0.


IF INDEX(prmItem ,'&quot;',1) > 0 THEN ASSIGN
            prmItem  = REPLACE(prmItem ,'&quot;','"').

DEF BUFFER bf-item FOR ITEM.
     DEF BUFFER bf-evend FOR e-item-vend.
    DEFINE VAR i AS INT NO-UNDO.
    def var uom-list as cha init ["M,EA,L,CS,C"] no-undo.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "AddVendor" THEN DO:
    FIND FIRST ITEM WHERE
    ITEM.company = prmComp AND
    ITEM.i-no = prmItem
    NO-LOCK NO-ERROR.

  FIND FIRST e-item WHERE
      e-item.company = item.company AND
      e-item.i-no = prmItem AND e-item.rec_key = prmRowid1
      NO-LOCK NO-ERROR.
     
    FIND FIRST e-item-vend WHERE
         e-item-vend.company = item.company AND
         e-item-vend.i-no = prmItem AND
         e-item-vend.rec_key = prmRowid2
         NO-LOCK NO-ERROR.

    FIND FIRST vend WHERE vend.company EQ e-item.company
                        AND vend.vend-no EQ prmVendNO NO-LOCK NO-ERROR.
    IF NOT AVAIL vend THEN DO:
        ASSIGN
            cError = "Vendor: " +   prmVendNO  + " is invalid, try help".
            RETURN.
    END.
       
     FIND FIRST bf-evend NO-LOCK
                  WHERE bf-evend.company   EQ e-item.company
                    AND bf-evend.i-no      EQ e-item.i-no
                    AND bf-evend.item-type EQ YES
                    AND bf-evend.vend-no   EQ prmVendNO
                    AND (ROWID(bf-evend)   NE ROWID(e-item-vend))  NO-ERROR.
     IF AVAIL bf-evend THEN DO:
         ASSIGN
             cError = "Vendor: " + prmVendNO + " already exists...".
             RETURN.
     END.

    run sys/ref/uom-rm.p (ITEM.mat-type, output uom-list). 
   
   IF prmStdum <> "" AND LOOKUP(prmStdum,uom-list) <= 0
    then do:
       cError =  "Invalid UOM. Try Help." .       
       return .
    end.   
      

END.

IF prmAction = "AddVendor" THEN DO:

    FIND FIRST ITEM WHERE
    ITEM.company = prmComp AND
    ITEM.i-no = prmItem
    NO-LOCK NO-ERROR.

  IF AVAILABLE ITEM THEN DO:

     FIND FIRST e-item WHERE
      e-item.company = item.company AND
      e-item.i-no = prmItem AND e-item.rec_key = prmRowid1
      EXCLUSIVE-LOCK NO-ERROR.
     
    FIND FIRST e-item-vend WHERE
         e-item-vend.company = item.company AND
         e-item-vend.i-no = prmItem AND
         e-item-vend.rec_key = prmRowid2
         EXCLUSIVE-LOCK NO-ERROR.
   find bf-item where recid(bf-item) = recid(item).
  
  ASSIGN
      e-item-vend.company               = e-item.company
      e-item-vend.i-no                  = e-item.i-no
      e-item.std-uom                    =   prmStdum        
      e-item-vend.setup                 = bf-item.min-sqft
      e-item-vend.item-type             = YES
      e-item-vend.vend-item             =   prmVendItem   
      e-item-vend.vend-no               =   prmVendNO  
      e-item-vend.updated-date[1]       =  DATE(prmDate)
      e-item-vend.run-qty[1]            =   prmRunQty1    
      e-item-vend.run-qty[2]            =   prmRunQty2 
      e-item-vend.run-qty[3]            =   prmRunQty3
      e-item-vend.run-qty[4]            =   prmRunQty4
      e-item-vend.run-qty[5]            =   prmRunQty5
      e-item-vend.run-qty[6]            =   prmRunQty6
      e-item-vend.run-qty[7]            =   prmRunQty7
      e-item-vend.run-qty[8]            =   prmRunQty8
      e-item-vend.run-qty[9]            =   prmRunQty9
      e-item-vend.run-qty[10]           =   prmRunQty10
      e-item-vend.run-cost[1]           =   prmRunCost1 
      e-item-vend.run-cost[2]           =   prmRunCost2 
      e-item-vend.run-cost[3]           =   prmRunCost3 
      e-item-vend.run-cost[4]           =   prmRunCost4 
      e-item-vend.run-cost[5]           =   prmRunCost5 
      e-item-vend.run-cost[6]           =   prmRunCost6 
      e-item-vend.run-cost[7]           =   prmRunCost7 
      e-item-vend.run-cost[8]           =   prmRunCost8 
      e-item-vend.run-cost[9]           =   prmRunCost9 
      e-item-vend.run-cost[10]          =   prmRunCost10
      e-item-vend.setups[1]             =   prmSetups1  
      e-item-vend.setups[2]             =   prmSetups2  
      e-item-vend.setups[3]             =   prmSetups3  
      e-item-vend.setups[4]             =   prmSetups4  
      e-item-vend.setups[5]             =   prmSetups5  
      e-item-vend.setups[6]             =   prmSetups6  
      e-item-vend.setups[7]             =   prmSetups7  
      e-item-vend.setups[8]             =   prmSetups8  
      e-item-vend.setups[9]             =   prmSetups9  
      e-item-vend.setups[10]            =   prmSetups10 

      e-item-vend.roll-w[1]             =   prmRollw1 
      e-item-vend.roll-w[2]             =   prmRollw2 
      e-item-vend.roll-w[3]             =   prmRollw3 
      e-item-vend.roll-w[4]             =   prmRollw4 
      e-item-vend.roll-w[5]             =   prmRollw5 
      e-item-vend.roll-w[6]             =   prmRollw6 
      e-item-vend.roll-w[7]             =   prmRollw7 
      e-item-vend.roll-w[8]             =   prmRollw8 
      e-item-vend.roll-w[9]             =   prmRollw9 
      e-item-vend.roll-w[10]            =   prmRollw10
      e-item-vend.roll-w[11]            =   prmRollw11
      e-item-vend.roll-w[12]            =   prmRollw12
      e-item-vend.roll-w[13]            =   prmRollw13
      e-item-vend.roll-w[14]            =   prmRollw14
      e-item-vend.roll-w[15]            =   prmRollw15
      e-item-vend.roll-w[16]            =   prmRollw16
      e-item-vend.roll-w[17]            =   prmRollw17
      e-item-vend.roll-w[18]            =   prmRollw18
      e-item-vend.roll-w[19]            =   prmRollw19
      e-item-vend.roll-w[20]            =   prmRollw20
      e-item-vend.roll-w[21]            =   prmRollw21
      e-item-vend.roll-w[22]            =   prmRollw22
      e-item-vend.roll-w[23]            =   prmRollw23
      e-item-vend.roll-w[24]            =   prmRollw24
      e-item-vend.roll-w[25]            =   prmRollw25
      e-item-vend.roll-w[26]            =   prmRollw26
      e-item-vend.roll-w[27]            =   prmRollw27
      e-item-vend.roll-w[28]            =   prmRollw28
      e-item-vend.roll-w[29]            =   prmRollw29
      e-item-vend.roll-w[30]            =   prmRollw30  .
    
        ASSIGN
            prmRowid2 = e-item-vend.rec_key.
        IF AVAIL ITEM AND ITEM.industry EQ "2" AND ITEM.mat-type EQ "B" THEN DO:
           FIND FIRST reftable WHERE reftable.rec_key  EQ e-item-vend.rec_key 
               AND reftable.reftable EQ "e-item-vend.adders" EXCLUSIVE-LOCK  NO-ERROR.
           IF NOT AVAIL reftable THEN DO:
               CREATE reftable.
               ASSIGN
                   reftable.rec_key  = e-item-vend.rec_key
                   reftable.reftable = "e-item-vend.adders"
                   reftable.company  = e-item-vend.company.
               END.
               IF AVAIL  reftable  THEN DO:
                   ASSIGN
                       reftable.val[1] = prmWidthmin  * 10000     
                       reftable.val[2] = prmLengthmin * 10000    
                       reftable.val[3] = prmWidthcst  * 10000     
                       reftable.val[4] = prmLengthcst * 10000.  
               end.

        END. /* avail item and mat-type */
          

        END.
        ASSIGN
            prmAction = "View" .

      END. /* end of save*/

      IF prmAction = "Select" THEN DO:
          
          FIND FIRST ITEM WHERE
                          ITEM.company = prmComp AND
          ITEM.i-no = prmItem
            NO-LOCK NO-ERROR.

  IF AVAILABLE ITEM THEN DO:

    FIND FIRST e-item WHERE
      e-item.company = item.company AND
      e-item.i-no = prmItem
      EXCLUSIVE-LOCK NO-ERROR.
    
    if not avail e-item then do:
      create e-item.
      assign e-item.company = item.company
             e-item.loc = item.loc
             e-item.i-no = item.i-no
             e-item.std-uom = item.pur-uom.
    end.
   
    FIND FIRST e-item-vend WHERE
         e-item-vend.company = item.company AND
         e-item-vend.i-no = prmItem AND
         e-item-vend.vend-no = ""
         EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL e-item-vend THEN DO:
        IF e-item-vend.rec_key = "" THEN DO:
            DELETE e-item-vend .

            create e-item-vend.
            assign
                e-item-vend.company = e-item.company
                e-item-vend.i-no = e-item.i-no
                e-item-vend.vend-no = "".
        END.
    END.
   
    IF NOT AVAILABLE e-item-vend THEN DO:
      create e-item-vend.
      assign e-item-vend.company = e-item.company
             e-item-vend.i-no = e-item.i-no
             e-item-vend.vend-no = "".
    END.

    find bf-item where recid(bf-item) = recid(item).

    if e-item-vend.setup = 0 and item.min-sqft <> 0 then 
      assign e-item-vend.setup = item.min-sqft
             bf-item.min-sqft = 0.
    do i = 1 to 10:
     assign e-item-vend.run-qty[i] = e-item.run-qty[i]
            e-item-vend.run-cost[i] = e-item.run-cost[i]
            .
    end.
   
   CREATE ttEItemVendor .
    ASSIGN
        ttEItemVendor.vITEM           = e-item-vend.i-no     
        ttEItemVendor.stdum           = e-item.std-uom
        ttEItemVendor.vend-item       = e-item-vend.vend-item
        ttEItemVendor.vend-no         = e-item-vend.vend-no
        ttEItemVendor.updated-date    = STRING(e-item-vend.updated-date[1])
        ttEItemVendor.run-qty[1]      = e-item-vend.run-qty[1]
        ttEItemVendor.run-qty[2]      = e-item-vend.run-qty[2]
        ttEItemVendor.run-qty[3]      = e-item-vend.run-qty[3]
        ttEItemVendor.run-qty[4]      = e-item-vend.run-qty[4]
        ttEItemVendor.run-qty[5]      = e-item-vend.run-qty[5]
        ttEItemVendor.run-qty[6]      = e-item-vend.run-qty[6]
        ttEItemVendor.run-qty[7]      = e-item-vend.run-qty[7]
        ttEItemVendor.run-qty[8]      = e-item-vend.run-qty[8]
        ttEItemVendor.run-qty[9]      = e-item-vend.run-qty[9]
        ttEItemVendor.run-qty[10]     = e-item-vend.run-qty[10]
        ttEItemVendor.setups[1]       = e-item-vend.setups[1]
        ttEItemVendor.setups[2]       = e-item-vend.setups[2]
        ttEItemVendor.setups[3]       = e-item-vend.setups[3]
        ttEItemVendor.setups[4]       = e-item-vend.setups[4]
        ttEItemVendor.setups[5]       = e-item-vend.setups[5]
        ttEItemVendor.setups[6]       = e-item-vend.setups[6]
        ttEItemVendor.setups[7]       = e-item-vend.setups[7]
        ttEItemVendor.setups[8]       = e-item-vend.setups[8]
        ttEItemVendor.setups[9]       = e-item-vend.setups[9]
        ttEItemVendor.setups[10]      = e-item-vend.setups[10]

        ttEItemVendor.run-cost[1]     = e-item-vend.run-cost[1]
        ttEItemVendor.run-cost[2]     = e-item-vend.run-cost[2]
        ttEItemVendor.run-cost[3]     = e-item-vend.run-cost[3]
        ttEItemVendor.run-cost[4]     = e-item-vend.run-cost[4]
        ttEItemVendor.run-cost[5]     = e-item-vend.run-cost[5]
        ttEItemVendor.run-cost[6]     = e-item-vend.run-cost[6]
        ttEItemVendor.run-cost[7]     = e-item-vend.run-cost[7]
        ttEItemVendor.run-cost[8]     = e-item-vend.run-cost[8]
        ttEItemVendor.run-cost[9]     = e-item-vend.run-cost[9]
        ttEItemVendor.run-cost[10]    = e-item-vend.run-cost[10]
        ttEItemVendor.roll-w[1]       = e-item-vend.roll-w[1]
        ttEItemVendor.roll-w[2]       = e-item-vend.roll-w[2]
        ttEItemVendor.roll-w[3]       = e-item-vend.roll-w[3]
        ttEItemVendor.roll-w[4]       = e-item-vend.roll-w[4]
        ttEItemVendor.roll-w[5]       = e-item-vend.roll-w[5]
        ttEItemVendor.roll-w[6]       = e-item-vend.roll-w[6]
        ttEItemVendor.roll-w[7]       = e-item-vend.roll-w[7]
        ttEItemVendor.roll-w[8]       = e-item-vend.roll-w[8]
        ttEItemVendor.roll-w[9]       = e-item-vend.roll-w[9]
        ttEItemVendor.roll-w[10]       = e-item-vend.roll-w[10]
        ttEItemVendor.roll-w[11]       = e-item-vend.roll-w[11]
        ttEItemVendor.roll-w[12]       = e-item-vend.roll-w[12]
        ttEItemVendor.roll-w[13]       = e-item-vend.roll-w[13]
        ttEItemVendor.roll-w[14]       = e-item-vend.roll-w[14]
        ttEItemVendor.roll-w[15]       = e-item-vend.roll-w[15]
        ttEItemVendor.roll-w[16]       = e-item-vend.roll-w[16]
        ttEItemVendor.roll-w[17]       = e-item-vend.roll-w[17]
        ttEItemVendor.roll-w[18]       = e-item-vend.roll-w[18]
        ttEItemVendor.roll-w[19]       = e-item-vend.roll-w[19]
        ttEItemVendor.roll-w[20]       = e-item-vend.roll-w[20]
        ttEItemVendor.roll-w[21]       = e-item-vend.roll-w[21]
        ttEItemVendor.roll-w[22]       = e-item-vend.roll-w[22]
        ttEItemVendor.roll-w[23]       = e-item-vend.roll-w[23]
        ttEItemVendor.roll-w[24]       = e-item-vend.roll-w[24]
        ttEItemVendor.roll-w[25]       = e-item-vend.roll-w[25]
        ttEItemVendor.roll-w[26]       = e-item-vend.roll-w[26]
        ttEItemVendor.roll-w[27]       = e-item-vend.roll-w[27]
        ttEItemVendor.roll-w[28]       = e-item-vend.roll-w[28]
        ttEItemVendor.roll-w[29]       = e-item-vend.roll-w[29]
        ttEItemVendor.roll-w[30]       = e-item-vend.roll-w[30]  
          
       ttEItemVendor.rowid-eitem      = STRING(e-item.rec_key)
        ttEItemVendor.rowid-vend       = STRING(e-item-vend.rec_key) .
     IF AVAIL ITEM AND ITEM.industry EQ "2" AND ITEM.mat-type EQ "B" THEN
         ASSIGN
            ttEItemVendor.showitem       = "Yes" .
  END.
END.



IF prmAction = "View" THEN DO:

    FIND FIRST ITEM WHERE
    ITEM.company = prmComp AND
    ITEM.i-no = prmItem
    NO-LOCK NO-ERROR.

  IF AVAILABLE ITEM THEN DO:

    FIND FIRST e-item WHERE
      e-item.company = item.company AND
      e-item.i-no = prmItem AND e-item.rec_key = prmRowid1
      EXCLUSIVE-LOCK NO-ERROR.
   
    
    FIND FIRST e-item-vend WHERE
         e-item-vend.company = item.company AND
         e-item-vend.i-no = prmItem AND e-item-vend.rec_key = prmRowid2
                EXCLUSIVE-LOCK NO-ERROR.
    
    
    CREATE ttEItemVendor .
    ASSIGN
        ttEItemVendor.vITEM           = e-item-vend.i-no     
        ttEItemVendor.stdum           = e-item.std-uom
        ttEItemVendor.vend-item       = e-item-vend.vend-item
        ttEItemVendor.vend-no         = e-item-vend.vend-no
        ttEItemVendor.updated-date    = STRING(e-item-vend.updated-date[1])
        ttEItemVendor.run-qty[1]      = e-item-vend.run-qty[1]
        ttEItemVendor.run-qty[2]      = e-item-vend.run-qty[2]
        ttEItemVendor.run-qty[3]      = e-item-vend.run-qty[3]
        ttEItemVendor.run-qty[4]      = e-item-vend.run-qty[4]
        ttEItemVendor.run-qty[5]      = e-item-vend.run-qty[5]
        ttEItemVendor.run-qty[6]      = e-item-vend.run-qty[6]
        ttEItemVendor.run-qty[7]      = e-item-vend.run-qty[7]
        ttEItemVendor.run-qty[8]      = e-item-vend.run-qty[8]
        ttEItemVendor.run-qty[9]      = e-item-vend.run-qty[9]
        ttEItemVendor.run-qty[10]     = e-item-vend.run-qty[10]
        ttEItemVendor.setups[1]       = e-item-vend.setups[1]
        ttEItemVendor.setups[2]       = e-item-vend.setups[2]
        ttEItemVendor.setups[3]       = e-item-vend.setups[3]
        ttEItemVendor.setups[4]       = e-item-vend.setups[4]
        ttEItemVendor.setups[5]       = e-item-vend.setups[5]
        ttEItemVendor.setups[6]       = e-item-vend.setups[6]
        ttEItemVendor.setups[7]       = e-item-vend.setups[7]
        ttEItemVendor.setups[8]       = e-item-vend.setups[8]
        ttEItemVendor.setups[9]       = e-item-vend.setups[9]
        ttEItemVendor.setups[10]      = e-item-vend.setups[10]

        ttEItemVendor.run-cost[1]     = e-item-vend.run-cost[1]
        ttEItemVendor.run-cost[2]     = e-item-vend.run-cost[2]
        ttEItemVendor.run-cost[3]     = e-item-vend.run-cost[3]
        ttEItemVendor.run-cost[4]     = e-item-vend.run-cost[4]
        ttEItemVendor.run-cost[5]     = e-item-vend.run-cost[5]
        ttEItemVendor.run-cost[6]     = e-item-vend.run-cost[6]
        ttEItemVendor.run-cost[7]     = e-item-vend.run-cost[7]
        ttEItemVendor.run-cost[8]     = e-item-vend.run-cost[8]
        ttEItemVendor.run-cost[9]     = e-item-vend.run-cost[9]
        ttEItemVendor.run-cost[10]    = e-item-vend.run-cost[10]
        ttEItemVendor.roll-w[1]       = e-item-vend.roll-w[1]
        ttEItemVendor.roll-w[2]       = e-item-vend.roll-w[2]
        ttEItemVendor.roll-w[3]       = e-item-vend.roll-w[3]
        ttEItemVendor.roll-w[4]       = e-item-vend.roll-w[4]
        ttEItemVendor.roll-w[5]       = e-item-vend.roll-w[5]
        ttEItemVendor.roll-w[6]       = e-item-vend.roll-w[6]
        ttEItemVendor.roll-w[7]       = e-item-vend.roll-w[7]
        ttEItemVendor.roll-w[8]       = e-item-vend.roll-w[8]
        ttEItemVendor.roll-w[9]       = e-item-vend.roll-w[9]
        ttEItemVendor.roll-w[10]       = e-item-vend.roll-w[10]
        ttEItemVendor.roll-w[11]       = e-item-vend.roll-w[11]
        ttEItemVendor.roll-w[12]       = e-item-vend.roll-w[12]
        ttEItemVendor.roll-w[13]       = e-item-vend.roll-w[13]
        ttEItemVendor.roll-w[14]       = e-item-vend.roll-w[14]
        ttEItemVendor.roll-w[15]       = e-item-vend.roll-w[15]
        ttEItemVendor.roll-w[16]       = e-item-vend.roll-w[16]
        ttEItemVendor.roll-w[17]       = e-item-vend.roll-w[17]
        ttEItemVendor.roll-w[18]       = e-item-vend.roll-w[18]
        ttEItemVendor.roll-w[19]       = e-item-vend.roll-w[19]
        ttEItemVendor.roll-w[20]       = e-item-vend.roll-w[20]
        ttEItemVendor.roll-w[21]       = e-item-vend.roll-w[21]
        ttEItemVendor.roll-w[22]       = e-item-vend.roll-w[22]
        ttEItemVendor.roll-w[23]       = e-item-vend.roll-w[23]
        ttEItemVendor.roll-w[24]       = e-item-vend.roll-w[24]
        ttEItemVendor.roll-w[25]       = e-item-vend.roll-w[25]
        ttEItemVendor.roll-w[26]       = e-item-vend.roll-w[26]
        ttEItemVendor.roll-w[27]       = e-item-vend.roll-w[27]
        ttEItemVendor.roll-w[28]       = e-item-vend.roll-w[28]
        ttEItemVendor.roll-w[29]       = e-item-vend.roll-w[29]
        ttEItemVendor.roll-w[30]       = e-item-vend.roll-w[30]  
          
        ttEItemVendor.rowid-eitem      = STRING(e-item.rec_key)
        ttEItemVendor.rowid-vend       = STRING(e-item-vend.rec_key) .

    FIND FIRST reftable WHERE reftable.rec_key  EQ e-item-vend.rec_key 
      AND reftable.reftable EQ "e-item-vend.adders" NO-LOCK  NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.rec_key  = e-item-vend.rec_key
       reftable.reftable = "e-item-vend.adders"
       reftable.company  = e-item-vend.company.
    END.
    IF AVAIL  reftable  THEN DO:
        ASSIGN
            ttEItemVendor.width-min       = reftable.val[1] / 10000
            ttEItemVendor.length-min      = reftable.val[2] / 10000
            ttEItemVendor.width-cst       = reftable.val[3] / 10000
            ttEItemVendor.length-cst      = reftable.val[4] / 10000  .
    END.


     IF AVAIL ITEM AND ITEM.industry EQ "2" AND ITEM.mat-type EQ "B" THEN
          ASSIGN ttEItemVendor.showitem = "Yes"  .

     
         
     FIND FIRST vend NO-LOCK
        WHERE vend.company EQ prmComp
          AND vend.vend-no EQ e-item-vend.vend-no NO-ERROR.
    ttEItemVendor.vend-name = IF AVAIL vend THEN vend.name ELSE "".

  END.
END.
