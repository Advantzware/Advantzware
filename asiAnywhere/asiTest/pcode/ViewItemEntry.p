                                              



/*------------------------------------------------------------------------
    File        : ViewItemEntry.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttViewItemEntry NO-UNDO
    FIELD abc AS CHAR
    FIELD OrdNo         AS INT 
    FIELD est-no LIKE oe-ordl.est-no
    FIELD job-no LIKE oe-ordl.job-no
    FIELD vLine LIKE oe-ordl.LINE
    FIELD CustPart      AS CHAR FORMAT "x(15)"
    FIELD Item1         AS CHAR FORMAT "x(15)"
    FIELD Name1         AS CHAR FORMAT "x(30)"
    FIELD Dscr          AS CHAR FORMAT "x(30)"
    FIELD Dscr2          AS CHAR FORMAT "x(30)"
    FIELD Dscr3          AS CHAR FORMAT "x(30)"
    FIELD quantity      AS DECIMAL FORMAT "->>,>>>,>>99.9<<"
    FIELD price         AS DECIMAL FORMAT "->>,>>>,>>9.99<<"
    FIELD uom           AS CHAR FORMAT "X(4)"
    FIELD counter       AS INTEGER 
    FIELD custpo        AS CHAR FORMAT "x(15)"
    FIELD taxable       AS LOGICAl Initial true 
    FIELD discount      AS DECIMAL FORMAT ">>>,>>9.99" 
    FIELD requested     AS CHAR FORMAT "x(2)"
    FIELD requestdate   AS DATE FORMAT "99/99/9999"
    FIELD extprice      AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD promised      AS CHAR FORMAT ">>>>9"
    FIELD promisdate    AS DATE FORMAT "99/99/9999"
    FIELD shipqty       AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD alloc         AS LOGICAL Initial true 
    FIELD ord-level     AS Decimal FORMAT ">>>,>>>,>>9.999"
    FIELD q-ono         AS Decimal FORMAT  "->>,>>>,>>9.999 "  
    FIELD q-onh         AS Decimal FORMAT "->>,>>>,>>9.999"  
    FIELD q-alloc       AS Decimal FORMAT "->>,>>>,>>9.999"  
    FIELD q-avail       AS Decimal FORMAT "->>,>>>,>>9.999"     
    FIELD q-back        AS Decimal FORMAT "->>,>>>,>>9.999"    
    FIELD vPartial      AS Decimal 
    FIELD VcasUnit      AS INT
    FIELD vEnum         AS INT
    FIELD vPono         AS INT
    FIELD vSman         AS CHAR
    FIELD vSpct         AS Decimal
    FIELD vScomm        AS Decimal 
    FIELD vSman2        AS CHAR
    FIELD vSpct2        AS Decimal
    FIELD vScomm2        AS Decimal
    FIELD vSman3        AS CHAR
    FIELD vSpct3        AS Decimal
    FIELD vScomm3       AS Decimal
    FIELD vType         AS CHAR
    FIELD vOver         AS Decimal
    FIELD vUnder        AS Decimal
    FIELD vManag   AS LOGICAL
    FIELD vBoardVen AS CHAR
    FIELD vSname    AS CHAR
    FIELD vSname2    AS CHAR   
    FIELD vSname3    AS CHAR   
   . 
DEFINE DATASET dsViewItemEntry FOR ttViewItemEntry .

DEFINE INPUT PARAMETER prmUser        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum    as INT no-undo.
DEFINE INPUT PARAMETER prmLine        as INT no-undo.
   
DEFINE INPUT PARAMETER prmEstimate    AS CHAR NO-UNDO.     
DEFINE INPUT PARAMETER prmItemNum     AS CHAR NO-UNDO.           
DEFINE INPUT PARAMETER prmPartNum     AS CHAR NO-UNDO.          
DEFINE INPUT PARAMETER prmQty         AS DECIMAL NO-UNDO.    
DEFINE INPUT PARAMETER prmItemName    AS CHAR NO-UNDO.          
DEFINE INPUT PARAMETER prmPartdscr     AS CHAR NO-UNDO.         
DEFINE INPUT PARAMETER prmPartdscr1     AS CHAR NO-UNDO.        
DEFINE INPUT PARAMETER prmPartdscr2     AS CHAR NO-UNDO.        
DEFINE INPUT PARAMETER prmPrice         AS DECIMAL NO-UNDO.      
DEFINE INPUT PARAMETER prmUom           AS CHAR NO-UNDO.      
DEFINE INPUT PARAMETER prmTax           AS CHAR NO-UNDO.   
DEFINE INPUT PARAMETER prmPoNum         AS CHAR NO-UNDO.   
DEFINE INPUT PARAMETER prmJob           AS CHAR NO-UNDO.   
DEFINE INPUT PARAMETER prmDiscount      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmCode          AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmReqDate       AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmTPrice        AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmPromCode      AS CHAR NO-UNDO.    
DEFINE INPUT PARAMETER prmPromDate      AS DATE NO-UNDO.        
DEFINE INPUT PARAMETER prmShip          AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmCas           AS INTEGER NO-UNDO.  
DEFINE INPUT PARAMETER prmPartial       AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmUnit          AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmEnum          AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPrevOrder     AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmSman          AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman2         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman3         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmType          AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmOver          AS DECIMAL NO-UNDO.   
DEFINE INPUT PARAMETER prmUnder         AS DECIMAL NO-UNDO.   
DEFINE INPUT PARAMETER prmVend          AS CHAR NO-UNDO.   
DEFINE INPUT PARAMETER prmManag         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmLn         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSname         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSname2         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSname3         AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewItemEntry.
DEFINE OUTPUT PARAMETER cError         AS CHAR NO-UNDO.
def var v-duelist as cha init "ASAP,NB4,MUST,HOT,RUSH,WO,HOLD,CR,BY,ON,MH,$$$,AM,INK,OE,RWRK,TOOL,HFR" no-undo.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR vLine AS INT NO-UNDO.
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = 0.
IF prmLine = ? THEN ASSIGN prmLine = 0.
IF prmEstimate     = ? THEN ASSIGN  prmEstimate  = "".  
IF prmItemNum      = ? THEN ASSIGN  prmItemNum   = "".     
IF prmPartNum      = ? THEN ASSIGN  prmPartNum   = "".    
IF prmQty          = ? THEN ASSIGN  prmQty       = 0.    
IF prmItemName     = ? THEN ASSIGN  prmItemName  = "".    
IF prmPartdscr     = ? THEN ASSIGN  prmPartdscr  = "".    
IF prmPartdscr1    = ? THEN ASSIGN  prmPartdscr1 = "".     
IF prmPartdscr2    = ? THEN ASSIGN  prmPartdscr2  = "".     
IF prmPrice        = ? THEN ASSIGN  prmPrice      = 0.     
IF prmUom          = ? THEN ASSIGN  prmUom        = "".     
IF prmTax          = ? THEN ASSIGN  prmTax        = "".     
IF prmPoNum        = ? THEN ASSIGN  prmPoNum      = "".     
IF prmJob          = ? THEN ASSIGN  prmJob        = "".                       
IF prmDiscount     = ? THEN ASSIGN  prmDiscount   = 0.                       
IF prmCode         = ? THEN ASSIGN  prmCode        = "".   

IF prmTPrice       = ? THEN ASSIGN  prmTPrice      = 0.    
IF prmPromCode     = ? THEN ASSIGN  prmPromCode    = "".    

IF prmShip         = ? THEN ASSIGN  prmShip        = 0.                      
IF prmCas          = ? THEN ASSIGN  prmCas         = 0.    
IF prmPartial      = ? THEN ASSIGN  prmPartial      = 0.   
IF prmUnit         = ? THEN ASSIGN  prmUnit         = 0.   
IF prmEnum         = ? THEN ASSIGN  prmEnum         = 0.   
IF prmPrevOrder    = ? THEN ASSIGN  prmPrevOrder    = 0.                     
IF prmSman         = ? THEN ASSIGN  prmSman         = "".                     
IF prmSman2        = ? THEN ASSIGN  prmSman2        = "".   
IF prmSman3        = ? THEN ASSIGN  prmSman3        = "".   
IF prmType         = ? THEN ASSIGN  prmType         = "".   
IF prmOver         = ? THEN ASSIGN  prmOver         = 0.   
IF prmUnder        = ? THEN ASSIGN  prmUnder        = 0.                     
IF prmVend         = ? THEN ASSIGN  prmVend         = "".                     
IF prmManag        = ? THEN ASSIGN  prmManag        = "".   
IF prmLn           = ? THEN ASSIGN  prmLn           = "".   
IF prmSname        = ? THEN ASSIGN  prmSname        = "".  
IF prmSname2        = ? THEN ASSIGN  prmSname2        = "".  
IF prmSname3        = ? THEN ASSIGN  prmSname3        = "".  
DEF BUFFER bf-ordl FOR oe-ordl.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF VAR lv-t-price AS DECIMAL NO-UNDO.
DEF VAR v-tmp-price AS DECIMAL NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
MESSAGE "test".
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/*Validate logic*/  
IF prmAction = "Add" THEN DO:
    FIND FIRST  oe-ord WHERE oe-ord.company = prmComp AND oe-ord.ord-no = prmOrderNum NO-LOCK NO-ERROR.
    FIND FIRST itemfg WHERE itemfg.company = prmComp AND (itemfg.i-no = prmItemNum OR prmItemNum = "") NO-LOCK NO-ERROR.
    if not avail itemfg THEN DO:
        ASSIGN cError =  "Invalid FG Item#. Try help. " .
        RETURN.
        
   end. /* not avail */ 
   if  date(prmReqDate) < oe-ord.ord-date THEN do:
       ASSIGN cError = "Due Date cannot be earlier than order date..." .
       RETURN.  
       END.
    if date(prmPromDate) < oe-ord.ord-date then do:
       ASSIGN
           cError = "Scheduled Date cannot be earlier than order date..." .
        RETURN.  
    end.
    if index(v-duelist,prmCode) <= 0 then do:
       ASSIGN cError = "Invalid Priority Code. " .
      RETURN.
    end.
    if index(v-duelist,prmPromCode) <= 0 then do:
       cError = "Invalid Priority Code. " .
      RETURN.
    end.


   find first cust-part where cust-part.company = prmComp and cust-part.part-no = prmPartNum
    no-lock no-error.
        if not avail cust-part then do:
           ASSIGN cError = "Invalid Cust Part#. Try help. " .
           RETURN.       
    end. 

    IF prmSman <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:
        ASSIGN cError = "Invalid Salesman".
        RETURN.
    END.
    END.
    IF prmSman2 <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman2  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sman THEN DO:
       ASSIGN cError = "Invalid Salesman".
   END.
   END.
   IF prmSman3 <> "" THEN DO:
   FIND sman WHERE sman.sman = prmSman3   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sman THEN DO:
          ASSIGN cError = "Invalid Salesman".
          RETURN.
       END.
   END.
   IF prmUom <> "" THEN DO:
       FIND uom WHERE uom.uom = prmUom   NO-LOCK NO-ERROR.
       IF NOT AVAILABLE uom THEN DO:
           ASSIGN cError = "Invalid Uom".
           RETURN.
        END.
    END.

 IF prmPrevOrder <> 0 THEN DO:
   FIND po-ord WHERE po-ord.po-no = prmPrevOrder   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE po-ord THEN DO:

       ASSIGN cError = "Invalid Board Po#".
       RETURN.
      END.

      END.


END.   /*IF prmAction = "Add" THEN DO:*/     

/* ********************  Preprocessor Definitions  ******************** */
IF prmAction = "Delete"  THEN DO:
   FIND FIRST bf-ordl WHERE bf-ordl.company EQ prmComp AND
            bf-ordl.ord-no = prmOrderNum AND bf-ordl.LINE = prmLine EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL bf-ordl THEN DO:
       DELETE bf-ordl.
   END.
    FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK :
    FIND LAST oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrderNum   NO-LOCK NO-ERROR.
    ASSIGN prmOrderNum = oe-ordl.ord-no 
        prmLine     = oe-ordl.LINE
          prmAction = "Select".
    END.
END. /*IF prmAction = "delete"*/      


/**********************************************************************************************************/

IF prmAction = "Add" THEN DO:

    FIND LAST oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrderNum NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl THEN DO:
        ASSIGN    vLine = oe-ordl.LINE + 1.
    END.
    ELSE DO:
        ASSIGN  vLine = 1.
    END.
    /******************************************************************************/
    FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrderNum NO-LOCK NO-ERROR.
      FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
  prmDiscount = cust.disc.
   v-tmp-price = if prmUom begins "L" AND prmUom NE "LB" then
                   if prmQty lt 0 then -1 else 1
                 else
                 if prmUom eq "CS" then
                   prmQty / (if prmCas ne 0 THEN prmCas else
                                    if  prmCas ne 0
                                                   then prmCas else
                                                        1)
                 else
                 if prmUom eq "C" then
                    prmQty / 100
                 else
                 if  prmUom eq "M" then
                    prmQty / 1000
                 else
                    prmQty.
                            
    lv-t-price = v-tmp-price * prmPrice.
    prmTPrice = (lv-t-price - ROUND(lv-t-price * cust.disc / 100,2)).
    
    /************************************************************/

    MESSAGE "test2" prmComp prmOrderNum vLine . 
    CREATE bf-ordl.
    ASSIGN
             bf-ordl.ord-no               = prmOrderNum
             bf-ordl.company              = prmComp
             bf-ordl.LINE                 = vLine
             bf-ordl.est-no               = prmEstimate 
             bf-ordl.i-no                 = prmItemNum
             bf-ordl.part-no              = prmPartNum 
             bf-ordl.qty                  = prmQty         
             bf-ordl.i-name               = prmItemName    
             bf-ordl.part-dscr1           = prmPartdscr    
             bf-ordl.part-dscr2           = prmPartdscr1   
             bf-ordl.part-dscr3           = prmPartdscr2   
             bf-ordl.price                = prmPrice 
             bf-ordl.t-price              = prmTPrice
             bf-ordl.pr-uom               = prmUom         
             bf-ordl.tax                  = IF prmTax = "yes" THEN TRUE ELSE FALSE    
             bf-ordl.po-no                = prmPoNum       
             bf-ordl.job-no               = prmJob
             bf-ordl.disc                 = prmDiscount            
             bf-ordl.req-code             = prmCode
             bf-ordl.req-date             = prmReqDate  
             bf-ordl.prom-code            = prmPromCode    
             bf-ordl.prom-date            = prmPromDate   
             bf-ordl.ship-qty             = prmShip        
             bf-ordl.cas-cnt              = prmCas         
             bf-ordl.partial              = prmPartial     
             bf-ordl.cases-unit           = prmUnit        
             bf-ordl.e-num                = prmEnum         
             bf-ordl.po-no-po             = prmPrevOrder   
             bf-ordl.s-man[1]             = prmSman       
             bf-ordl.s-man[2]             = prmSman2       
             bf-ordl.s-man[3]             = prmSman3       
             bf-ordl.type-code            = prmType        
             bf-ordl.over-pct             = prmOver        
             bf-ordl.under-pct            = prmUnder        
             bf-ordl.vend-no              = prmVend        
             bf-ordl.whsed                = IF prmManag = "yes" THEN TRUE ELSE FALSE 
             .


             ASSIGN prmLine = vLine
                 prmAction = "Select".
          
       MESSAGE "addtest" prmAction  prmComp prmItemNum.
      
        
END.

/***************************************************************************************************************/

IF prmAction = "Update" THEN DO:
    FIND FIRST  oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST  oe-ord WHERE oe-ord.company = oe-ordl.company AND oe-ordl.ord-no = oe-ordl.ord-no EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST itemfg WHERE itemfg.company = oe-ordl.company AND (itemfg.i-no = prmItemNum OR prmItemNum = "")NO-LOCK NO-ERROR.
if not avail itemfg THEN DO:
     ASSIGN cError =  "Invalid FG Item#. Try help. " .
     RETURN.
             
   end. /* not avail */
   if  date(prmReqDate) < oe-ord.ord-date THEN do:
       ASSIGN cError = "Due Date cannot be earlier than order date..." .
      RETURN.
       END.
    if date(prmPromDate) < oe-ord.ord-date then do:
       ASSIGN
           cError = "Scheduled Date cannot be earlier than order date..." .
       RETURN.
      
    end.
    if index(v-duelist,prmCode) <= 0 then do:
       ASSIGN cError = "Invalid Priority Code. " .
       RETURN.
      
    end.

    if index(v-duelist,prmPromCode) <= 0 then do:
       cError = "Invalid Priority Code. " .
       RETURN.
      
    end.

    
  

         /*
         else do:
            find first cust where cust.company = oe-ord.company
                              and cust.cust-no = oe-ord.cust-no no-lock no-error.
            if itemfg.cust-no ne oe-ord.cust-no and itemfg.cust-no ne "" and
               avail cust and cust.active ne "X"                         then do:
               find first cust where cust.company = prmComp and
                                     cust.cust-no = itemfg.cust-no
                                     no-lock no-error.
               if avail cust and cust.active ne "X" then do:                      
                  cError =  "This item exists for a different customer!. Do you want to continue?".
                  
               end.  
            end.  
         END.
         
*/
          IF prmSman <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:
     ASSIGN cError = "Invalid Salesman".
     RETURN.
    END.
    END.

    IF prmSman2 <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman2  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sman THEN DO:
   ASSIGN cError = "Invalid Salesman".
   RETURN.
   END.
   END.

IF prmSman3 <> "" THEN DO:
   FIND sman WHERE sman.sman = prmSman3   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sman THEN DO:
      ASSIGN cError = "Invalid Salesman".
      RETURN.
      END.
      END.

IF prmUom <> "" THEN DO:
   FIND uom WHERE uom.uom = prmUom   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE uom THEN DO:
       ASSIGN cError = "Invalid Uom".
       RETURN.
      END.
      END.

  IF prmPrevOrder <> 0 THEN DO:
   FIND po-ord WHERE po-ord.po-no = prmPrevOrder   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE po-ord THEN DO:
       ASSIGN cError = "Invalid Board Po#".
       RETURN.
      END.
      END.

END.   /*IF prmAction = "Update" THEN DO:*/

/*************************************************************************************************************/
IF prmAction = "Update" THEN DO:
    FIND FIRST oe-ordl where oe-ordl.company EQ prmComp AND
              oe-ordl.ord-no = prmOrderNum AND
              oe-ordl.LINE = prmLine
             EXCLUSIVE-LOCK NO-ERROR.
     FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
  prmDiscount = cust.disc.
   v-tmp-price = if prmUom begins "L" AND prmUom NE "LB" then
                   if prmQty lt 0 then -1 else 1
                 else
                 if prmUom eq "CS" then
                   prmQty / (if prmCas ne 0 THEN prmCas else
                                    if  prmCas ne 0
                                                   then prmCas else
                                                        1)
                 else
                 if prmUom eq "C" then
                    prmQty / 100
                 else
                 if  prmUom eq "M" then
                    prmQty / 1000
                 else
                    prmQty.
                            
    lv-t-price = v-tmp-price * prmPrice.
    prmTPrice = (lv-t-price - ROUND(lv-t-price * cust.disc / 100,2)).
    IF AVAIL oe-ordl THEN DO:
        ASSIGN
            oe-ordl.est-no               = prmEstimate 
            oe-ordl.i-no                 = prmItemNum
            oe-ordl.part-no              = prmPartNum 
            oe-ordl.qty                  = prmQty         
            oe-ordl.i-name               = prmItemName    
            oe-ordl.part-dscr1           = prmPartdscr    
            oe-ordl.part-dscr2           = prmPartdscr1   
            oe-ordl.part-dscr3           = prmPartdscr2   
            oe-ordl.price                = prmPrice 
            oe-ordl.t-price              = prmTPrice
            oe-ordl.pr-uom               = prmUom         
            oe-ordl.tax                  = IF prmTax = "yes" THEN TRUE ELSE FALSE    
            oe-ordl.po-no                = prmPoNum       
            oe-ordl.job-no               = prmJob
            oe-ordl.disc                 = prmDiscount            
            oe-ordl.req-code             = prmCode
            oe-ordl.req-date             = prmReqDate  
            oe-ordl.prom-code            = prmPromCode    
            oe-ordl.prom-date            = prmPromDate   
            oe-ordl.ship-qty             = prmShip        
            oe-ordl.cas-cnt              = prmCas         
            oe-ordl.partial              = prmPartial     
            oe-ordl.cases-unit           = prmUnit        
            oe-ordl.e-num                = prmEnum         
            oe-ordl.po-no-po             = prmPrevOrder   
            oe-ordl.s-man[1]             = prmSman       
            oe-ordl.s-man[2]             = prmSman2       
            oe-ordl.s-man[3]             = prmSman3       
            oe-ordl.type-code            = prmType        
            oe-ordl.over-pct             = prmOver        
            oe-ordl.under-pct            = prmUnder        
            oe-ordl.vend-no              = prmVend        
            oe-ordl.whsed                = IF prmManag = "yes" THEN TRUE ELSE FALSE 
            .

             ASSIGN prmAction = "Select".
             END.   /*for each*/
END.   /*prmAction = update**/
/******************************************************************************************************************/
IF prmAction = "Select" THEN DO:
FIND FIRST oe-ordl where oe-ordl.company EQ prmComp AND
             oe-ordl.ord-no = prmOrderNum AND
             oe-ordl.LINE = prmLine
             NO-LOCK NO-ERROR.

        IF AVAIL oe-ordl THEN DO:
            create ttViewItemEntry.
            assign 
                ttViewItemEntry.OrdNo          = oe-ordl.ord-no
                ttViewItemEntry.vLine          = oe-ordl.LINE
                ttViewItemEntry.est-no         = oe-ordl.est-no
                ttViewItemEntry.Item1          = oe-ordl.i-no
                ttViewItemEntry.CustPart       = oe-ordl.part-no
                ttViewItemEntry.quantity       = oe-ordl.qty
                ttViewItemEntry.Name1          = oe-ordl.i-name
                ttViewItemEntry.Dscr           = oe-ordl.part-dscr1 
                ttViewItemEntry.Dscr2          = oe-ordl.part-dscr2 
                ttViewItemEntry.Dscr3          = oe-ordl.part-dscr3
                ttViewItemEntry.price          = oe-ordl.price
                ttViewItemEntry.uom            = oe-ordl.pr-uom
                ttViewItemEntry.taxable        = oe-ordl.tax
                ttViewItemEntry.custpo         = oe-ordl.po-no
                ttViewItemEntry.job-no         = oe-ordl.job-no
                ttViewItemEntry.discount       = oe-ordl.disc
                ttViewItemEntry.requested      = oe-ordl.req-code
                ttViewItemEntry.requestdate    = oe-ordl.req-date
                ttViewItemEntry.extprice       = oe-ordl.t-price
                ttViewItemEntry.promised       = oe-ordl.prom-code
                ttViewItemEntry.promisdate     = oe-ordl.prom-date
                ttViewItemEntry.shipqty        = oe-ordl.ship-qty
                ttViewItemEntry.counter        = oe-ordl.cas-cnt
                ttViewItemEntry.vPartial       =  oe-ordl.partial
                ttViewItemEntry.VcasUnit       =  oe-ordl.cases-unit 
                ttViewItemEntry.vEnum          = oe-ordl.e-num 
                ttViewItemEntry.vPono          = oe-ordl.po-no-po 
                ttViewItemEntry.vSman          =  oe-ordl.s-man[1]
                ttViewItemEntry.vSpct          =  oe-ordl.s-pct[1]
                ttViewItemEntry.vScomm         = oe-ordl.s-comm[1]
                ttViewItemEntry.vSman2         =  oe-ordl.s-man[2]
                ttViewItemEntry.vSpct2         =  oe-ordl.s-pct[2]
                ttViewItemEntry.vScomm2        =  oe-ordl.s-comm[2]
                ttViewItemEntry.vSman3         = oe-ordl.s-man[3]
                ttViewItemEntry.vSpct3         =  oe-ordl.s-pct[3]
                ttViewItemEntry.vScomm3        = oe-ordl.s-comm[3]
                ttViewItemEntry.vType          = oe-ordl.type-code
                ttViewItemEntry.vOver          = oe-ordl.over-pct 
                ttViewItemEntry.vUnder         = oe-ordl.under-pct
                  ttViewItemEntry.vBoardVen    = oe-ordl.vend-no
                ttViewItemEntry.vManag         = oe-ordl.whsed
                                              .
            FOR EACH sman NO-LOCK :
                IF sman.sman = oe-ordl.s-man[1] THEN
                    ASSIGN ttViewItemEntry.vSname         = sman.sname .
                IF sman.sman = oe-ordl.s-man[2] THEN 
                    ASSIGN ttViewItemEntry.vSname2         = sman.sname.
                IF sman.sman = oe-ordl.s-man[3] THEN 
                    ASSIGN ttViewItemEntry.vSname3         = sman.sname.
            END.   /*for each sman */
MESSAGE "line" vLine.
END.   /*if avail oe-ordl*/
END.  /*PrmAction = Select*/



