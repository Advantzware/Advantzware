
 




/*------------------------------------------------------------------------
    File        : QuoteHand.p
    Purpose     : FGItem

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttQuoteHandLook NO-UNDO 
    FIELD vqno      AS INT
    FIELD vino      AS CHARACTER
    FIELD viname    AS CHARACTER
    FIELD vhand     AS DECIMAL
    FIELD vcust     AS CHARACTER
    FIELD vcustpart AS CHARACTER
    FIELD vdscr     AS CHARACTER
    FIELD vest      AS CHARACTER

    FIELD vpartdscr1     AS CHARACTER
    FIELD vpartdscr2     AS CHARACTER
    FIELD vprice         AS DECIMAL
    FIELD vuom           AS CHARACTER
    FIELD vtype          AS INT
    FIELD vdiscount      AS DECIMAL
    FIELD vqty           AS INT
    FIELD vqtyunit       AS INT 
    FIELD jjhff          AS CHAR
    .
                                           
    
DEFINE DATASET dsQuoteHandLook FOR ttQuoteHandLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuoteHandLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR vItem AS CHAR.
DEF VAR vName AS CHAR.
DEFINE VAR v-count AS INT NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmQuote     = ? THEN ASSIGN prmQuote     = 0.
IF prmCust      = ? THEN ASSIGN prmCust      = "".
IF prmItem      = ? THEN ASSIGN prmItem      = "".
 
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEFINE VAR custcount AS CHAR NO-UNDO.
/*
FOR EACH usercust WHERE
    usercust.user_id =  prmUser AND
    usercust.company = prmComp
     NO-LOCK:
    ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END. /* end of usercust*/  */
                                   
   
 IF prmAction = "HandQuote" THEN  DO:
     
     FOR EACH  quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote AND quotehd.cust-no = prmCust NO-LOCK :
     FIND FIRST  quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND (quoteitm.i-no = prmItem OR prmItem = "") AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
            create ttQuoteHandLook.
                   assign  
                       ttQuoteHandLook.vqno         = quotehd.q-no
                       ttQuoteHandLook.vest         = quotehd.est-no
                       ttQuoteHandLook.vcust        = quotehd.cust-no 
                       ttQuoteHandLook.vcustpart    = quoteitm.part-no
                       ttQuoteHandLook.vino         = quoteitm.i-no
                       ttQuoteHandLook.vuom         = quoteitm.uom
                       ttQuoteHandLook.vdscr        = quoteitm.part-dscr1
                       ttQuoteHandLook.vpartdscr1   = quoteitm.part-dscr2
                       ttQuoteHandLook.vpartdscr2   = quoteitm.part-dscr3
                       
                       ttQuoteHandLook.vprice       = quoteitm.price
                       ttQuoteHandLook.vqty         = quoteitm.qty 
                       ttQuoteHandLook.vtype        = quoteitm.est-type  .
                  

                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttQuoteHandLook.viname     = itemfg.i-name .
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttQuoteHandLook.vdiscount      = cust.disc .
                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1 NO-LOCK NO-ERROR.
                   IF AVAIL eb THEN DO:
                       ASSIGN
                           ttQuoteHandLook.vqtyunit      = eb.cas-cnt .
                   END.
                   IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttQuoteHandLook.vino         = eb.stock-no.
                    END.
                    IF ttQuoteHandLook.vprice = ? THEN ASSIGN ttQuoteHandLook.vprice = 0.
                    MESSAGE "testitem" ttQuoteHandLook.vprice  .
     END.
           
END. /*end of action*/
