
/*------------------------------------------------------------------------
    File        : ArViewInvoice.p
    Purpose     : ViewInvoice

     Main File   : ar\v-arinvl.w
    Syntax      :

    Description : Return a Dataset of all Invpice

    Author(s)   : 
    Created     :  oct 8 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttArViewInvlLine NO-UNDO
    FIELD arline      AS INT     
    FIELD actnum      AS CHAR         
    FIELD actdscr     AS CHAR     
    FIELD i-name      AS CHAR  
    FIELD i-dscr      AS CHAR        
    FIELD lot-no      AS CHAR        
    FIELD inv-qty     AS DECIMAL  
    FIELD cons-uom    AS CHAR
    FIELD sf-sht      AS DECIMAL
    FIELD unit-pr     AS DECIMAL
    FIELD pr-qty-uom  AS CHAR
    FIELD disc        AS DECIMAL
    FIELD cal-amt     AS DECIMAL
    FIELD amt-msf     AS DECIMAL
    FIELD cost        AS DECIMAL
    FIELD dscr1       AS CHAR
    FIELD sman1       AS CHAR
    FIELD s-pct1      AS DECIMAL
    FIELD s-comm1     AS DECIMAL
    FIELD sman2       AS CHAR
    FIELD s-pct2      AS DECIMAL
    FIELD s-comm2     AS DECIMAL
    FIELD sman3       AS CHAR
    FIELD s-pct3      AS DECIMAL
    FIELD s-comm3     AS DECIMAL
    FIELD x-no        AS INT
    FIELD reckey      AS CHARACTER

  
    .

DEFINE DATASET dsCustArViewInvlLine FOR ttArViewInvlLine.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInv      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmLine     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmActnum   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmActdscr  AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmIname    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmIdscr    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmLotno    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInvqty   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmConsuom  AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmSfsht    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmUnitpr   AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmQtyuom   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmDisc     AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmCalamt   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmAmtmsf   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmCost     AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmDscr1    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSman1    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSpct1    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmScomm1   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmSman2    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmSpct2    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmScomm2   AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSman3    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmSpct3    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmScomm3   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey   AS CHARACTER NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustArViewInvlLine .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.

     FOR EACH ttArViewInvlLine:
        DELETE ttArViewInvlLine .
    END.

IF prmAction       = ?  THEN ASSIGN prmAction    = "Select".
IF prmComp         = ?  THEN ASSIGN prmComp      = "".
IF prmUser         = ?  THEN ASSIGN prmUser      = "".
IF prmLine         = ?  THEN ASSIGN prmLine      = 0.
IF prmActnum       = ?  THEN ASSIGN prmActnum    = "".
IF prmActdscr      = ?  THEN ASSIGN prmActdscr   = "".
IF prmIname        = ?  THEN ASSIGN prmIname     = "". 
IF prmIdscr        = ?  THEN ASSIGN prmIdscr     = "".
IF prmLotno        = ?  THEN ASSIGN prmLotno     = "".
IF prmInvqty       = ?  THEN ASSIGN prmInvqty    = 0.
IF prmConsuom      = ?  THEN ASSIGN prmConsuom   = "".
IF prmSfsht        = ?  THEN ASSIGN prmSfsht     = 0.
IF prmUnitpr       = ?  THEN ASSIGN prmUnitpr    = 0.
IF prmQtyuom       = ?  THEN ASSIGN prmQtyuom    = "".
IF prmDisc         = ?  THEN ASSIGN prmDisc      = 0.
IF prmCalamt       = ?  THEN ASSIGN prmCalamt    = 0.
IF prmAmtmsf       = ?  THEN ASSIGN prmAmtmsf    = 0.
IF prmCost         = ?  THEN ASSIGN prmCost      = 0.
IF prmDscr1        = ?  THEN ASSIGN prmDscr1     = "".
IF prmSman1        = ?  THEN ASSIGN prmSman1     = "".
IF prmSpct1        = ?  THEN ASSIGN prmSpct1     = 0.
IF prmScomm1       = ?  THEN ASSIGN prmScomm1    = 0.
IF prmSman2        = ?  THEN ASSIGN prmSman2     = "".
IF prmSpct2        = ?  THEN ASSIGN prmSpct2     = 0.
IF prmScomm2       = ?  THEN ASSIGN prmScomm2    = 0.
IF prmSman3        = ?  THEN ASSIGN prmSman3     = "".
IF prmSpct3        = ?  THEN ASSIGN prmSpct3     = 0.
IF prmScomm3       = ?  THEN ASSIGN prmScomm3    = 0.


DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.

DEF VAR lv-uom-list AS cha INIT "EA,MSF,M" NO-UNDO.
{oe/oe-sysct1.i NEW}
DEF VAR ll-inquiry AS LOG NO-UNDO.
DEF VAR v-actdscr LIKE account.dscr NO-UNDO.

 DEF BUFFER bf-invl FOR ar-invl.
    DEF VAR li-next-line AS INT NO-UNDO.
    DEF VAR out-qty LIKE ar-invl.qty NO-UNDO.
  DEF BUFFER bf-inv FOR ar-inv.

 
     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

ASSIGN
    cocode = prmComp
    g_company = prmComp  .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode .



FUNCTION get-actdscr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  IF AVAIL ar-invl THEN DO:
     FIND FIRST account WHERE account.company = g_company
                          AND account.actnum = ar-invl.actnum NO-LOCK NO-ERROR.
     IF AVAIL account THEN RETURN account.dscr.
     ELSE RETURN "".
  END.
  ELSE RETURN "".   /* Function return value. */
END FUNCTION.

FUNCTION calc-amt RETURNS DECIMAL
  ( /* parameter-definitions */ ) :

  RETURN IF ll-inquiry AND AVAIL ar-invl          AND /* Function return value. */
            NOT ar-invl.billable AND ar-invl.misc THEN 0
                                                  ELSE ar-invl.amt.
END FUNCTION.

FUNCTION get-i-dscr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  RETURN IF AVAIL ar-invl AND ar-invl.i-dscr EQ "" THEN
           ar-invl.part-dscr1 ELSE ar-invl.i-dscr.   /* Function return value. */

END FUNCTION.



IF prmAction = "Select" THEN DO:
    FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.inv-no = prmInv  NO-LOCK NO-ERROR.
         FOR EACH ar-invl WHERE ar-invl.x-no = ar-inv.x-no NO-LOCK :
             CREATE ttArViewInvlLine.
             ASSIGN 
                 ttArViewInvlLine.arline        = ar-invl.LINE 
                 ttArViewInvlLine.actnum        = ar-invl.actnum
                 ttArViewInvlLine.actdscr       = get-actdscr()
                 ttArViewInvlLine.i-name        = ar-invl.i-name
                 ttArViewInvlLine.i-dscr        = get-i-dscr()
                 ttArViewInvlLine.lot-no        = ar-invl.lot-no
                 ttArViewInvlLine.inv-qty       = ar-invl.inv-qty 
                 ttArViewInvlLine.cons-uom      = ar-invl.cons-uom
                 ttArViewInvlLine.sf-sht        = ar-invl.sf-sht 
                 ttArViewInvlLine.unit-pr       = ar-invl.unit-pr
                 ttArViewInvlLine.pr-qty-uom    = ar-invl.pr-qty-uom
                 ttArViewInvlLine.disc          = ar-invl.disc
                 ttArViewInvlLine.cal-amt       = calc-amt() 
                 ttArViewInvlLine.amt-msf       =  ar-invl.amt-msf
                 ttArViewInvlLine.cost          = ar-invl.cost
                 ttArViewInvlLine.dscr1         = ar-invl.dscr[1]
                 ttArViewInvlLine.sman1         = ar-invl.sman[1]
                 ttArViewInvlLine.s-pct1        = ar-invl.s-pct[1]
                 ttArViewInvlLine.s-comm1       = ar-invl.s-comm[1]
                 ttArViewInvlLine.sman2         = ar-invl.sman[2]
                 ttArViewInvlLine.s-pct2        = ar-invl.s-pct[2]
                 ttArViewInvlLine.s-comm2       = ar-invl.s-comm[2]
                 ttArViewInvlLine.sman3         = ar-invl.sman[3]
                 ttArViewInvlLine.s-pct3        = ar-invl.s-pct[3]
                 ttArViewInvlLine.s-comm3       = ar-invl.s-comm[3]
                 ttArViewInvlLine.x-no          = ar-invl.x-no
                 ttArViewInvlLine.reckey        = ar-invl.rec_key
                 
                .
            
      END. /*FOR EACH buff-cust  */
      
END. /*IF prmAction = "Select" THEN DO:*/


/********************************Add **********************************/

IF prmAction = "AddNewLine" THEN DO:
    FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.inv-no = prmInv  NO-LOCK NO-ERROR.
  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST bf-invl WHERE bf-invl.x-no = ar-inv.x-no USE-INDEX x-no NO-LOCK NO-ERROR.
  li-next-line = IF AVAIL bf-invl THEN bf-invl.LINE + 1 ELSE 1.

 /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST ar-ctrl WHERE ar-ctrl.company = ar-inv.company NO-LOCK NO-ERROR.
   
  FIND FIRST cust WHERE
       cust.company EQ ar-inv.company AND
       cust.cust-no EQ ar-inv.cust-no
       NO-LOCK NO-ERROR.
    CREATE ar-invl .
  ASSIGN ar-invl.x-no = ar-inv.x-no
         ar-invl.company = ar-inv.company
         ar-invl.cust-no = ar-inv.cust-no
         ar-invl.inv-no = ar-inv.inv-no
         ar-invl.LINE = li-next-line
         ar-invl.po-no = ar-inv.po-no
         ar-invl.pr-qty-uom = "EA"
         ar-invl.cons-uom = "EA"
         ar-invl.dscr[1] = "EA"
         ar-invl.actnum = IF AVAIL ar-ctrl THEN ar-ctrl.sales ELSE ""
         ar-invl.sman[1] = IF AVAIL cust THEN cust.sman ELSE ""
         ar-invl.s-pct[1] = IF ar-invl.sman[1] NE "" THEN 100 ELSE 0.

         CREATE ttArViewInvlLine.
             ASSIGN 
                 ttArViewInvlLine.arline        = ar-invl.LINE 
                 ttArViewInvlLine.actnum        = ar-invl.actnum
                 ttArViewInvlLine.actdscr       = get-actdscr()
                 ttArViewInvlLine.cons-uom      = ar-invl.cons-uom
                 ttArViewInvlLine.pr-qty-uom    = ar-invl.pr-qty-uom
                 ttArViewInvlLine.dscr1         = ar-invl.dscr[1]
                 ttArViewInvlLine.sman1         = ar-invl.sman[1]
                 ttArViewInvlLine.s-pct1        = ar-invl.s-pct[1]  
                 ttArViewInvlLine.x-no          = ar-invl.x-no
                 ttArViewInvlLine.reckey        = ar-invl.rec_key  .

END.
    
/********************************************************************/


IF prmAction = "ValidateAdd" THEN DO:

     FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.inv-no = prmInv  NO-LOCK NO-ERROR.
     FIND FIRST ar-invl WHERE ar-invl.company = cocode AND ar-invl.inv-no = ar-inv.inv-no 
                                                       AND ar-invl.rec_key <> prmReckey
                                                       AND ar-invl.LINE = prmLine NO-LOCK NO-ERROR .
     IF AVAIL ar-invl THEN DO:
         cError = " Invoice already exists with inv# " + STRING(prmInv) + "  line " + STRING(prmLine) . 
         RETURN .
     END.

     IF ar-inv.posted THEN DO:
     cError =  "Invoice already posted. No Adding allowed!" .     
     RETURN .
     END.


   FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum =prmActnum
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          cError = "Invalid GL Account Number.".
          RETURN .
       END.
    IF LOOKUP(prmQtyuom ,lv-uom-list) <= 0 THEN DO:
       cError =  "Invalid Unit of Measure." .
       RETURN .
    END.   
    
    IF prmSman1 <> "" THEN DO:
        IF prmSman1 NE "" AND NOT CAN-FIND(FIRST sman WHERE
         sman.company EQ g_company AND
         sman.sman EQ prmSman1) THEN DO:
            cError =  "Invalid Sales Rep." .
            RETURN .
         END.
   END.

   IF prmSman2 NE "" AND NOT CAN-FIND(FIRST sman WHERE
         sman.company EQ g_company AND
         sman.sman EQ prmSman2) THEN DO:
            cError =  "Invalid Sales Rep." .
            RETURN .
   END.
   IF prmSman3 NE "" AND NOT CAN-FIND(FIRST sman WHERE
        sman.company EQ g_company AND
        sman.sman EQ prmSman3) THEN DO:
            cError =  "Invalid Sales Rep." .
            RETURN .
   END.

END.  /* end of validate add*/

IF prmAction = "Add" THEN DO:

     FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.inv-no = prmInv  NO-LOCK NO-ERROR.
     FIND FIRST ar-invl WHERE  ar-invl.company = cocode AND ar-invl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN
        ar-invl.line        =  prmLine   
        ar-invl.actnum      =  prmActnum      
        ar-invl.i-name      =  prmIname     
        ar-invl.i-dscr      =  prmIdscr     
        ar-invl.lot-no      =  prmLotno  
        ar-invl.inv-qty     =  prmInvqty 
        ar-invl.cons-uom    =  prmConsuom
        ar-invl.sf-sht      =  prmSfsht  
        ar-invl.unit-pr     =  prmUnitpr   
        ar-invl.pr-qty-uom  =  prmQtyuom 
        ar-invl.disc        =  prmDisc   
        /*ar-invl.amt         =  prmCalamt       */
        ar-invl.amt-msf     =  prmAmtmsf       
        ar-invl.cost        =  prmCost         
        ar-invl.dscr[1]     =  prmDscr1        
        ar-invl.sman[1]     =  prmSman1        
        ar-invl.s-pct[1]    =  prmSpct1   
        ar-invl.s-comm[1]   =  prmScomm1  
        ar-invl.sman[2]     =  prmSman2   
        ar-invl.s-pct[2]    =  prmSpct2   
        ar-invl.s-comm[2]   =  prmScomm2  
        ar-invl.sman[3]     =  prmSman3   
        ar-invl.s-pct[3]    =  prmSpct3   
        ar-invl.s-comm[3]   =  prmScomm3  .

    RUN local-assign-record .

      ASSIGN
      prmAction = "View"
      prmLine   = ar-invl.line   .


END.


/**************Update *************************************************/

IF prmAction = "ValidateUpdate" THEN DO:

     FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.inv-no = prmInv  NO-LOCK NO-ERROR.
     FIND FIRST ar-invl WHERE ar-invl.company = cocode AND ar-invl.inv-no = ar-inv.inv-no 
                                                       AND ar-invl.rec_key <> prmReckey
                                                       AND ar-invl.LINE = prmLine NO-LOCK NO-ERROR .
     IF AVAIL ar-invl THEN DO:
         cError = " Invoice already exists with inv# " + STRING(prmInv) + "  line " + STRING(prmLine) . 
         RETURN .
     END.

     IF ar-inv.posted THEN DO:
     cError =  "Invoice already posted. No Editing allowed!" .
     RETURN .
      END.

   FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum =prmActnum
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          cError = "Invalid GL Account Number.".
          RETURN .
       END.
    IF LOOKUP(prmQtyuom ,lv-uom-list) <= 0 THEN DO:
       cError =  "Invalid Unit of Measure." .
       RETURN .
    END.   
    
    IF prmSman1 <> "" THEN DO:
        IF prmSman1 NE "" AND NOT CAN-FIND(FIRST sman WHERE
         sman.company EQ g_company AND
         sman.sman EQ prmSman1) THEN DO:
            cError =  "Invalid Sales Rep." .
            RETURN .
         END.
   END.

   IF prmSman2 NE "" AND NOT CAN-FIND(FIRST sman WHERE
         sman.company EQ g_company AND
         sman.sman EQ prmSman2) THEN DO:
            cError =  "Invalid Sales Rep." .
            RETURN .
   END.
   IF prmSman3 NE "" AND NOT CAN-FIND(FIRST sman WHERE
        sman.company EQ g_company AND
        sman.sman EQ prmSman3) THEN DO:
            cError =  "Invalid Sales Rep." .
            RETURN .
   END.

END.


IF prmAction = "Update" THEN DO:
     FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.inv-no = prmInv  NO-LOCK NO-ERROR.
      FIND FIRST ar-invl WHERE  ar-invl.company = cocode AND ar-invl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN
        ar-invl.line        =  prmLine   
        ar-invl.actnum      =  prmActnum      
        ar-invl.i-name      =  prmIname     
        ar-invl.i-dscr      =  prmIdscr     
        ar-invl.lot-no      =  prmLotno  
        ar-invl.inv-qty     =  prmInvqty 
        ar-invl.cons-uom    =  prmConsuom
        ar-invl.sf-sht      =  prmSfsht  
        ar-invl.unit-pr     =  prmUnitpr   
        ar-invl.pr-qty-uom  =  prmQtyuom 
        ar-invl.disc        =  prmDisc   
        ar-invl.amt         =  prmCalamt       
        ar-invl.amt-msf     =  prmAmtmsf       
        ar-invl.cost        =  prmCost         
        ar-invl.dscr[1]     =  prmDscr1        
        ar-invl.sman[1]     =  prmSman1        
        ar-invl.s-pct[1]    =  prmSpct1   
        ar-invl.s-comm[1]   =  prmScomm1  
        ar-invl.sman[2]     =  prmSman2   
        ar-invl.s-pct[2]    =  prmSpct2   
        ar-invl.s-comm[2]   =  prmScomm2  
        ar-invl.sman[3]     =  prmSman3   
        ar-invl.s-pct[3]    =  prmSpct3   
        ar-invl.s-comm[3]   =  prmScomm3  .

         RUN local-assign-record .

         ASSIGN
             prmAction = "View"
             prmLine   =  ar-invl.LINE  .
        

END.  

/*********************************delete ******************************/

IF prmAction = "ValidateDelete"  THEN DO:

     FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.inv-no = prmInv  EXCLUSIVE-LOCK NO-ERROR.

    IF ar-inv.posted THEN DO:
     cError =  "Invoice already posted. No Deletion allowed!"  .
     RETURN .
    END.

END.

IF prmAction = "MainDelete" THEN DO:

    FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.inv-no = prmInv  EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST ar-invl WHERE ar-invl.company = ar-inv.company AND ar-invl.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

    FIND bf-inv WHERE RECID(bf-inv) = RECID(ar-inv).
      ASSIGN bf-inv.gross = bf-inv.gross - ar-invl.amt
         .
     {ar/ar-invk.i bf-inv}
     
    IF AVAIL ar-invl THEN
        DELETE ar-invl .

END.

IF prmAction = "DataDelete"  THEN DO:

    FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.inv-no = prmInv  EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST ar-invl WHERE ar-invl.company = ar-inv.company AND ar-invl.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ar-invl THEN
        DELETE ar-invl .

    FIND LAST ar-invl WHERE ar-inv.company = prmComp AND ar-invl.inv-no = ar-inv.inv-no  NO-LOCK NO-ERROR.
    IF AVAIL ar-invl THEN
        ASSIGN
        prmReckey = ar-invl.rec_key
        prmAction = "View" .

END.  

/*******************************View************************************/


IF prmAction = "View" THEN DO:
  
    FIND FIRST ar-inv WHERE  ar-inv.company = cocode AND ar-inv.inv-no = prmInv  NO-LOCK NO-ERROR.
         FOR EACH ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.rec_key = prmReckey NO-LOCK :
             CREATE ttArViewInvlLine.
             ASSIGN 
                 ttArViewInvlLine.arline        = ar-invl.LINE 
                 ttArViewInvlLine.actnum        = ar-invl.actnum
                 ttArViewInvlLine.actdscr       = get-actdscr()
                 ttArViewInvlLine.i-name        = ar-invl.i-name
                 ttArViewInvlLine.i-dscr        = get-i-dscr()
                 ttArViewInvlLine.lot-no        = ar-invl.lot-no
                 ttArViewInvlLine.inv-qty       = ar-invl.inv-qty 
                 ttArViewInvlLine.cons-uom      = ar-invl.cons-uom
                 ttArViewInvlLine.sf-sht        = ar-invl.sf-sht 
                 ttArViewInvlLine.unit-pr       = ar-invl.unit-pr
                 ttArViewInvlLine.pr-qty-uom    = ar-invl.pr-qty-uom
                 ttArViewInvlLine.disc          = ar-invl.disc
                 ttArViewInvlLine.cal-amt       = calc-amt() 
                 ttArViewInvlLine.amt-msf       =  ar-invl.amt-msf
                 ttArViewInvlLine.cost          = ar-invl.cost
                 ttArViewInvlLine.dscr1         = ar-invl.dscr[1]
                 ttArViewInvlLine.sman1         = ar-invl.sman[1]
                 ttArViewInvlLine.s-pct1        = ar-invl.s-pct[1]
                 ttArViewInvlLine.s-comm1       = ar-invl.s-comm[1]
                 ttArViewInvlLine.sman2         = ar-invl.sman[2]
                 ttArViewInvlLine.s-pct2        = ar-invl.s-pct[2]
                 ttArViewInvlLine.s-comm2       = ar-invl.s-comm[2]
                 ttArViewInvlLine.sman3         = ar-invl.sman[3]
                 ttArViewInvlLine.s-pct3        = ar-invl.s-pct[3]
                 ttArViewInvlLine.s-comm3       = ar-invl.s-comm[3]
                 ttArViewInvlLine.x-no          = ar-invl.x-no
                 ttArViewInvlLine.reckey        = ar-invl.rec_key
                 
                .
            
      END. /*FOR EACH buff-cust  */
      
END. /*IF prmAction = "Select" THEN DO:*/

/*****************************procedure**********************************/



PROCEDURE local-assign-record :

  FIND bf-inv WHERE RECID(bf-inv) = RECID(ar-inv) .
  IF prmAction = "Add" THEN .  /* copy */
  ELSE DO:  
    ASSIGN bf-inv.gross = bf-inv.gross - ar-invl.amt
           bf-inv.net   = bf-inv.net - ar-invl.amt.
  END.  
        
  
  ar-invl.qty = ar-invl.inv-qty.

  run sys/ref/convsuom.p (ar-invl.cons-uom,
                          ar-invl.pr-qty-uom,
                          ar-invl.sf-sht,
                          ar-invl.qty,
                          OUTPUT out-qty).

  assign
   ar-invl.amt     = if   (out-qty * ar-invl.unit-pr) eq 0
                     then (ar-invl.qty * ar-invl.unit-pr)
                     else (out-qty * ar-invl.unit-pr)
   ar-invl.amt-msf = ((ar-invl.qty * ar-invl.sf-sht) / 1000.0)
   bf-inv.gross    = bf-inv.gross + ar-invl.amt
   bf-inv.net      = bf-inv.net + ar-invl.amt.

   find first cust where cust.company eq g_company
                      and cust.cust-no eq ar-inv.cust-no no-lock no-error.
   ar-invl.tax = if ar-inv.tax-code ne "" and cust.sort eq "Y" then YES ELSE NO.

  {ar/ar-invk.i bf-inv}

END PROCEDURE.
