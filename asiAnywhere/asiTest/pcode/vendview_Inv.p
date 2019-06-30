
/*------------------------------------------------------------------------
    File        : vendView_Inv.p
    Purpose     : VendViewInvoice

     Main File   : 
    Syntax      :

    Description : Return a Dataset of all Invpice

    Author(s)   : 
    Created     :  
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttVendViewInvlLine NO-UNDO
    FIELD pono        AS INT
    FIELD arline      AS INT     
    FIELD actnum      AS CHAR         
    FIELD actdscr     AS CHAR     
    FIELD i-no        AS CHAR  
    FIELD i-dscr      AS CHAR  
    FIELD inv-qty     AS DECIMAL  
    FIELD cons-uom    AS CHAR
    FIELD unit-price  AS DECIMAL  
    FIELD qty-uom-pri AS CHAR
    FIELD tax         AS CHAR  
    FIELD sq-ft       AS DECIMAL
    FIELD amt         AS DECIMAL
    FIELD totl-msf    AS DECIMAL
    FIELD job         AS CHAR
    FIELD snum        AS INT  
    FIELD bnum        AS INT
    FIELD xy          AS CHAR
    FIELD reckey      AS CHARACTER

  
    .

DEFINE DATASET dsVendApViewInvlLine FOR ttVendViewInvlLine.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInv          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPono         AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmLine         AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmActnum       AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmActdscr      AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmIno          AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmIdscr        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInvqty       AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmConsuom      AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmUnitPrice    AS DECIMAL    NO-UNDO. 
DEFINE INPUT PARAMETER prmQtyUomPri    AS CHAR       NO-UNDO. 
DEFINE INPUT PARAMETER prmTax          AS CHAR       NO-UNDO. 
DEFINE INPUT PARAMETER prmSqft        AS DECIMAL    NO-UNDO. 
DEFINE INPUT PARAMETER prmAmt          AS DECIMAL    NO-UNDO. 
DEFINE INPUT PARAMETER prmTotlmsf     AS DECIMAL    NO-UNDO. 
DEFINE INPUT PARAMETER prmJob          AS CHAR       NO-UNDO. 
DEFINE INPUT PARAMETER prmSnum         AS INT        NO-UNDO. 
DEFINE INPUT PARAMETER prmBnum         AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHARACTER  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendApViewInvlLine .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.

     FOR EACH ttVendViewInvlLine:
        DELETE ttVendViewInvlLine .
    END.

IF prmAction      = ?  THEN ASSIGN prmAction      = "Select".
IF prmComp        = ?  THEN ASSIGN prmComp        = "".
IF prmUser        = ?  THEN ASSIGN prmUser        = "".
IF prmInv         = ?  THEN ASSIGN prmInv         = "".
IF prmPono        = ?  THEN ASSIGN prmPono        = 0.
IF prmLine        = ?  THEN ASSIGN prmLine        = 0.
IF prmActnum      = ?  THEN ASSIGN prmActnum      = "". 
IF prmActdscr     = ?  THEN ASSIGN prmActdscr     = "".
IF prmIno         = ?  THEN ASSIGN prmIno         = "".
IF prmIdscr       = ?  THEN ASSIGN prmIdscr       = "".
IF prmInvqty      = ?  THEN ASSIGN prmInvqty      = 0.
IF prmConsuom     = ?  THEN ASSIGN prmConsuom     = "".
IF prmUnitPrice   = ?  THEN ASSIGN prmUnitPrice   = 0.
IF prmQtyUomPri   = ?  THEN ASSIGN prmQtyUomPri   = "".
IF prmTax         = ?  THEN ASSIGN prmTax         = "".
IF prmSqft        = ?  THEN ASSIGN prmSqft        = 0.
IF prmAmt         = ?  THEN ASSIGN prmAmt         = 0.
IF prmTotlmsf     = ?  THEN ASSIGN prmTotlmsf     = 0.
IF prmJob         = ?  THEN ASSIGN prmJob         = "".
IF prmSnum        = ?  THEN ASSIGN prmSnum        = 0.
IF prmBnum        = ?  THEN ASSIGN prmBnum        = 0.
IF prmReckey      = ?  THEN ASSIGN prmReckey      = "".





DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF VAR v-po-qty as log initial true no-undo.
def var factor# as decimal no-undo.
DEF VAR lv-uom-list AS cha INIT "EA,MSF,M" NO-UNDO.
{oe/oe-sysct1.i NEW}
DEF VAR ll-inquiry AS LOG NO-UNDO.
DEF VAR v-actdscr LIKE account.dscr NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
 DEF BUFFER bf-invl FOR ap-invl.
    DEF VAR li-next-line AS INT NO-UNDO.
    DEF VAR out-qty LIKE ap-invl.qty NO-UNDO.
  DEF BUFFER bf-inv FOR ap-inv.
 DEF BUFFER b-ap-invl FOR ap-invl.
 DEF TEMP-TABLE tt-ap-invl NO-UNDO LIKE ap-invl
    FIELD tt-rowid AS ROWID.
 DEF VAR v-vend-act AS cha NO-UNDO.
 DEF VAR v-ap-pur AS CHAR NO-UNDO.
 DEF VAR lv-num-rec AS INT NO-UNDO.
 DEF VAR lv-po-glnum AS LOG NO-UNDO.
 DEF VAR v-vend-actnum AS cha NO-UNDO.

DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.
DEF VAR v-basis-w AS DEC NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF VAR lv-invl-qty LIKE ap-invl.qty NO-UNDO.
  DEF VAR v-temp-pr AS DEC FORM "->>>,>>9.99<<<" /*LIKE ap-invl.unit-pr */ NO-UNDO.
  DEF VAR ld-amt-msf LIKE ap-invl.amt-msf NO-UNDO.

{sys/inc/VAR.i "new shared"}  


  
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

 DEF NEW SHARED TEMP-TABLE tt-pol FIELD selekt AS LOG LABEL "Selected"
                      FIELD rec-id AS RECID                      
                      FIELD qty-inv AS log
                      FIELD amt-inv AS LOG
                      FIELD qty-to-inv LIKE ap-invl.qty
                      FIELD qty-to-inv-uom AS CHAR.
 DEF TEMP-TABLE tt-ei NO-UNDO LIKE e-item.
DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD rec_key LIKE e-item-vend.rec_key
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20.

/*DEF TEMP-TABLE tt-ap-invl NO-UNDO LIKE ap-invl
    FIELD tt-rowid AS ROWID.*/

DEF BUFFER b-tt FOR tt-ap-invl.


 
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
    g_company = prmComp
    g_user    = prmUser  .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode .

 {ce/msfcalc.i}
 {sys/inc/apdesc.i}
 {sys/inc/apsecure.i}

 {sys/inc/apinvmsg.i}

 {sys/inc/poqty.i}
 {sys/inc/fgpostgl.i}
 {sys/inc/appaper.i}

DEF VAR v-rmpostgl-char AS cha NO-UNDO.
{sys/inc/rmpostgl.i}

v-rmpostgl-char = sys-ctrl.char-fld.

FUNCTION get-actdscr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  IF AVAIL ap-invl THEN DO:
     FIND FIRST account WHERE account.company = g_company
                          AND account.actnum = ap-invl.actnum NO-LOCK NO-ERROR.
     IF AVAIL account THEN RETURN account.dscr.
     ELSE RETURN "".
  END.
  ELSE RETURN "".   /* Function return value. */
END FUNCTION.

FUNCTION display-job RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  IF AVAIL ap-invl then
     FIND FIRST po-ordl WHERE po-ordl.company = g_company
                       AND po-ordl.po-no = ap-invl.po-no
                       AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                       NO-LOCK NO-ERROR.
  IF AVAIL po-ordl THEN RETURN po-ordl.job-no + "-" + STRING(po-ordl.job-no2,">9").
  ELSE RETURN "".   /* Function return value. */
END FUNCTION.

FUNCTION display-snum RETURNS INT
  ( /* parameter-definitions */ ) :
  IF AVAIL ap-invl THEN
    FIND FIRST po-ordl WHERE po-ordl.company = g_company
                       AND po-ordl.po-no = ap-invl.po-no
                       AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                       NO-LOCK NO-ERROR.
  IF AVAIL po-ordl THEN RETURN po-ordl.s-num.
  ELSE RETURN 0.   /* Function return value. */
END FUNCTION.

FUNCTION display-bnum RETURNS INT
  ( /* parameter-definitions */ ) :
  IF AVAIL ap-invl THEN
     FIND FIRST po-ordl WHERE po-ordl.company = g_company
                          AND po-ordl.po-no = ap-invl.po-no
                          AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                          NO-LOCK NO-ERROR.
  IF AVAIL po-ordl THEN RETURN po-ordl.s-num.
  ELSE RETURN 0.   /* Function return value. */
END FUNCTION.

FUNCTION display-item-no RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  IF AVAIL ap-invl THEN 
     FIND FIRST po-ordl WHERE po-ordl.company = g_company
                       AND po-ordl.po-no = ap-invl.po-no
                       AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                       NO-LOCK NO-ERROR.
 
  IF AVAIL po-ordl THEN RETURN po-ordl.i-no.
  ELSE RETURN "NO PO ITEM".

END FUNCTION.

FUNCTION display-line RETURNS INTEGER
  ( /* parameter-definitions */ ) :
    DEF VAR apline AS INT NO-UNDO.

   IF AVAIL ap-invl THEN do:
              apline = (ap-invl.line + (ap-invl.po-no * 1000 * -1)) .
              RETURN apline.
   END.
   ELSE RETURN 0.

END FUNCTION.






IF prmAction = "SelectGrid" THEN DO:
    
    FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmReckey  NO-LOCK NO-ERROR.
         FOR EACH ap-invl WHERE ap-invl.i-no = ap-inv.i-no NO-LOCK :
             CREATE ttVendViewInvlLine.
             ASSIGN 
                 ttVendViewInvlLine.pono           = ap-invl.po-no 
                 ttVendViewInvlLine.arline         = display-line()  
                 ttVendViewInvlLine.actnum         = ap-invl.actnum  
                 ttVendViewInvlLine.actdscr        = get-actdscr()   
                 ttVendViewInvlLine.i-no           = display-item-no()
                 ttVendViewInvlLine.i-dscr         = ap-invl.dscr    
                 ttVendViewInvlLine.inv-qty        = ap-invl.qty
                 ttVendViewInvlLine.cons-uom       = ap-invl.cons-uom 
                 ttVendViewInvlLine.unit-price     = ap-invl.unit-pr
                 ttVendViewInvlLine.qty-uom-pri    = ap-invl.pr-qty-uom 
                 ttVendViewInvlLine.tax            = STRING(ap-invl.tax)
                 ttVendViewInvlLine.sq-ft          = ap-invl.sf-sht
                 ttVendViewInvlLine.amt            = ap-invl.amt
                 ttVendViewInvlLine.totl-msf       = ap-invl.amt-msf
                 ttVendViewInvlLine.job            = display-job()
                 ttVendViewInvlLine.snum           = display-snum() 
                 ttVendViewInvlLine.bnum           = display-bnum() 
                 ttVendViewInvlLine.reckey         = ap-invl.rec_key
                 
                .
             
            
      END. /*FOR EACH buff-cust  */
      
END. /*IF prmAction = "Select" THEN DO:*/


/********************************Add **********************************/


    
/********************************************************************/


IF prmAction = "ValidateAdd" THEN DO:

     FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmInv  NO-LOCK NO-ERROR.
     
     IF ap-inv.posted THEN DO:
     cError =  "Invoice already posted. No Adding allowed!" .     
     RETURN .
     END.

      IF apsecure-log AND ap-inv.user-id NE prmUser THEN do:
        ASSIGN cError = "This invoice may only be updated by UserID: " + TRIM(ap-inv.user-id) + "..." . 
        RETURN.
      END.


     IF INT(prmPono) NE 0              AND
       NOT CAN-FIND(FIRST tt-ap-invl WHERE tt-ap-invl.tt-rowid EQ ROWID(ap-invl)) THEN DO:

      FIND FIRST po-ord
          WHERE po-ord.company EQ g_company
            AND po-ord.vend-no EQ ap-inv.vend-no
            AND po-ord.po-no   EQ INT(prmPono)
          USE-INDEX vend-no NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ord THEN lv-msg = "Invalid PO, try help".

      IF lv-msg EQ "" THEN DO:

        FIND FIRST b-ap-invl WHERE
             ROWID(b-ap-invl) EQ ROWID(ap-invl)
             NO-LOCK NO-ERROR.

        FOR EACH po-ordl NO-LOCK
            WHERE po-ordl.company EQ po-ord.company
              AND po-ordl.po-no   EQ po-ord.po-no:

          RUN ap/valid-po2.p (BUFFER po-ordl, BUFFER b-ap-invl).
          IF AVAIL po-ordl THEN LEAVE.
        END.
        IF NOT AVAIL po-ordl THEN lv-msg = "No Receipts exist for this PO".
      END.

      IF lv-msg EQ ""                                                        AND
         NOT CAN-FIND(FIRST tt-ap-invl WHERE tt-ap-invl.i-no EQ ap-inv.i-no) THEN DO:

        RUN build-table (RECID(po-ord)).



        IF NOT apinvmsg-log AND lv-num-rec LE 0 THEN
          lv-msg = "All receipts for this PO have been invoiced".
      END.
        ELSE DO:
          RUN ap/d-selpos.w (RECID(ap-inv)).

          IF CAN-FIND(FIRST tt-pol
                      WHERE tt-pol.selekt
                        AND tt-pol.qty-to-inv NE 0) THEN
            RUN create-ap-from-po.

          ELSE
            lv-msg = "Nothing selected for this PO".
        END.
      

      IF lv-msg NE "" THEN DO:
         ASSIGN cError = TRIM(lv-msg).
          RETURN.
      END.
      
    END.



   FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum =prmActnum
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          cError = "Invalid GL Account Number.".
          RETURN .
       END.

   IF DEC(prmInvqty) EQ 0 THEN DO:
      cError = "The QUANTITY you enter must be greater than 0, please re-enter...".
      RETURN.
    END.


    
    IF DEC(prmUnitPrice) EQ 0 THEN DO:
      cError = "The UNIT PRICE you enter must be greater than 0, please re-enter...".
      RETURN.
    END. 
      
    
    

END.  /* end of validate add*/

/***********************check add user******************************************/

IF prmAction = "AddnewRecValidate" THEN DO:
    /*DEF VAR z AS INT NO-UNDO.*/
  FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmReckey  NO-LOCK NO-ERROR.
  IF ap-inv.posted THEN DO:
     cError =  "Invoice already posted. No Adding allowed!" .     
     RETURN .
  END.

      IF apsecure-log AND ap-inv.user-id NE prmUser THEN do:
        ASSIGN cError = "This invoice may only be updated by UserID: " + TRIM(ap-inv.user-id) + "..." . 
        RETURN.
      END.

END. 

IF prmAction = "AddnewRec" THEN DO:
    /*DEF VAR z AS INT NO-UNDO.*/
FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
 IF AVAIL ap-ctrl THEN
    v-ap-pur = ap-ctrl.purchases.

FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmReckey  NO-LOCK NO-ERROR.
    FIND LAST bf-invl
      WHERE bf-invl.i-no  EQ ap-inv.i-no
        AND bf-invl.po-no EQ 0
      USE-INDEX i-no NO-ERROR.

  FIND FIRST vend
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      NO-LOCK NO-ERROR.

  ASSIGN
   z          = (IF AVAIL bf-invl THEN bf-invl.line ELSE 0) + 1
   v-vend-act = IF AVAIL vend THEN vend.actnum ELSE "".  

  IF v-vend-act EQ "" THEN
     v-vend-act = v-ap-pur.

  CREATE ap-invl.

  FIND FIRST tt-ap-invl WHERE tt-ap-invl.i-no EQ ap-inv.i-no NO-ERROR.

  IF AVAIL tt-ap-invl THEN DO:
    BUFFER-COPY tt-ap-invl EXCEPT rec_key TO ap-invl.
    tt-ap-invl.tt-rowid = ROWID(ap-invl).
  END.

  ELSE DO:
    ASSIGN
     ap-invl.i-no       = ap-inv.i-no
     ap-invl.actnum     = v-vend-act
     ap-invl.company    = ap-inv.company
     ap-invl.vend-no    = ap-inv.vend-no
     ap-invl.line       = z
     ap-invl.loc        = ap-inv.loc
     ap-invl.period     = ap-inv.period
     ap-invl.posted     = ap-inv.posted
     ap-invl.cons-uom   = "EA"
     ap-invl.pr-qty-uom = "EA"
     ap-invl.tax        = ap-inv.tax-gr NE "".

    IF apdesc-log THEN DO:
      FIND LAST bf-invl
          WHERE bf-invl.i-no   EQ ap-inv.i-no
            AND bf-invl.po-no  EQ 0
            AND ROWID(bf-invl) NE ROWID(ap-invl)
          USE-INDEX i-no NO-ERROR.
       IF AVAIL bf-invl THEN ap-invl.dscr = bf-invl.dscr.
    END.
  END.

  ASSIGN
      prmAction = "View"
      prmLine   = ap-invl.line
      prmReckey = ap-invl.rec_key   .


END.  /* end of create ap-invl*/

IF prmAction = "Add" THEN DO:

     FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmInv  NO-LOCK NO-ERROR.
     FIND FIRST ap-invl WHERE  ap-invl.company = cocode AND ap-invl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

     
    ASSIGN
        ap-invl.po-no       =  prmPono   
        ap-invl.actnum      =  prmActnum    
        ap-invl.dscr        =  prmIdscr      
        ap-invl.qty         =  prmInvqty     
        ap-invl.cons-uom    =  prmConsuom    
        ap-invl.sf-sht      =  prmSqft       
        ap-invl.unit-pr     =  prmUnitPrice  
        ap-invl.pr-qty-uom  =  prmQtyUomPri  
        ap-invl.amt         =  prmAmt        
        ap-invl.amt-msf     =  prmTotlmsf    
        ap-invl.tax         =  IF prmTax = "YES" THEN TRUE ELSE FALSE 
         .
        IF prmPono <> 0 THEN
            ap-invl.line        = prmLine  .

     
        RUN update-header (ROWID(ap-inv), NO).

      ASSIGN
      prmAction = "View"
       .
 

END.


/**************Update *************************************************/

IF prmAction = "ValidateUpdate" THEN DO:

     FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmInv  NO-LOCK NO-ERROR.
     /*FIND FIRST ap-invl WHERE ap-invl.company = cocode AND ap-invl.inv-no = ap-inv.inv-no 
                                                       AND ap-invl.rec_key <> prmReckey
                                                       AND ap-invl.LINE = prmLine NO-LOCK NO-ERROR .
     IF AVAIL ap-invl THEN DO:
         cError = " Invoice already exists with inv# " + STRING(prmInv) + "  line " + STRING(prmLine) . 
         RETURN .
     END.*/
   IF AVAIL ap-inv THEN do:
     IF ap-inv.posted THEN DO:
     cError =  "Invoice already posted. No Adding allowed!" .     
     RETURN .
     END.
   END.

    IF apsecure-log AND ap-inv.user-id NE prmUser THEN do:
        ASSIGN cError = "This invoice may only be updated by UserID: " + TRIM(ap-inv.user-id) + "..." . 
        RETURN.
    END.
 

     IF INT(prmPono) NE 0              AND
       NOT CAN-FIND(FIRST tt-ap-invl WHERE tt-ap-invl.tt-rowid EQ ROWID(ap-invl)) THEN DO:

      FIND FIRST po-ord
          WHERE po-ord.company EQ g_company
            AND po-ord.vend-no EQ ap-inv.vend-no
            AND po-ord.po-no   EQ INT(prmPono)
          USE-INDEX vend-no NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ord THEN lv-msg = "Invalid PO, try help".

      IF lv-msg EQ "" THEN DO:

        FIND FIRST b-ap-invl WHERE
             ROWID(b-ap-invl) EQ ROWID(ap-invl)
             NO-LOCK NO-ERROR.

        FOR EACH po-ordl NO-LOCK
            WHERE po-ordl.company EQ po-ord.company
              AND po-ordl.po-no   EQ po-ord.po-no:

          RUN ap/valid-po2.p (BUFFER po-ordl, BUFFER b-ap-invl).
          IF AVAIL po-ordl THEN LEAVE.
        END.
        IF NOT AVAIL po-ordl THEN lv-msg = "No Receipts exist for this PO".
      END.

      IF lv-msg EQ ""                                                        AND
         NOT CAN-FIND(FIRST tt-ap-invl WHERE tt-ap-invl.i-no EQ ap-inv.i-no) THEN DO:

        RUN build-table (RECID(po-ord)).

        IF NOT apinvmsg-log AND lv-num-rec LE 0 THEN
          lv-msg = "All receipts for this PO have been invoiced".
      END.
        ELSE DO:
          RUN ap/d-selpos.w (RECID(ap-inv)).

          IF CAN-FIND(FIRST tt-pol
                      WHERE tt-pol.selekt
                        AND tt-pol.qty-to-inv NE 0) THEN
            RUN create-ap-from-po.

          ELSE
            lv-msg = "Nothing selected for this PO".
        END.
     

      IF lv-msg NE "" THEN DO:
         ASSIGN cError = TRIM(lv-msg).
          RETURN .
      END.
      
    END.



   FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum =prmActnum
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          cError = "Invalid GL Account Number.".
          RETURN .
       END.

   IF DEC(prmInvqty) EQ 0 THEN DO:
      cError = "The QUANTITY you enter must be greater than 0, please re-enter...".
      RETURN.
    END.


    IF DEC(prmUnitPrice) EQ 0 THEN DO:
      cError = "The UNIT PRICE you enter must be greater than 0, please re-enter...".
      RETURN.
    END. 
     
END.


IF prmAction = "Update" THEN DO:
     FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmInv  NO-LOCK NO-ERROR.
      FIND FIRST ap-invl WHERE  ap-invl.company = cocode AND ap-invl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN
        ap-invl.actnum      =  prmActnum        
        ap-invl.dscr        =  prmIdscr     
        ap-invl.qty         =  prmInvqty
        ap-invl.sf-sht      =  prmSqft  
        ap-invl.unit-pr     =  prmUnitPrice 
        ap-invl.pr-qty-uom  =  prmQtyUomPri 
        ap-invl.amt         =  prmAmt 
        ap-invl.tax         =  IF prmTax = "YES" THEN TRUE ELSE FALSE.
          .

       
        IF DEC(prmPono) EQ 0 AND
            prmActnum EQ "" THEN DO:
            FIND FIRST vend WHERE vend.company = ap-inv.company
                AND vend.vend-no = ap-inv.vend-no NO-LOCK NO-ERROR.
            v-vend-act = IF AVAIL vend THEN vend.actnum ELSE "".
            IF v-vend-act EQ "" THEN
               ASSIGN v-vend-act = v-ap-pur.
                      ttVendViewInvlLine.actnum = v-vend-act.
        END. 
        
       /* IF NOT CAN-FIND(FIRST tt-ap-invl WHERE tt-rowid EQ ROWID(ap-invl)) THEN

            FIND FIRST tt-ap-invl WHERE tt-ap-invl.i-no EQ ap-inv.i-no NO-ERROR.
        IF AVAIL tt-ap-invl THEN DO:
            FIND CURRENT ap-invl.
            BUFFER-COPY tt-ap-invl EXCEPT rec_key TO ap-invl.
            tt-rowid = ROWID(ap-invl).
            FIND CURRENT ap-invl NO-LOCK.
        END.

        IF AVAIL ap-invl THEN DO:
            FIND FIRST tt-ap-invl WHERE tt-ap-invl.tt-rowid EQ ROWID(ap-invl) NO-ERROR.
            IF AVAIL tt-ap-invl THEN DELETE tt-ap-invl.
        END.*/

       /* IF CAN-FIND(FIRST tt-ap-invl) THEN
            FOR EACH tt-ap-invl:
            CREATE bf-invl.
            BUFFER-COPY tt-ap-invl EXCEPT rec_key TO bf-invl.
            FIND CURRENT bf-invl NO-LOCK.
            DELETE tt-ap-invl.
            RUN update-header (ROWID(ap-inv), NO).
        END.*/

       RUN update-header (ROWID(ap-inv), NO).

         ASSIGN
             prmAction = "View"
             prmLine   =  ap-invl.LINE  .
        

END.  

/*********************************delete ******************************/

IF prmAction = "ValidateDelete"  THEN DO:

     FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmInv  EXCLUSIVE-LOCK NO-ERROR.

    IF ap-inv.posted THEN DO:
     cError =  "Invoice already posted. No Deletion allowed!"  .
     RETURN .
    END.
     IF apsecure-log AND ap-inv.user-id NE prmUser THEN do:
        ASSIGN cError = "This invoice may only be updated by UserID: " + TRIM(ap-inv.user-id) + "..." . 
        RETURN.
    END.

END.


IF prmAction = "DataDelete"  THEN DO:

    FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmInv  EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST ap-invl WHERE ap-invl.company = ap-inv.company AND ap-invl.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ap-invl THEN
        DELETE ap-invl .

    FIND LAST ap-invl WHERE ap-inv.company = prmComp AND ap-invl.i-no = ap-inv.i-no  NO-LOCK NO-ERROR.
    IF AVAIL ap-invl THEN
        ASSIGN
        prmReckey = ap-invl.rec_key
        prmAction = "View" .

END.  

/*******************************View************************************/


IF prmAction = "View" THEN DO:
  
    /*FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.inv-no = prmInv  NO-LOCK NO-ERROR.*/
         FOR EACH ap-invl WHERE ap-invl.company = cocode AND ap-invl.rec_key = prmReckey NO-LOCK :
             CREATE ttVendViewInvlLine.
             ASSIGN 
                 ttVendViewInvlLine.pono           = ap-invl.po-no           
                 ttVendViewInvlLine.arline         = display-line()       
                 ttVendViewInvlLine.actnum         = ap-invl.actnum          
                 ttVendViewInvlLine.actdscr        = get-actdscr()           
                 ttVendViewInvlLine.i-no           = string(display-item-no())       
                 ttVendViewInvlLine.i-dscr         = ap-invl.dscr            
                 ttVendViewInvlLine.inv-qty        = ap-invl.qty             
                 ttVendViewInvlLine.cons-uom       = ap-invl.cons-uom        
                 ttVendViewInvlLine.unit-price     = ap-invl.unit-pr         
                 ttVendViewInvlLine.qty-uom-pri    = ap-invl.pr-qty-uom      
                 ttVendViewInvlLine.tax            = string(ap-invl.tax)             
                 ttVendViewInvlLine.sq-ft          = ap-invl.sf-sht          
                 ttVendViewInvlLine.amt            = ap-invl.amt             
                 ttVendViewInvlLine.totl-msf       = ap-invl.amt-msf         
                 ttVendViewInvlLine.job            = display-job()           
                 ttVendViewInvlLine.snum           = display-snum()   
                 ttVendViewInvlLine.reckey         = ap-invl.rec_key
                 
                .
            
      END. /*FOR EACH buff-cust  */
      
END. /*IF prmAction = "Select" THEN DO:*/


/*****************calc-amt *******************************************/

IF prmAction = "CalcamtonTextBox" THEN DO:

  /*DEF VAR v-qty AS DEC NO-UNDO.*/
 /* DEF VAR lv-invl-qty LIKE ap-invl.qty NO-UNDO.
  DEF VAR v-temp-pr AS DEC FORM "->>>,>>9.99<<<" /*LIKE ap-invl.unit-pr */ NO-UNDO.
  DEF VAR ld-amt-msf LIKE ap-invl.amt-msf NO-UNDO.*/

  /*{ce/msfcalc.i}*/
 
  CREATE ttVendViewInvlLine.

  FIND po-ordl WHERE po-ordl.company = g_company
                 AND po-ordl.po-no = prmPono
                 AND po-ordl.LINE = {ap/invlline.i -1} NO-LOCK NO-ERROR.
  
  IF AVAIL po-ordl THEN DO:
     ASSIGN v-wid = po-ordl.s-wid
            v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
            v-dep = 0
            v-bwt = 0.
     
     find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.            
     if avail item then do:
          v-dep = item.s-dep.          
          {po/pol-dims.i}
     end.
  END.

  if v-wid eq 0 or v-len eq 0 then do:
    if v-wid eq 0 THEN v-wid = if v-len eq 0 then 12 else v-len.
    v-len = dec(prmSqft) * 144 / v-wid.
  end.
  
  if v-len eq 0 then v-len = 12.
  
  if prmQtyUomPri eq prmConsuom OR
     prmQtyUomPri = "0"
      THEN v-temp-pr = dec(prmUnitPrice).
  ELSE
    run sys/ref/convcuom.p (prmQtyUomPri, prmConsuom,
                            v-bwt, v-len, v-wid, v-dep,
                            dec(prmUnitPrice), output v-temp-pr).

  ttVendViewInvlLine.amt = (dec(prmInvqty) * v-temp-pr).
  
  IF prmQtyUomPri EQ "L" THEN ttVendViewInvlLine.amt = prmUnitPrice .

  if prmConsuom eq "MSF" 
     THEN ttVendViewInvlLine.totl-msf = (prmInvqty).
  else DO: 
    run sys/ref/convquom.p (prmConsuom, "MSF",      /*prmConsuom*/
                          v-bwt, v-len, v-wid, v-dep,
                          DEC(prmInvqty), output ld-amt-msf).
    ttVendViewInvlLine.totl-msf = (ld-amt-msf).
    
  END.

END.  /* calcamt */ 

/*****************************change amount textbox********/
IF prmAction = "amountchangetextbox" THEN DO:

  /*DEF VAR lv-invl-qty LIKE ap-invl.qty NO-UNDO.
  DEF VAR v-temp-pr LIKE ap-invl.unit-pr NO-UNDO.
  DEF VAR ld-amt-msf LIKE ap-invl.amt-msf NO-UNDO.*/

  /*{ce/msfcalc.i}*/
  CREATE ttVendViewInvlLine.

    FIND po-ordl WHERE po-ordl.company = g_company
                 AND po-ordl.po-no = prmPono
                 AND po-ordl.LINE = {ap/invlline.i -1} NO-LOCK NO-ERROR.
  
    IF AVAIL po-ordl THEN DO:
       ASSIGN v-wid = po-ordl.s-wid
              v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
              v-dep = 0
              v-bwt = 0
            .
     
       find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.            
       if avail item then do:
            v-dep = item.s-dep.          
            {po/pol-dims.i}
       end.
    END.

     if v-wid eq 0 or v-len eq 0 then do:
       if v-wid eq 0 THEN v-wid = if v-len eq 0 then 12 else v-len.
       v-len = dec(prmSqft) * 144 / v-wid.
     end.
   
     if v-len eq 0 then v-len = 12.

     v-temp-pr = dec(prmAmt) / dec(prmInvqty).

     if prmQtyUomPri NE prmConsuom AND
        prmQtyUomPri NE "0" THEN
       run sys/ref/convcuom.p (prmConsuom, prmQtyUomPri,
                               v-bwt, v-len, v-wid, v-dep,
                               v-temp-pr, output v-temp-pr).
                               
     ttVendViewInvlLine.unit-price = (v-temp-pr).
   
     if prmConsuom eq "MSF" 
        THEN ttVendViewInvlLine.totl-msf = (prmInvqty).
     else DO: 
       run sys/ref/convquom.p (prmConsuom, "MSF",
                            v-bwt, v-len, v-wid, v-dep,
                            DEC(prmInvqty), output ld-amt-msf).
       ttVendViewInvlLine.totl-msf = (ld-amt-msf). 
     END.
 

END.  /* change amount  */

IF prmAction = "BrdReClc" THEN DO:
    FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.rec_key = prmInv  NO-LOCK NO-ERROR.
         

  DEF VAR lv-tot-rec-qty LIKE rm-rdtlh.qty NO-UNDO.
  /*DEF VAR v-qty LIKE rm-rdtlh.qty NO-UNDO.*/
  /*DEF VAR v-len like po-ordl.s-len NO-UNDO.
  DEF VAR v-wid like po-ordl.s-wid NO-UNDO.  */
  DEF VAR lv-item-no AS CHAR NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR v-old-qty LIKE lv-tot-rec-qty NO-UNDO.

  DEF BUFFER b-po-ordl FOR po-ordl.

  FOR EACH ap-invl WHERE ap-invl.i-no = ap-inv.i-no AND
    ap-invl.po-no NE 0
    EXCLUSIVE-LOCK,
    FIRST po-ordl WHERE po-ordl.company = g_company AND
      po-ordl.po-no = ap-invl.po-no AND
      po-ordl.LINE = {ap/invlline.i -1} AND
      po-ordl.item-type
      NO-LOCK,
    FIRST po-ord WHERE
      po-ord.company = cocode AND
      po-ord.po-no = ap-invl.po-no
      NO-LOCK:

    ASSIGN
      lv-item-no = display-item-no()
      lv-uom = ap-invl.cons-uom.

    FIND FIRST ITEM WHERE
      ITEM.company = cocode AND
      ITEM.i-no = lv-item-no AND
      ITEM.mat-type = "B"
      NO-LOCK NO-ERROR.
  
      ASSIGN lv-tot-rec-qty = 0
             v-len = 0
             v-wid = 0.

      FOR EACH rm-rcpth WHERE
        rm-rcpth.company = cocode AND
        rm-rcpth.po-no = STRING(ap-invl.po-no) AND
        rm-rcpth.i-no = lv-item-no AND
        rm-rcpth.job-no EQ po-ordl.job-no AND
        rm-rcpth.job-no2 EQ po-ordl.job-no2 AND
        rm-rcpth.rita-code = "R"
        NO-LOCK,
        FIRST rm-rdtlh OF rm-rcpth NO-LOCK:

        ASSIGN
          v-len = item.s-len
          v-wid = if item.r-wid gt 0 then item.r-wid else item.s-wid.
        
        for each b-po-ordl
            where b-po-ordl.company eq cocode
               and b-po-ordl.po-no   eq po-ord.po-no
               and b-po-ordl.i-no    eq rm-rcpth.i-no
               and b-po-ordl.job-no  eq rm-rcpth.job-no
               and b-po-ordl.job-no2 eq rm-rcpth.job-no2
             no-lock
             by b-po-ordl.s-num desc:
             
           assign
            v-len = b-po-ordl.s-len
            v-wid = b-po-ordl.s-wid.
          
           if b-po-ordl.s-num eq rm-rdtlh.s-num then leave.
        end.
      
        if rm-rcpth.job-no ne "" then
          for each job-mat
              where job-mat.company eq cocode
                and job-mat.rm-i-no eq item.i-no
                and job-mat.job-no  eq rm-rcpth.job-no
                and job-mat.job-no2 eq rm-rcpth.job-no2
              no-lock
              by job-mat.frm desc:
              
            assign
             v-len = job-mat.len
             v-wid = job-mat.wid.
          
            if job-mat.frm eq rm-rdtlh.s-num then leave.  
          end.
      
        if v-len eq 0 then v-len = 12.
        if v-wid eq 0 then v-wid = 12.

        v-qty = rm-rdtlh.qty.

        IF rm-rcpth.pur-uom NE lv-uom THEN
          RUN sys/ref/convquom.p (rm-rcpth.pur-uom, lv-uom,
                                item.basis-w, v-len, v-wid, item.s-dep,
                                v-qty, OUTPUT v-qty).

        {sys/inc/roundup.i v-qty}
        

        lv-tot-rec-qty = lv-tot-rec-qty + v-qty.
      END.

      IF lv-tot-rec-qty NE 0 THEN
      DO:
         ASSIGN
            v-old-qty = ap-invl.qty
            ap-invl.qty = lv-tot-rec-qty.
         RUN vend-cost.
         RUN calc-amt3.
         ap-invl.qty = v-old-qty.
      END.
  END.

  IF NOT CAN-FIND(FIRST tt-ap-invl WHERE tt-rowid EQ ROWID(ap-invl)) THEN
     FIND FIRST tt-ap-invl WHERE tt-ap-invl.i-no EQ ap-inv.i-no NO-ERROR.
  IF AVAIL tt-ap-invl THEN DO:
      FIND CURRENT ap-invl.
      BUFFER-COPY tt-ap-invl EXCEPT rec_key TO ap-invl.
      tt-rowid = ROWID(ap-invl).
      FIND CURRENT ap-invl NO-LOCK.
   END.

  IF AVAIL ap-invl THEN DO:
    FIND FIRST tt-ap-invl WHERE tt-ap-invl.tt-rowid EQ ROWID(ap-invl) NO-ERROR.
    IF AVAIL tt-ap-invl THEN DELETE tt-ap-invl.
  END.

  RUN update-header (ROWID(ap-inv), NO).

  
            
END.

/*****************************procedure**********************************/



PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-po-recid AS RECID NO-UNDO.

  DEF VAR i AS INT NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR v-qty-r AS DEC NO-UNDO.
  DEF VAR v-qty-i AS DEC NO-UNDO.
  DEF VAR v-amt-r AS DEC NO-UNDO.
  DEF VAR v-amt-i AS DEC NO-UNDO.
  DEF VAR v-negative-receipt AS LOG NO-UNDO.
  DEF VAR v-po-no AS CHAR NO-UNDO.

  FIND po-ord WHERE RECID(po-ord) = ip-po-recid NO-LOCK NO-ERROR.

  for each tt-pol:
      delete tt-pol.
  end.
  
  lv-num-rec = 0.

  if avail po-ord then    
  for each po-ordl WHERE
      po-ordl.company EQ po-ord.company AND
      po-ordl.po-no EQ po-ord.po-no AND
    (not can-find(ap-invl where ap-invl.i-no       eq ap-inv.i-no
                               and ap-invl.po-no      eq po-ordl.po-no
                               and {ap/invlline.i -1} eq po-ordl.LINE
                             use-index i-no))
           use-index po-no NO-LOCK:
    
      IF LOOKUP(po-ordl.stat,"X,F") > 0 /* not deleted or cancelled */ THEN
         NEXT.

      v-negative-receipt = NO.

      IF po-ordl.t-rec-qty EQ 0 AND
         NOT can-find(first item where item.company eq cocode
                               and item.i-no    eq po-ordl.i-no
                               and item.i-code  eq "R"
                               and item.stocked eq no) THEN
         DO:
            v-po-no = STRING(po-ordl.po-no).

            FOR EACH rm-rcpth WHERE
                rm-rcpth.company EQ po-ordl.company AND
                rm-rcpth.po-no EQ v-po-no AND
                rm-rcpth.rita-code EQ "R"
                NO-LOCK,
                EACH rm-rdtlh WHERE
                     rm-rdtlh.r-no EQ rm-rcpth.r-no AND
                     rm-rdtlh.rita-code EQ rm-rcpth.rita-code AND
                     rm-rdtlh.qty LT 0
                     NO-LOCK:
            
                v-negative-receipt = YES.
                LEAVE.
            END.
            
            IF v-negative-receipt = NO THEN
            DO:
               FOR EACH fg-rcpth WHERE
                   fg-rcpth.company EQ po-ordl.company AND
                   fg-rcpth.vend-no EQ po-ord.vend-no AND
                   fg-rcpth.po-no EQ v-po-no AND
                   LOOKUP(fg-rcpth.rita-code,"R,E") > 0
                   NO-LOCK,
                   EACH fg-rdtlh WHERE
                        fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                        fg-rdtlh.rita-code EQ fg-rcpth.rita-code AND
                        fg-rdtlh.qty LT 0
                        NO-LOCK:
                  
                   v-negative-receipt = YES.
                   LEAVE.
               END.
            END.
         END.

                     
  IF NOT (po-ordl.t-rec-qty NE 0 OR v-negative-receipt OR
         (po-ordl.item-type and
         can-find(first item where item.company eq cocode
                               and item.i-no    eq po-ordl.i-no
                               and item.i-code  eq "R"
                               and item.stocked eq no))) THEN NEXT.

      run sys/inc/po-recqa.p (recid(po-ordl), output v-qty-r, output v-amt-r).    
      run sys/inc/po-invqa.p (recid(po-ordl), output v-qty-i, output v-amt-i).

      ASSIGN v-amt-i = 0
             v-amt-r = 0.

      find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.

      IF po-ordl.stat NE "C" OR
         (apinvmsg-log = YES AND v-qty-r EQ 0 AND
          v-qty-i EQ 0) OR
         v-qty-r NE v-qty-i OR
         (avail item           and
          item.i-code eq "R"   AND
          INDEX("MOXY789",ITEM.mat-type) GT 0 AND
          item.stocked eq no)  THEN DO:

        create tt-pol.
        ASSIGN tt-pol.rec-id  = recid(po-ordl)
               tt-pol.qty-inv = (v-qty-r EQ 0 AND
                                 v-qty-i EQ 0) OR
                                (v-qty-i NE v-qty-r and v-qty-r NE 0)
               tt-pol.amt-inv = v-amt-i NE v-amt-r and v-amt-r NE 0
               tt-pol.selekt  = if choice then yes else no.
        lv-num-rec = lv-num-rec + 1.
      END.
end.

END PROCEDURE.


PROCEDURE create-ap-from-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR v-vend-act AS CHAR NO-UNDO.
  DEF VAR v-dscr AS CHAR NO-UNDO.
  DEF VAR v-tmp-qty LIKE ap-invl.qty NO-UNDO.

  DEF VAR v-ext-cost AS DEC NO-UNDO.
  DEF VAR v-qty-uom AS CHAR NO-UNDO.
  DEF VAR v-out-qty AS DEC NO-UNDO.        
  DEF VAR v-out-cost AS DEC NO-UNDO.
  DEF VAR v-setup-per AS DEC NO-UNDO.

  {ce/msfcalc.i}


  FIND FIRST vend NO-LOCK
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      NO-ERROR.
  ASSIGN
   v-vend-act = IF AVAIL vend THEN vend.actnum ELSE ""
   v-dscr     = "".

  IF v-vend-act EQ "" THEN
     v-vend-act = v-ap-pur.

  FOR EACH tt-pol
      WHERE tt-pol.selekt
        AND tt-pol.qty-to-inv NE 0,
      FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK
      BREAK BY po-ordl.po-no:

      FIND FIRST po-ord NO-LOCK
          WHERE po-ord.company EQ po-ordl.company 
            AND po-ord.po-no   EQ po-ordl.po-no
          NO-ERROR.

     CREATE tt-ap-invl.
     ASSIGN tt-ap-invl.i-no =  ap-inv.i-no
            tt-ap-invl.actnum = v-vend-act
            tt-ap-invl.company = ap-inv.company
            tt-ap-invl.vend-no = ap-inv.vend-no
            tt-ap-invl.dscr = v-dscr
            tt-ap-invl.loc = ap-inv.loc
            tt-ap-invl.period = ap-inv.period
            tt-ap-invl.posted = ap-inv.posted
            tt-ap-invl.tax = ap-inv.tax-gr NE ""
            .
      /**IF aptax-chr = "ITEM" THEN DO:
        FIND ITEM WHERE ITEM.company = g_company
                 AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN tt-ap-invl.tax = ITEM.tax-rcpt .
     END.*/

     ASSIGN tt-ap-invl.po-no = (po-ord.po-no)
            tt-ap-invl.LINE = (po-ordl.LINE + (po-ord.po-no * 1000) ) /* ap/invline.i 1 */
            tt-ap-invl.dscr = po-ordl.i-name
            tt-ap-invl.unit-pr = (po-ordl.cost)
            tt-ap-invl.pr-qty-uom = po-ordl.pr-uom
            tt-ap-invl.cons-uom = po-ordl.pr-qty-uom
            v-wid = po-ordl.s-wid
            v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
            v-dep = 0
            v-bwt = 0.
     
     IF tt-ap-invl.cons-uom EQ "ROLL" THEN tt-ap-invl.cons-uom = "LF".

     IF po-ordl.item-type AND appaper-chr NE "PO UOM" AND
        CAN-FIND(FIRST item
                 WHERE item.company EQ cocode
                   AND item.i-no    EQ po-ordl.i-no
                   AND item.mat-type EQ "P")          THEN
       tt-ap-invl.cons-uom = appaper-chr.

     RELEASE prod.
     RELEASE costtype.

     IF po-ordl.item-type EQ NO                          AND
        (fgpostgl EQ "AllItems" OR fgpostgl EQ "POOnly") THEN DO:
       FIND FIRST itemfg
           WHERE itemfg.company EQ po-ordl.company
             AND itemfg.i-no    EQ po-ordl.i-no
           NO-LOCK NO-ERROR.
              
       IF AVAIL itemfg THEN
       FIND FIRST prodl 
           WHERE prodl.company EQ itemfg.company
             AND prodl.procat  EQ itemfg.procat
             AND CAN-FIND(FIRST prod
                          WHERE prod.company EQ cocode
                            AND prod.prolin  EQ prodl.prolin)
           NO-LOCK NO-ERROR.

       IF AVAIL prodl THEN
       FIND FIRST prod
           WHERE prod.company EQ prodl.company
             AND prod.prolin  EQ prodl.prolin
           NO-LOCK NO-ERROR.
     END.

     ELSE
     IF po-ordl.item-type AND v-rmpostgl-char EQ "ALLITEMS" THEN DO:
       FIND FIRST item
          WHERE item.company EQ cocode
            AND item.i-no    EQ po-ordl.i-no
          NO-LOCK NO-ERROR.
      
       IF AVAIL item AND item.stocked THEN
       FIND FIRST costtype
           WHERE costtype.company   EQ cocode
             AND costtype.cost-type EQ item.cost-type
           NO-LOCK NO-ERROR.
     END.

     IF AVAIL prod AND prod.wip-mat NE "" THEN
        tt-ap-invl.actnum = prod.wip-mat.
     ELSE
     IF AVAIL costtype AND costtype.ap-accrued NE "" THEN
        tt-ap-invl.actnum = costtype.ap-accrued.
     ELSE
     IF lv-po-glnum THEN tt-ap-invl.actnum = po-ordl.actnum.
     ELSE DO:
        if v-vend-actnum eq "" then do:
            find first vend where vend.company eq cocode
                  and vend.vend-no eq po-ord.vend-no 
                no-lock no-error.
            if avail vend then v-vend-actnum = vend.actnum.
        end.
        tt-ap-invl.actnum = v-vend-actnum.
     end.  

     IF v-vend-actnum EQ "" THEN
        v-vend-actnum = v-ap-pur.

     if tt-ap-invl.actnum eq "" then tt-ap-invl.actnum = v-vend-actnum.

     find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.            
     if avail item then do:
          v-dep = item.s-dep.          
          {po/pol-dims.i}
     end.

        IF NOT po-ordl.item-type AND tt-ap-invl.cons-uom NE "EA" THEN
          RUN sys/ref/convquom.p ("EA", tt-ap-invl.cons-uom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  tt-pol.qty-to-inv, OUTPUT tt-pol.qty-to-inv).

        ASSIGN
         tt-ap-invl.qty     = tt-pol.qty-to-inv.

        IF tt-pol.qty-to-inv-uom NE "" AND
           tt-pol.qty-to-inv-uom NE po-ordl.pr-qty-uom THEN
           RUN sys/ref/convquom.p (tt-pol.qty-to-inv-uom, po-ordl.pr-qty-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   tt-ap-invl.qty, OUTPUT v-tmp-qty).
        ELSE
           v-tmp-qty = tt-ap-invl.qty.
        IF po-ordl.pr-uom = "MSF" AND po-ordl.item-type = NO THEN DO:
        
          IF po-ordl.item-type = NO THEN DO:
        
            RUN calc-setup-cost(INPUT ROWID(po-ordl), 
                                INPUT v-tmp-qty,
                                OUTPUT v-ext-cost,
                                OUTPUT v-qty-uom,
                                OUTPUT v-out-qty,
                                OUTPUT v-out-cost,
                                OUTPUT v-setup-per).
     

            ASSIGN  tt-ap-invl.amt = (v-out-cost / 1000) * v-tmp-qty
                    tt-ap-invl.unit-pr = tt-ap-invl.amt / tt-ap-invl.qty.
          END.
        END.
        ELSE
          ASSIGN
           tt-ap-invl.amt = po-ordl.t-cost / po-ordl.ord-qty * v-tmp-qty
           tt-ap-invl.unit-pr = tt-ap-invl.amt / tt-ap-invl.qty.
        


        IF tt-ap-invl.cons-uom NE tt-ap-invl.pr-qty-uom THEN
          RUN sys/ref/convcuom.p (tt-ap-invl.cons-uom, tt-ap-invl.pr-qty-uom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  tt-ap-invl.unit-pr, OUTPUT tt-ap-invl.unit-pr).
        tt-ap-invl.unit-pr = ROUND(tt-ap-invl.unit-pr, 2).

         if v-len eq 0 then v-len = 12.
        if v-wid eq 0 then v-wid = 12.
        
        tt-ap-invl.sf-sht = if v-corr then (v-len * v-wid * .007)
                                      else (v-len * v-wid / 144).

        if not avail item             and
           (v-len eq 0 or v-wid eq 0) then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq po-ordl.i-no
                and NOT po-ordl.item-type
              no-lock no-error.
          if avail itemfg then tt-ap-invl.sf-sht = (itemfg.t-sqft).
        end.

        if tt-ap-invl.cons-uom eq "EA" THEN v-qty = tt-ap-invl.qty.          
        else
          run sys/ref/convquom.p(tt-ap-invl.cons-uom, "EA",
                                 v-bwt, v-len, v-wid, v-dep,
                                 tt-ap-invl.qty, output v-qty).

        tt-ap-invl.amt-msf = (tt-ap-invl.sf-sht * v-qty / 1000).

  END.

END PROCEDURE.

PROCEDURE vend-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  

  /*from create po line item program (d-poordl.w) */
  DEF VAR v-qty  AS DEC NO-UNDO.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-pb-qty AS DEC NO-UNDO.
  DEF VAR v-pb-stp AS DEC NO-UNDO.
  DEF VAR v-pb-cst AS DEC NO-UNDO.
  DEF VAR v-pb-cns AS DEC NO-UNDO.
  DEF VAR v-save-qty AS DEC NO-UNDO.
  DEF VAR v-setup AS DEC NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-added-cost AS DEC NO-UNDO.
  DEF VAR lv-added-cons-cost AS DEC NO-UNDO.
  DEF VAR lv-adder-setup AS DEC NO-UNDO.
  DEF VAR lv-recid AS RECID NO-UNDO.
  DEF VAR lv-t-cost AS DEC NO-UNDO.
  DEF VAR lv-cons-cost AS DEC NO-UNDO.
  DEF VAR v-ord-qty LIKE po-ordl.ord-qty NO-UNDO.
  DEF VAR lv-pb-qty LIKE po-ordl.ord-qty NO-UNDO.
  DEF VAR lv-final-cost LIKE po-ordl.cost NO-UNDO.
  DEF VAR lv-final-setup AS DEC NO-UNDO.
  DEF VAR ar-pr-qty-uom-cost AS DEC NO-UNDO.
  DEF VAR ld-dim-charge AS DEC NO-UNDO.
  DEF VAR v-index AS INT NO-UNDO.

  EMPTY TEMP-TABLE tt-ei.
  EMPTY TEMP-TABLE tt-eiv.

  
    RUN set-dims.

    /* for adders */
    RELEASE job-mat.

    FIND FIRST job NO-LOCK
        WHERE job.company EQ po-ordl.company
          AND job.job-no  EQ po-ordl.job-no
          AND job.job-no2 EQ po-ordl.job-no2
        NO-ERROR.
    IF AVAIL job THEN
    FIND FIRST job-mat NO-LOCK
        WHERE job-mat.company  EQ job.company
          AND job-mat.job      EQ job.job
          AND job-mat.job-no   EQ job.job-no
          AND job-mat.job-no2  EQ job.job-no2
          AND job-mat.frm      EQ po-ordl.s-num
          AND job-mat.blank-no EQ po-ordl.b-num
        USE-INDEX seq-idx NO-ERROR.
        
    IF AVAIL job-mat THEN lv-recid = RECID(job-mat).

    FIND FIRST e-item NO-LOCK
        WHERE e-item.company EQ cocode
          AND e-item.i-no    EQ po-ordl.i-no
        NO-ERROR.

    IF AVAIL e-item THEN DO:
      CREATE tt-ei.
      BUFFER-COPY e-item TO tt-ei.

      FIND FIRST e-item-vend NO-LOCK
          WHERE e-item-vend.company EQ e-item.company
            AND e-item-vend.i-no    EQ e-item.i-no
            AND e-item-vend.vend-no EQ po-ord.vend-no
          NO-ERROR.

      IF AVAIL e-item-vend THEN DO:
        CREATE tt-eiv.
        tt-eiv.rec_key = e-item-vend.rec_key.

        FIND FIRST b-qty WHERE
             b-qty.reftable = "vend-qty" AND
             b-qty.company = e-item-vend.company AND
             b-qty.CODE    = e-item-vend.i-no AND
             b-qty.code2   = e-item-vend.vend-no             
             NO-LOCK NO-ERROR.

        FIND FIRST b-cost WHERE
             b-cost.reftable = "vend-cost" AND
             b-cost.company = e-item-vend.company AND
             b-cost.CODE    = e-item-vend.i-no AND
             b-cost.code2   = e-item-vend.vend-no             
             NO-LOCK NO-ERROR.

        FIND FIRST b-setup WHERE
             b-setup.reftable = "vend-setup" AND
             b-setup.company = e-item-vend.company AND
             b-setup.CODE    = e-item-vend.i-no AND
             b-setup.code2   = e-item-vend.vend-no             
             NO-LOCK NO-ERROR.

        DO v-index = 1 TO 20:

           IF v-index LE 10 THEN
              ASSIGN
                 tt-eiv.run-qty[v-index] = e-item-vend.run-qty[v-index]
                 tt-eiv.run-cost[v-index] = e-item-vend.run-cost[v-index]
                 tt-eiv.setups[v-index] = e-item-vend.setups[v-index].
           ELSE
              IF AVAIL b-qty THEN
                 ASSIGN
                 tt-eiv.run-qty[v-index] = b-qty.val[v-index - 10]
                 tt-eiv.run-cost[v-index] = b-cost.val[v-index - 10]
                 tt-eiv.setups[v-index] = b-setup.val[v-index - 10].
        END.
      END.
    END.

    IF AVAIL tt-eiv THEN DO:
      ASSIGN
       v-cost = po-ordl.cost
       v-qty  = ap-invl.qty.

      
      IF tt-ei.std-uom NE ap-invl.cons-uom THEN
         RUN sys/ref/convquom.p(ap-invl.cons-uom,
                               tt-ei.std-uom, v-basis-w,
                               v-len, v-wid, v-dep,
                               v-qty, OUTPUT v-qty).      

      v-save-qty = v-qty.

      ASSIGN
       v-save-qty = v-qty - v-save-qty
       v-setup    = 0
       v-pb-qty   = 0.
            
      RUN est/dim-charge.p (tt-eiv.rec_key,
                            v-wid,
                            v-len,
                            INPUT-OUTPUT ld-dim-charge).

      DO li = 1 TO 20:
        IF tt-eiv.run-qty[li] LT v-qty THEN NEXT.
        ASSIGN
         v-cost   = (tt-eiv.run-cost[li] + ld-dim-charge) * v-qty
         v-setup  = tt-eiv.setups[li]
         v-pb-qty = tt-eiv.run-qty[li] - v-save-qty.

        IF li LT 20 THEN
          ASSIGN
           v-pb-cst = tt-eiv.run-cost[li + 1] + ld-dim-charge
           v-pb-stp = tt-eiv.setups[li + 1].
        LEAVE.
      END.

      IF poqty-log THEN DO:
        IF v-pb-qty GE 9999999 THEN v-pb-qty = 0.

        IF v-pb-qty EQ 0 THEN v-pb-cst = 0.
        ELSE DO:
          v-pb-qty = v-pb-qty + .001.

          v-pb-cst = v-pb-cst * v-pb-qty.

          IF v-pb-qty NE 0 THEN v-pb-cst = v-pb-cst / v-pb-qty.  
          ELSE v-pb-cst = v-pb-cst.
        END.

        IF tt-ei.std-uom NE ap-invl.cons-uom THEN
          RUN sys/ref/convquom.p(tt-ei.std-uom,
                                 ap-invl.cons-uom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-pb-qty, OUTPUT v-pb-qty).

        IF tt-ei.std-uom NE ap-invl.pr-qty-uom  THEN
          RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                 ap-invl.pr-qty-uom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cst).

        IF ap-invl.pr-qty-uom NE ap-invl.cons-uom THEN
          RUN sys/ref/convcuom.p(ap-invl.pr-qty-uom,
                                 ap-invl.cons-uom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cns).

        lv-pb-qty = IF v-pb-qty LE 0 THEN 0 ELSE v-pb-qty.
      END.
      
      IF v-qty <> 0 THEN v-cost = v-cost / v-qty.  
      
      IF tt-ei.std-uom NE ap-invl.pr-qty-uom THEN
        RUN sys/ref/convcuom.p(tt-ei.std-uom,
                               ap-invl.pr-qty-uom, v-basis-w,
                               v-len, v-wid, v-dep,
                               v-cost, OUTPUT v-cost).
      ASSIGN
        lv-final-cost = v-cost
        lv-final-setup = v-setup
        ar-pr-qty-uom-cost = v-cost.

      IF ap-invl.pr-qty-uom NE ap-invl.cons-uom THEN
        RUN sys/ref/convcuom.p(ap-invl.pr-qty-uom,
                               ap-invl.cons-uom, v-basis-w,
                               v-len, v-wid, v-dep,
                               v-cost, OUTPUT v-cost).
      lv-cons-cost = v-cost.
    END.

    IF AVAIL job-mat THEN DO:
      
      IF poqty-log THEN
        RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                       lv-pb-qty,
                       v-pb-cst,
                       v-pb-cns,
                       OUTPUT v-pb-cst,
                       OUTPUT v-pb-cns,
                       OUTPUT lv-adder-setup).
      
        RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                       ap-invl.qty,
                       ar-pr-qty-uom-cost,
                       lv-cons-cost,
                       OUTPUT lv-added-cost,
                       OUTPUT lv-added-cons-cost,
                       OUTPUT lv-adder-setup).

        lv-final-cost = lv-added-cost.
    END.

    IF poqty-log THEN DO:
      IF CAN-DO("L,LOT",ap-invl.pr-qty-uom) THEN
        lv-t-cost = (v-pb-cst + v-pb-stp) *
                    IF ap-invl.qty LT 0 THEN -1 ELSE 1.

      ELSE DO:
        v-ord-qty = lv-pb-qty.

        IF ap-invl.cons-uom NE ap-invl.pr-qty-uom THEN
   
          RUN sys/ref/convquom.p(ap-invl.cons-uom,
                                 ap-invl.pr-qty-uom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
     
        lv-t-cost = (v-ord-qty * v-pb-cst) + v-pb-stp.
      END.

      IF po-ordl.disc NE 0 THEN
        lv-t-cost = lv-t-cost * (1 - (po-ordl.disc / 100)).
    END.

    v-ord-qty = ap-invl.qty.

    /*assuming getting setup from vendor cost file*/
    IF CAN-DO("L,LOT",ap-invl.pr-qty-uom) THEN
      lv-t-cost = (lv-final-cost + lv-final-setup) *
                  IF ap-invl.qty LT 0 THEN -1 ELSE 1.
    ELSE DO:
      
      IF ap-invl.cons-uom NE ap-invl.pr-qty-uom THEN
        RUN sys/ref/convquom.p(ap-invl.cons-uom,
                               ap-invl.pr-qty-uom,
                               v-basis-w, v-len, v-wid, v-dep,
                               v-ord-qty, OUTPUT v-ord-qty).
    
      lv-t-cost = (v-ord-qty * lv-final-cost) +
                  lv-final-setup.
    END.

    IF po-ordl.disc NE 0 THEN
       lv-t-cost = lv-t-cost * (1 - (po-ordl.disc / 100)).
    
    ap-invl.unit-pr = lv-t-cost / v-ord-qty.

  
END PROCEDURE.

PROCEDURE set-dims :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     {po/calc10.i v-len}
     {po/calc10.i v-wid}.

    FIND FIRST ITEM
        WHERE item.company EQ cocode
          AND item.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

    ASSIGN
     v-basis-w = IF AVAIL ITEM THEN item.basis-w ELSE 0
     v-dep     = IF AVAIL ITEM THEN item.s-dep ELSE 0.
 
END PROCEDURE.

PROCEDURE po-adder2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-recid  as recid.
DEF INPUT PARAM ip-recid1 as recid.
DEF INPUT PARAM ip-vend-no LIKE po-ord.vend-no NO-UNDO.
DEF INPUT PARAM ip-qty as DEC NO-UNDO.
DEF INPUT PARAM ip-cost as DEC NO-UNDO.
DEF INPUT PARAM ip-cons-cost as DEC NO-UNDO.

DEF OUTPUT PARAM op-cost AS DEC NO-UNDO.
DEF OUTPUT PARAM op-cons-cost AS DEC NO-UNDO.
DEF OUTPUT PARAM op-adder-setup AS DEC NO-UNDO.

def var v-tot-cost as dec no-undo.
def var v-cost     as dec no-undo.
def var v-add-cost as dec no-undo.
def var v-qty-comp as dec no-undo.
def var v-setup like e-item-vend.setup no-undo.
def var v-adder as dec extent 2 NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.

def buffer xjob-mat for job-mat.

find xjob-mat where recid(xjob-mat) eq ip-recid1 no-lock.

assign
 v-adder[1] = ip-cost
 v-adder[2] = ip-cons-cost.


  IF ap-invl.pr-qty-uom EQ "EA" THEN
     v-tot-cost = ip-cost.
  ELSE
    RUN sys/ref/convcuom.p(ap-invl.pr-qty-uom, "EA",
                           v-basis-w, v-len, v-wid, v-dep,
                           ip-cost, OUTPUT v-tot-cost).
 
  for each job-mat no-lock
      where job-mat.company  eq xjob-mat.company
        and job-mat.job      eq xjob-mat.job
        and job-mat.frm      eq xjob-mat.frm
        and job-mat.job-no   eq xjob-mat.job-no
        and job-mat.job-no2  eq xjob-mat.job-no2
      use-index seq-idx,

      first item no-lock
      where item.company  eq job-mat.company
        and item.i-no     eq job-mat.i-no
        and item.mat-type eq "A":

    find first e-item no-lock
        where e-item.company eq po-ordl.company
          and e-item.i-no    eq po-ordl.i-no
        no-error.
    
    find first e-item-vend no-lock
        where e-item-vend.company eq item.company
          and e-item-vend.i-no    eq item.i-no
          and e-item-vend.vend-no eq ip-vend-no
        no-error.

    if avail e-item and avail e-item-vend AND ip-vend-no NE "" then do:
      if ap-invl.cons-uom eq e-item.std-uom then
           v-qty-comp = ip-qty.
      else
        run sys/ref/convquom.p(ap-invl.cons-uom, e-item.std-uom,
                               v-basis-w, v-len, v-wid, v-dep,
                               ip-qty, output v-qty-comp).

      ASSIGN
         v-setup = 0
         v-index = 0.
      do i = 1 to 10:
         if v-qty-comp le e-item-vend.run-qty[i] then
         DO:
            v-index = i.
            LEAVE.
         END.
      end.
    
      IF v-index EQ 0 THEN
      DO:
         FIND FIRST b-qty WHERE
              b-qty.reftable = "vend-qty" AND
              b-qty.company = e-item-vend.company AND
              b-qty.CODE    = e-item-vend.i-no AND
              b-qty.code2   = e-item-vend.vend-no
              NO-LOCK NO-ERROR.
        
         DO i = 1 TO 10:
            IF v-qty-comp LE b-qty.val[i] THEN
            DO:
               v-index = i + 10.
               LEAVE.
            END.
         END.
      END.

      IF v-index EQ 0 THEN
         v-index = 10.

      IF v-index LE 10 THEN
         ASSIGN
            v-setup = e-item-vend.setups[i]
            op-adder-setup = op-adder-setup + v-setup
            v-cost = ((e-item-vend.run-cost[i] * v-qty-comp) + v-setup) / v-qty-comp.
      ELSE
      DO:
         FIND FIRST b-cost WHERE
              b-cost.reftable = "vend-cost" AND
              b-cost.company = e-item-vend.company AND
              b-cost.CODE    = e-item-vend.i-no AND
              b-cost.code2   = e-item-vend.vend-no
              NO-LOCK NO-ERROR.
        
         FIND FIRST b-setup WHERE
              b-setup.reftable = "vend-setup" AND
              b-setup.company = e-item-vend.company AND
              b-setup.CODE    = e-item-vend.i-no AND
              b-setup.code2   = e-item-vend.vend-no              
              NO-LOCK NO-ERROR.

         IF AVAIL b-cost AND AVAIL b-setup THEN
            ASSIGN
               v-setup = b-setup.val[v-index - 10]
               op-adder-setup = op-adder-setup + v-setup
               v-cost = ((b-cost.val[v-index - 10] * v-qty-comp) + v-setup) / v-qty-comp.
      END.
      
      /* This adds the Adder cost in */
      IF e-item.std-uom NE ap-invl.pr-qty-uom THEN
        RUN sys/ref/convcuom.p(e-item.std-uom, ap-invl.pr-qty-uom, job-mat.basis-w,
                               job-mat.len, job-mat.wid, item.s-dep,
                               v-cost, OUTPUT v-cost).
    END.

    ELSE DO:
      v-cost = job-mat.std-cost.
      
      IF job-mat.sc-uom NE ap-invl.pr-qty-uom THEN
        RUN sys/ref/convcuom.p(job-mat.sc-uom, ap-invl.pr-qty-uom, job-mat.basis-w,
                               job-mat.len, job-mat.wid, item.s-dep,
                               job-mat.std-cost, OUTPUT v-cost).
    END.

    v-add-cost = v-add-cost + v-cost.
  END.

  IF ap-invl.pr-qty-uom NE "EA" THEN 
    RUN sys/ref/convcuom.p("EA", ap-invl.pr-qty-uom,
                           v-basis-w, v-len, v-wid, v-dep,
                           v-tot-cost, OUTPUT v-tot-cost).
 
  op-cost = v-add-cost + v-tot-cost.

  IF ap-invl.pr-qty-uom NE ap-invl.cons-uom THEN
    RUN sys/ref/convcuom.p(ap-invl.pr-qty-uom, ap-invl.cons-uom,
                           v-basis-w, v-len, v-wid, v-dep,
                           ip-cost, OUTPUT op-cons-cost).

assign
 v-adder[1] = op-cost      - v-adder[1]
 v-adder[2] = op-cons-cost - v-adder[2].

END PROCEDURE.

PROCEDURE calc-amt3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR lv-invl-qty LIKE ap-invl.qty NO-UNDO.
  DEF VAR v-temp-pr AS DEC FORM "->>>,>>9.99<<<" /*LIKE ap-invl.unit-pr */ NO-UNDO.

  {ce/msfcalc.i}

  ASSIGN v-wid = po-ordl.s-wid
         v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
         v-dep = 0
         v-bwt = 0.
  
  find first ITEM where item.company eq cocode
                    and item.i-no    eq po-ordl.i-no
                    and po-ordl.item-type
                    no-lock no-error.            
  if avail item then do:
       v-dep = item.s-dep.          
       {po/pol-dims.i}
  end.
  

  if v-wid eq 0 or v-len eq 0 then do:
    if v-wid eq 0 THEN v-wid = if v-len eq 0 then 12 else v-len.
    v-len = ap-invl.sf-sht * 144 / v-wid.
  end.
  
  if v-len eq 0 then v-len = 12.
  
  if ap-invl.pr-qty-uom eq ap-invl.cons-uom OR
     ap-invl.pr-qty-uom = "0" THEN
     v-temp-pr = ap-invl.unit-pr.
  ELSE
    run sys/ref/convcuom.p (ap-invl.pr-qty-uom, ap-invl.cons-uom,
                            v-bwt, v-len, v-wid, v-dep,
                            ap-invl.unit-pr, output v-temp-pr).

  ap-invl.amt = ap-invl.qty * v-temp-pr.
  
  IF ap-invl.pr-qty-uom EQ "L" THEN ap-invl.amt = ap-invl.unit-pr.

  if ap-invl.cons-uom eq "MSF" 
     THEN ap-invl.amt-msf = ap-invl.qty.
  else  
     run sys/ref/convquom.p (ap-invl.cons-uom, "MSF",
                          v-bwt, v-len, v-wid, v-dep,
                          ap-invl.qty, output ap-invl.amt-msf).
 
END PROCEDURE.

PROCEDURE update-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-overwrite-tax AS LOG NO-UNDO.

  DEF VAR ld AS DEC NO-UNDO.
  DEF VAR v-tax-rate AS DEC NO-UNDO.
  DEF VAR v-frt-tax-rate AS DEC NO-UNDO.

  DEF BUFFER b-ap-inv  FOR ap-inv.
  DEF BUFFER b-ap-invl FOR ap-invl.


  FIND b-ap-inv WHERE ROWID(b-ap-inv) EQ ip-rowid EXCLUSIVE-LOCK NO-ERROR.

  IF AVAIL b-ap-inv THEN DO:

    IF NOT ip-overwrite-tax THEN
       b-ap-inv.tax-amt = 0.

    ASSIGN
     b-ap-inv.net     = 0
     b-ap-inv.freight = 0.

    IF b-ap-inv.tax-gr NE "" THEN
      RUN ar/cctaxrt.p (b-ap-inv.company, b-ap-inv.tax-gr,
                        OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

    FOR EACH b-ap-invl WHERE b-ap-invl.i-no EQ b-ap-inv.i-no NO-LOCK:
      b-ap-inv.net = b-ap-inv.net + b-ap-invl.amt.

      IF b-ap-invl.tax AND NOT ip-overwrite-tax THEN
        b-ap-inv.tax-amt = b-ap-inv.tax-amt +
                           ROUND((b-ap-invl.amt * v-tax-rate / 100),2).

      IF b-ap-invl.po-no NE 0 THEN DO:
        FIND FIRST po-ordl
            WHERE po-ordl.company EQ cocode
              AND po-ordl.po-no   EQ (IF b-ap-invl.po-no EQ 0 THEN b-ap-inv.po-no
                                                              ELSE b-ap-invl.po-no)
              AND po-ordl.line    EQ (b-ap-invl.line + (b-ap-invl.po-no * 1000 * -1)) 
            USE-INDEX po-no NO-ERROR.

        IF AVAIL po-ordl THEN DO:
          RUN po/getfrtcs.p (ROWID(po-ordl), b-ap-invl.qty, OUTPUT ld).
          b-ap-inv.freight = b-ap-inv.freight + ld.
        END.
      END.
    END.

    ASSIGN
     b-ap-inv.tax-amt = b-ap-inv.tax-amt +
                        ROUND((b-ap-inv.freight * v-frt-tax-rate / 100),2)
     b-ap-inv.net     = b-ap-inv.net + b-ap-inv.tax-amt
     b-ap-inv.due     = b-ap-inv.net - b-ap-inv.disc-taken -
                        b-ap-inv.paid + b-ap-inv.freight.
  END.


  FIND CURRENT b-ap-inv NO-LOCK.

END PROCEDURE.

PROCEDURE calc-setup-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER pv-bf-po-ordl-r AS ROWID.
 DEF INPUT PARAMETER pv-qty-in AS DEC.
 DEF OUTPUT PARAMETER pv-ext-cost AS DEC.
 DEF OUTPUT PARAMETER pv-qty-uom AS CHAR.
 DEF OUTPUT PARAMETER pv-out-qty AS DEC NO-UNDO.
 DEF OUTPUT PARAMETER pv-out-cost AS DEC NO-UNDO.
 DEF OUTPUT PARAMETER pv-setup-per AS DEC NO-UNDO.
 
  DEF VAR v-tot-msf AS DEC NO-UNDO.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
  DEF VAR lv-over-cost AS DEC NO-UNDO.
  def var lv-ext-cost as dec no-undo.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR lv-from-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR lv-out-ea AS DEC NO-UNDO.
  DEF VAR v-rec-qty AS INT NO-UNDO.
  DEF VAR v-job-qty AS DEC NO-UNDO.
  DEF VAR v-ord-qty AS DEC NO-UNDO.
  DEF VAR v-ord-cost AS DEC NO-UNDO .
  DEF VAR v-ord-uom  AS CHAR NO-UNDO.
  DEF VAR v-setup-qty AS DEC NO-UNDO.
  DEF VAR v-setup-per AS DEC NO-UNDO.
  DEF VAR v-ord-po-uom AS CHAR NO-UNDO.
  DEF VAR v-cost-per-ea AS DEC NO-UNDO .
  DEF VAR v-cost-setup AS DEC NO-UNDO .
  DEF VAR v-cost-with-setup AS DEC NO-UNDO .
  DEF VAR v-corr AS LOG NO-UNDO.
  DEF VAR v-basis-w AS DEC NO-UNDO. 
  DEF VAR v-out-qty AS DEC NO-UNDO.
  DEF VAR v-qty-per-msf AS DEC NO-UNDO.
  DEF VAR v-tot-cost AS DEC NO-UNDO.
  DEF BUFFER b-job-hdr FOR job-hdr.
  DEF BUFFER bf-po-ordl FOR po-ordl.
  DEF BUFFER b-po-ordl FOR po-ordl.
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF VAR fg-uom-list AS CHAR.
  DEF VAR lv-overrun-checked AS LOG.

  FIND bf-po-ordl WHERE ROWID(bf-po-ordl) = pv-bf-po-ordl-r NO-LOCK.
  RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

  if not avail bf-po-ordl then return.  /* no records */
 

DO :

 
find itemfg where itemfg.company eq cocode
              and itemfg.i-no  eq bf-po-ordl.i-no 
            use-index i-no no-lock no-error.

ASSIGN
 lv-cost-uom = itemfg.prod-uom
 v-bwt       = 0
 v-len       = itemfg.t-len
 v-wid       = itemfg.t-wid
 v-dep       = 0
 v-ord-qty   = 0
 v-ord-cost  = 0
 v-ord-uom   = ""
 v-ord-po-uom  = ""
 v-rec-qty   = 0
 v-job-qty   = 0
 v-ord-cost  = 0
 v-ord-uom   = ""
 v-ord-po-uom      = ""
 v-cost-per-ea     = 0
 v-cost-setup      = 0
 v-cost-with-setup = 0
 v-cost-setup      = 0
 v-setup-per       = 0.


IF AVAIL(bf-po-ordl) THEN DO:

  ASSIGN v-ord-qty  = pv-qty-in /* bf-po-ordl.ord-qty */
         v-ord-cost = bf-po-ordl.cons-cost
         v-ord-uom  = bf-po-ordl.cons-uom
         v-ord-po-uom = bf-po-ordl.pr-qty-uom.
END.

if /*ip-first-disp  and */ avail bf-po-ordl and bf-po-ordl.i-no <> "" then do: /* for row-display */  

  IF AVAIL bf-po-ordl THEN
    ASSIGN
     v-len = bf-po-ordl.s-len
     v-wid = bf-po-ordl.s-wid.

  ASSIGN
   lv-out-qty  = pv-qty-in /* bf-po-ordl.ord-qty */
   lv-from-uom = bf-po-ordl.pr-qty-uom
   lv-out-cost = bf-po-ordl.cost.
END. /* avail bf-po-ordl */
/* ======================================================================= */
ELSE
if avail bf-po-ordl and bf-po-ordl.i-no <> "" then do: /* in update mode - use screen-value */

  v-rec-qty = INT(bf-po-ordl.ord-qty).

  FOR EACH b-fg-rctd
      WHERE b-fg-rctd.company    EQ bf-po-ordl.company
        AND b-fg-rctd.rita-code  EQ "R"
        AND b-fg-rctd.i-no       EQ bf-po-ordl.i-no 
        AND INT(b-fg-rctd.po-no) EQ INT(bf-po-ordl.po-no )
        AND b-fg-rctd.job-no     EQ bf-po-ordl.job-no 
        AND b-fg-rctd.job-no2    EQ INT(bf-po-ordl.job-no2 )
        AND ROWID(b-fg-rctd)     NE ROWID(bf-po-ordl)
      NO-LOCK:
    v-rec-qty = v-rec-qty + b-fg-rctd.t-qty.
  END.
  
  IF AVAIL bf-po-ordl THEN DO:
    ASSIGN
     v-len = bf-po-ordl.s-len
     v-wid = bf-po-ordl.s-wid
     v-rec-qty = v-rec-qty + bf-po-ordl.t-rec-qty.

    IF LOOKUP(bf-po-ordl.pr-qty-uom,fg-uom-list) EQ 0 THEN
       RUN sys/ref/convquom.p("EA", bf-po-ordl.pr-qty-uom, 0, 0, 0, 0,
                              v-rec-qty, OUTPUT v-rec-qty).
  END.

  ASSIGN
   lv-out-qty  = DEC(pv-qty-in /* bf-po-ordl.ord-qty */)
   lv-from-uom = bf-po-ordl.pr-qty-uom 
   lv-out-cost = DEC(bf-po-ordl.cost ).
END.


IF lv-from-uom EQ "L" THEN
  ASSIGN
   lv-from-uom = "EA"
   lv-out-cost = lv-out-cost / lv-out-qty.

/* Calculate for quantity comparison purposes */
RUN rm/convquom.p(v-ord-po-uom, 'EA',                   
          v-bwt, v-len, v-wid, v-dep,
          v-ord-qty, OUTPUT v-ord-qty).

RUN rm/convquom.p(v-ord-po-uom, 'EA',            
          v-bwt, v-len, v-wid, v-dep,
          lv-out-qty, OUTPUT lv-out-ea).

/* convert cost pr-uom*/

IF lv-from-uom EQ lv-cost-uom               OR
   (LOOKUP(lv-from-uom,fg-uom-list) GT 0 AND
    LOOKUP(lv-cost-uom,fg-uom-list) GT 0)   THEN.
ELSE
  RUN rm/convcuom.p(lv-from-uom, lv-cost-uom,                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-cost, OUTPUT lv-out-cost).

IF LOOKUP(lv-cost-uom,fg-uom-list) EQ 0 THEN
  RUN rm/convquom.p("EA", lv-cost-uom,                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-qty).

 /* get quantity in eaches */
  RUN rm/convquom.p(lv-cost-uom, "EA",                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-ea).

 ASSIGN v-basis-w = 0
        v-corr = NO.
   lv-over-cost = bf-po-ordl.cost.
   IF bf-po-ordl.PR-uom EQ lv-cost-uom               OR
     (LOOKUP(lv-from-uom,fg-uom-list) GT 0 AND
      LOOKUP(lv-cost-uom,fg-uom-list) GT 0)   THEN.
   ELSE
     RUN rm/convcuom.p(bf-po-ordl.PR-uom, lv-cost-uom, 
                       v-bwt, v-len, v-wid, v-dep,
                       bf-po-ordl.cost, OUTPUT lv-over-cost).

   v-setup-qty = lv-out-ea.
   IF lv-cost-uom NE 'EA' THEN
       RUN rm/convquom.p("EA", lv-cost-uom,                   
                         v-bwt, v-len, v-wid, v-dep,
                         lv-out-ea, OUTPUT v-setup-qty).

   lv-out-cost = lv-over-cost + (bf-po-ordl.setup / v-setup-qty).
   v-setup-per = bf-po-ordl.setup / v-setup-qty.

/* These should be to screen value */
ASSIGN
  pv-out-qty  = lv-out-qty
  lv-ext-cost = lv-out-qty * lv-out-cost
  pv-qty-uom  = lv-cost-uom
  pv-out-cost  = lv-out-cost 
  pv-ext-cost  = lv-ext-cost /* +DEC(bf-po-ordl.frt-cost ) */
  pv-setup-per = v-setup-per.
 
END.

END PROCEDURE.
