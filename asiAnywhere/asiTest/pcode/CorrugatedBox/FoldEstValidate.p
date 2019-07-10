

                                 
/*------------------------------------------------------------------------
    File        : FoldEstValidate.p
    Purpose     : Validation for add order

    Syntax      :

    Description : Validation for add order

    Author(s)   : Sewa
    Created     : sep 22, 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE INPUT PARAMETER prmAction            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmUser              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEstimate          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCust              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustPart          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmShipTo            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmItemName          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFgItem            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmStyle             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFlute             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmTest              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBoard             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCalliper          AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmCategory          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmLength            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmWidth             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmDepth             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmFrom              AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmBlank             AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmTab               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmColor             AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmPasses            AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmCoating           AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmCoatPasses        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmQtySet            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmInkFrom           AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmPassesFrom        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmCoatingFrom       AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmCoatPassesFrom    AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmPurchManuf        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEstDate           AS DATETIME NO-UNDO.
    DEFINE INPUT PARAMETER prmType              AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER prmDiein             AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER cError              AS CHAR NO-UNDO.
    


    IF   prmAction         = ?      THEN    prmAction       = "".       
    IF   prmUser           = ?      THEN    prmUser         = "".       
    IF   prmEstimate       = ?      THEN    prmEstimate     = "".       
    IF   prmCust           = ?      THEN    prmCust         = "".       
    IF   prmCustPart       = ?      THEN    prmCustPart     = "".       
    IF   prmShipTo         = ?      THEN    prmShipTo       = "".       
    IF   prmItemName       = ?      THEN    prmItemName     = "".       
    IF   prmFgItem         = ?      THEN    prmFgItem       = "".       
    IF   prmEstQty         = ?      THEN    prmEstQty       = 0.       
    IF   prmStyle          = ?      THEN    prmStyle        = "".       
    IF   prmFlute          = ?      THEN    prmFlute        = "".       
    IF   prmTest           = ?      THEN    prmTest         = "".       
    IF   prmBoard          = ?      THEN    prmBoard        = "".       
    IF   prmCalliper       = ?      THEN    prmCalliper     = 0.       
    IF   prmCategory       = ?      THEN    prmCategory     = "".       
    IF   prmLength         = ?      THEN    prmLength       = 0.       
    IF   prmWidth          = ?      THEN    prmWidth        = 0.       
    IF   prmDepth          = ?      THEN    prmDepth        = 0.       
    IF   prmFrom           = ?      THEN    prmFrom         = 0.       
    IF   prmBlank          = ?      THEN    prmBlank        = 0.       
    IF   prmTab            = ?      THEN    prmTab          = "".       
    IF   prmColor          = ?      THEN    prmColor        = 0.       
    IF   prmPasses         = ?      THEN    prmPasses       = 0.       
    IF   prmCoating        = ?      THEN    prmCoating      = 0.       
    IF   prmCoatPasses     = ?      THEN    prmCoatPasses   = 0.       
    IF   prmQtySet         = ?      THEN    prmQtySet       = 1.       
    IF   prmInkFrom        = ?      THEN    prmInkFrom      = 1.       
    IF   prmPassesFrom     = ?      THEN    prmPassesFrom   = 1.       
    IF   prmCoatingFrom    = ?      THEN    prmCoatingFrom  = 1.       
    IF   prmCoatPassesFrom = ?      THEN    prmCoatPassesFrom =1. 
    IF   prmDiein          = ?      THEN    prmDiein          =0.
    IF   prmPurchManuf     = ?      THEN    prmPurchManuf     = "".


 
{cec/descalc.i "new"}
    def buffer bb for eb.
    def buffer bf for ef.
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-ef FOR ef.
  def buffer bf-eb for eb.
   def buffer bf-est for est.
  def buffer bf-est-qty for est-qty.
      DEF BUFFER bf1-eb FOR eb.
def buffer bqty for est-qty.
  def buffer best for est.
 def var ll-dum as log no-undo.
  def var lv-ef-recid as recid no-undo.
  def var lv-eb-recid as recid no-undo.
  DEF VAR ll-mass-del AS LOG NO-UNDO.
  def var li-est-type like est.est-type no-undo.
  def var lv-ind like style.industry no-undo.

  DEF VAR vTr-no AS CHAR NO-UNDO.

  def var char-val as cha no-undo.
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared buffer xqty for est-qty.
DEF TEMP-TABLE tt-est-op LIKE est-op.
def NEW SHARED var cocode     as   char  format "x(3)"  no-undo.
def NEW SHARED var locode     as   char  format "x(5)"  no-undo.
def NEW SHARED var  x  as   int no-undo.
def NEW SHARED var  y  as   int no-undo.
DEF NEW SHARED  VAR  k  as   int no-undo.
def new shared var xcal as dec no-undo.
def new shared var sh-wid as dec no-undo.
def new shared var sh-len as dec no-undo.
def new shared temp-table formule field formule as dec extent 12.

/*{sys/inc/var.i SHARED} */
def var xx as dec no-undo.
    DEF BUFFER recalc-mr FOR reftable.
def var li-new-estnum as INT no-undo.
DEF VAR li AS INT NO-UNDO.
DEFINE VAR bi AS INT NO-UNDO.
DEF VAR prev-cust LIKE eb.cust-no NO-UNDO.
DEF VAR prev-ship LIKE eb.ship-id NO-UNDO.
DEF VAR prev-style LIKE eb.style NO-UNDO.
DEF VAR ls-part-no AS cha NO-UNDO.
DEF VAR prev-yrprice LIKE eb.yrprice NO-UNDO.
def var ll-is-copy-record as log no-undo.
DEF VAR ll-new-shipto AS LOG NO-UNDO.
DEF VAR lv-copy-what AS cha NO-UNDO.
DEFINE VAR vQty AS DECIMAL NO-UNDO.
DEFINE VAR prmLoc AS CHAR NO-UNDO.
     def var lv-rowid as rowid no-undo.
    def var ll-dumb as log no-undo.
    def var char-hdl as cha no-undo.
    def var li-row-num as int no-undo.
    def var li-cnt as int no-undo.
    DEF VAR lv-part-no LIKE eb.part-no NO-UNDO.
    DEF VAR lv-msg AS CHAR NO-UNDO.
    DEFINE VAR CorType AS INTEGER NO-UNDO.
    DEFINE VAR varEqty AS INT NO-UNDO.
    def var lv-estqty-recid as recid no-undo.
    def var char-val2 as cha no-undo.        
   def var date-val as cha no-undo.
     def var date-val2 as cha no-undo.
     def var lv-copy-qty as int extent 20 no-undo.

    def var k_frac as dec init 6.25 no-undo.
    def var ls-add-what as cha no-undo.

    DEFINE VAR prmComp AS CHAR NO-UNDO.
    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
prmLoc  =  "MAIN" .
ASSIGN
    cocode = prmComp
    locode = prmLoc  .
  

/*****************************add******************************/

IF prmAction = "Add" THEN do:

    
    /* Code placed here will execute PRIOR to standard behavior. */
    IF prmFgItem <> "" THEN DO:
        FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"
            AND reftable.company  EQ prmComp
            AND reftable.loc      EQ ""
            AND reftable.code     EQ prmFgItem    NO-LOCK NO-ERROR.
          
             IF AVAIL reftable AND reftable.code2 = "I" THEN DO:
                 ASSIGN
                     cError  = prmFgItem +  "has InActive Status  Order cannot be placed for the Inactive Item.".
                 RETURN .
                 END.
         END.


/**custpart*/
 
 ASSIGN
     lv-part-no = prmCustPart
     lv-msg     = "".

    IF lv-part-no EQ ""                                                     OR
       (CAN-FIND(FIRST b-eb OF ef
                 WHERE b-eb.part-no EQ lv-part-no
                   AND (ROWID(b-eb) NE ROWID(eb) OR ll-is-copy-record)) AND
        (lv-copy-what NE "form" OR NOT ll-is-copy-record))                  THEN
      lv-msg = IF lv-part-no EQ "" THEN "Cust Part may not be blank  "
                                   ELSE "already exists on Form #" +
                                        TRIM(STRING(ef.form-no,">>>")).

    IF lv-msg NE "" THEN DO:
      cError = prmCustPart + " " +
              TRIM(lv-msg) + "..." .
     
      RETURN .
    END.
/*endcustpart*/    
    

 FIND  FIRST style  WHERE style.company  = prmComp
                      AND style.style    EQ prmStyle AND prmStyle <> ""
                      AND (style.industry EQ "1")  NO-LOCK NO-ERROR.
        IF NOT AVAIL  style  THEN DO:
      cError =  "Invalid Style, try help..." .
      RETURN .
   END.

 IF prmShipTo <> "" THEN DO:
     FIND  FIRST shipto WHERE shipto.company EQ prmComp
                      AND shipto.cust-no EQ prmCust
                      AND shipto.ship-id EQ prmShipTo NO-LOCK NO-ERROR.
      IF NOT AVAIL shipto THEN DO:
          ASSIGN 
      cError = " Invalid Ship To, try help...             " .
        RETURN. 
        END.
  END.

  prmCategory =  CAPS(prmCategory).
   FIND  FIRST fgcat  WHERE fgcat.company EQ prmComp AND prmCategory <> ""
                      AND (fgcat.procat  EQ prmCategory)  NO-LOCK NO-ERROR.
       IF NOT AVAIL  fgcat THEN DO:
      cError =  "Invalid Category, try help..." .
      RETURN .
    END.
    
     if int(prmEstQty) <= 0 then do:
         ASSIGN 
             cError =  "Quantity must be entered. ".
         return .
         end.
    
         if /* eb.cust-no:screen-value <> "" and */
             not can-find(cust where cust.company = prmComp and cust.cust-no = prmCust)
             then do:
                ASSIGN
                    cError =  "Invalid Customer Number. Try Help." .
                return .
                end.
     
  FIND FIRST item where item.company = prmComp
                    and item.i-no = prmBoard AND prmBoard <> "" NO-LOCK NO-ERROR.
  IF NOT AVAIL ITEM then do:
         ASSIGN
             cError =  "Invalid Board. Try Help. " .
         return .
  end.
     
    /* RUN valid-wid-len NO-ERROR.*/

    if decimal(prmLength) = 0 then do:
        ASSIGN 
            cError =  "Length can not be 0. " .
        return .
        end.
   if decimal(prmWidth) = 0 then do:
       ASSIGN
           cError =  "Width can not be 0. " .
       return .
       end.

  IF prmBoard <> ""  THEN DO:
         find style where style.company = prmComp and
                            style.style = prmStyle
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then  DO:
              FIND FIRST item WHERE  item.company = prmComp and  (item.industry = lv-ind or lv-ind = "") 
                   and item.mat-type >= "1" and item.mat-type <= "4" AND item.i-no = prmBoard  NO-LOCK NO-ERROR.
              IF NOT AVAIL ITEM THEN DO:
                   ASSIGN
                       cError =  "Invalid Board. Try Help. " .
                   return .
                   END.
              END.
           ELSE DO:
               FIND FIRST item WHERE item.company = prmComp and (item.industry = lv-ind or lv-ind = "") 
                   and (item.mat-type = 'B' or item.mat-type = 'P' or item.mat-type = 'R') AND item.i-no = prmBoard NO-LOCK  NO-ERROR.
                   IF NOT AVAIL ITEM THEN DO:
                   ASSIGN
                       cError =  "Invalid Board. Try Help. " .
                   return .
                   END.
           END.
  END.

 
IF prmFlute <> "" THEN DO:
 FIND FIRST  item WHERE  item.company = prmComp and ( item.industry = "1" ) 
     and ( item.mat-type = "P" ) AND ITEM.i-no = prmFlute   NO-LOCK NO-ERROR.
 IF NOT AVAIL ITEM  THEN DO:
     ASSIGN 
         cError = "Invalid Paper 1......Try help".
        RETURN.
 END.
END.

IF prmTest <> "" THEN DO:
 FIND FIRST  item WHERE  item.company = prmComp and ( item.industry = "1" ) 
     and ( item.mat-type = "P" ) AND ITEM.i-no = prmTest   NO-LOCK NO-ERROR.
 IF NOT AVAIL ITEM  THEN DO:
     ASSIGN 
         cError = "Invalid Paper 2......Try help".
        RETURN.
 END.
END.



END.  /* end of vali*/

/******************Delete***************************/
/*
IF prmAction = "Delete" THEN DO:
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)    AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)    AND eb.company = prmComp NO-LOCK NO-ERROR. 
     FOR EACH del-eb NO-LOCK
      WHERE del-eb.company EQ eb.company
        AND del-eb.est-no  EQ eb.est-no
        AND ((del-eb.form-no EQ eb.form-no AND
              del-eb.blank-no EQ eb.blank-no) /*OR ll-mass-del*/ ):

    IF ((del-eb.ord-no NE 0 AND
         CAN-FIND(FIRST oe-ordl
                  WHERE oe-ordl.company EQ del-eb.company
                    AND oe-ordl.ord-no  EQ del-eb.ord-no
                    AND oe-ordl.est-no  EQ del-eb.est-no)) OR
        CAN-FIND(FIRST job-hdr
                 WHERE job-hdr.company  EQ est.company
                   AND job-hdr.est-no   EQ est.est-no
                   AND job-hdr.opened   EQ YES
                   AND job-hdr.frm      EQ del-eb.form-no
                   AND job-hdr.blank-no EQ del-eb.blank-no))    AND
       (est.est-type NE 8 OR
        (est.form-qty EQ 1 AND ef.blank-qty EQ 1))              THEN DO:
      IF est.est-type EQ 6 THEN DO:
        cError =  "An order and/or job exists for this estimate, cannot delete..." .
          RETURN .
           END.
      ELSE
        cError =  "An order and/or job exists for this estimate, cannot delete..." .
           RETURN .
      
      
    END.
  END.
 END.

 /****************************MassDelete********************************/

 IF prmAction = "MassDelete" THEN  DO:

     FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)    AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)    AND eb.company = prmComp NO-LOCK NO-ERROR. 
     FOR EACH del-eb NO-LOCK
      WHERE del-eb.company EQ eb.company
        AND del-eb.est-no  EQ eb.est-no
        AND ((del-eb.form-no EQ eb.form-no AND
              del-eb.blank-no EQ eb.blank-no) /*OR ll-mass-del*/ ):

    IF ((del-eb.ord-no NE 0 AND
         CAN-FIND(FIRST oe-ordl
                  WHERE oe-ordl.company EQ del-eb.company
                    AND oe-ordl.ord-no  EQ del-eb.ord-no
                    AND oe-ordl.est-no  EQ del-eb.est-no)) OR
        CAN-FIND(FIRST job-hdr
                 WHERE job-hdr.company  EQ est.company
                   AND job-hdr.est-no   EQ est.est-no
                   AND job-hdr.opened   EQ YES
                   AND job-hdr.frm      EQ del-eb.form-no
                   AND job-hdr.blank-no EQ del-eb.blank-no))    AND
       (est.est-type NE 8 OR
        (est.form-qty EQ 1 AND ef.blank-qty EQ 1))              THEN DO:
      IF est.est-type EQ 6 THEN DO:
        cError =  "An order and/or job exists for this estimate, cannot delete..." .
          RETURN .
           END.
      ELSE
        cError =  "An order and/or job exists for this estimate, cannot delete..." .
           RETURN .
      
      
    END.
  END.


 END.*/

 /************************update*************************/


IF prmAction = "Update" THEN DO:
    
    /* Code placed here will execute PRIOR to standard behavior. */
    IF prmFgItem <> "" THEN DO:
        FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"
            AND reftable.company  EQ prmComp
            AND reftable.loc      EQ ""
            AND reftable.code     EQ prmFgItem    NO-LOCK NO-ERROR.
          
             IF AVAIL reftable AND reftable.code2 = "I" THEN DO:
                 ASSIGN
                     cError  = prmFgItem +  "has InActive Status  Order cannot be placed for the Inactive Item.".
                 RETURN .
                 END.
         END.

 ASSIGN
     lv-part-no = prmCustPart
     lv-msg     = "".

    IF lv-part-no EQ ""                                                     OR
       (CAN-FIND(FIRST b-eb OF ef
                 WHERE b-eb.part-no EQ lv-part-no
                   AND (ROWID(b-eb) NE ROWID(eb) OR ll-is-copy-record)) AND
        (lv-copy-what NE "form" OR NOT ll-is-copy-record))                  THEN
      lv-msg = IF lv-part-no EQ "" THEN "may not be blank "
                                   ELSE "already exists on Form #" +
                                        TRIM(STRING(ef.form-no,">>>")).

    IF lv-msg NE "" THEN DO:
      cError = prmCustPart + " " +
              TRIM(lv-msg) + "..." .
     
      RETURN .
    END.
/*endcustpart*/    

FIND  FIRST style  WHERE style.company  = prmComp
                      AND style.style    EQ prmStyle AND prmStyle <> ""
                      AND (style.industry EQ "1")  NO-LOCK NO-ERROR.
        IF NOT AVAIL  style  THEN DO:
      cError =  "Invalid Style, try help..." .
      RETURN .
 END.

 IF prmShipTo <> "" THEN DO:
     FIND  FIRST shipto WHERE shipto.company EQ prmComp
                      AND shipto.cust-no EQ prmCust
                      AND shipto.ship-id EQ prmShipTo NO-LOCK NO-ERROR.
      IF NOT AVAIL shipto THEN DO:
          ASSIGN 
      cError = " Invalid Ship To, try help...             " .
        RETURN. 
        END.
  END.

  prmCategory =  CAPS(prmCategory).
   FIND  FIRST fgcat  WHERE fgcat.company EQ prmComp AND prmCategory <> ""
                      AND (fgcat.procat  EQ prmCategory)  NO-LOCK NO-ERROR.
       IF NOT AVAIL  fgcat THEN DO:
      cError =  "Invalid Category, try help..." .
      RETURN .
    END.
    
     if int(prmEstQty) <= 0 then do:
         ASSIGN 
             cError =  "Quantity must be entered. ".
         return .
         end.
    
         if /* eb.cust-no:screen-value <> "" and */
             not can-find(cust where cust.company = prmComp and cust.cust-no = prmCust)
             then do:
                ASSIGN
                    cError =  "Invalid Customer Number. Try Help." .
                return .
                end.
     
   FIND FIRST item where item.company = prmComp
                    and item.i-no = prmBoard AND prmBoard <> "" NO-LOCK NO-ERROR.
  IF NOT AVAIL ITEM then do:
         ASSIGN
             cError =  "Invalid Board. Try Help. " .
         return .
  end.
     
    /* RUN valid-wid-len NO-ERROR.*/

    if decimal(prmLength) = 0 then do:
        ASSIGN 
            cError =  "Length can not be 0. " .
        return .
        end.
   if decimal(prmWidth) = 0 then do:
       ASSIGN
           cError =  "Width can not be 0. " .
       return .
       end.

   IF prmFlute <> "" THEN DO:
 FIND FIRST  item WHERE  item.company = prmComp and ( item.industry = "1" ) 
     and ( item.mat-type = "P" ) AND ITEM.i-no = prmFlute   NO-LOCK NO-ERROR.
 IF NOT AVAIL ITEM  THEN DO:
     ASSIGN 
         cError = "Invalid Paper 1......Try help".
        RETURN.
 END.
END.

IF prmTest <> "" THEN DO:
 FIND FIRST  item WHERE  item.company = prmComp and ( item.industry = "1" ) 
     and ( item.mat-type = "P" ) AND ITEM.i-no = prmTest   NO-LOCK NO-ERROR.
 IF NOT AVAIL ITEM  THEN DO:
     ASSIGN 
         cError = "Invalid Paper 2......Try help".
        RETURN.
 END.
END.

IF prmBoard <> ""  THEN DO:
         find style where style.company = prmComp and
                            style.style = prmStyle
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then  DO:
              FIND FIRST item WHERE  item.company = prmComp and  (item.industry = lv-ind or lv-ind = "") 
                   and item.mat-type >= "1" and item.mat-type <= "4" AND item.i-no = prmBoard  NO-LOCK NO-ERROR.
              IF NOT AVAIL ITEM THEN DO:
                   ASSIGN
                       cError =  "Invalid Board. Try Help. " .
                   return .
                   END.
              END.
           ELSE DO:
               FIND FIRST item WHERE item.company = prmComp and (item.industry = lv-ind or lv-ind = "") 
                   and (item.mat-type = 'B' or item.mat-type = 'P' or item.mat-type = 'R') AND item.i-no = prmBoard NO-LOCK  NO-ERROR.
                   IF NOT AVAIL ITEM THEN DO:
                   ASSIGN
                       cError =  "Invalid Board. Try Help. " .
                   return .
                   END.
           END.
  END.

  /* ====== end validation =======*/
  
END.

/*******************add a form ***********************************/
IF prmAction = "FormEstimate" THEN DO:

     /* Code placed here will execute PRIOR to standard behavior. */
    IF prmFgItem <> "" THEN DO:
        FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"
            AND reftable.company  EQ prmComp
            AND reftable.loc      EQ ""
            AND reftable.code     EQ prmFgItem    NO-LOCK NO-ERROR.
          
             IF AVAIL reftable AND reftable.code2 = "I" THEN DO:
                 ASSIGN
                     cError  = prmFgItem +  "has InActive Status  Order cannot be placed for the Inactive Item.".
                 RETURN .
                 END.
         END.

    
 ASSIGN
     lv-part-no = prmCustPart
     lv-msg     = "".

    IF lv-part-no EQ ""                                                     OR
       (CAN-FIND(FIRST b-eb OF ef
                 WHERE b-eb.part-no EQ lv-part-no
                   AND (ROWID(b-eb) NE ROWID(eb) OR ll-is-copy-record)) AND
        (lv-copy-what NE "form" OR NOT ll-is-copy-record))                  THEN
      lv-msg = IF lv-part-no EQ "" THEN "may not be blank "
                                   ELSE "already exists on Form #" +
                                        TRIM(STRING(ef.form-no,">>>")).

    IF lv-msg NE "" THEN DO:
      cError = prmCustPart + " " +
              TRIM(lv-msg) + "..." .
     
      RETURN .
    END.
/*endcustpart*/    

 FIND  FIRST style  WHERE style.company  = prmComp
                      AND style.style    EQ prmStyle
                      AND (style.industry EQ "1")  NO-LOCK NO-ERROR.
        IF NOT AVAIL  style  THEN DO:
      cError =  "Invalid Style, try help..." .
      RETURN .
 END.

 IF prmShipTo <> "" THEN DO:
     FIND  FIRST shipto WHERE shipto.company EQ prmComp
                      AND shipto.cust-no EQ prmCust
                      AND shipto.ship-id EQ prmShipTo NO-LOCK NO-ERROR.
      IF NOT AVAIL shipto THEN DO:
          ASSIGN 
      cError = " Invalid Ship To, try help...             " .
        RETURN. 
        END.
  END.

  prmCategory =  CAPS(prmCategory).
   FIND  FIRST fgcat  WHERE fgcat.company EQ prmComp AND prmCategory <> ""
                      AND (fgcat.procat  EQ prmCategory)  NO-LOCK NO-ERROR.
       IF NOT AVAIL  fgcat THEN DO:
      cError =  "Invalid Category, try help..." .
      RETURN .
    END.
    
     if int(prmEstQty) <= 0 then do:
         ASSIGN 
             cError =  "Quantity must be entered. ".
         return .
         end.
    
         if /* eb.cust-no:screen-value <> "" and */
             not can-find(cust where cust.company = prmComp and cust.cust-no = prmCust)
             then do:
                ASSIGN
                    cError =  "Invalid Customer Number. Try Help." .
                return .
                end.
     
   FIND FIRST item where item.company = prmComp
                    and item.i-no = prmBoard AND prmBoard <> "" NO-LOCK NO-ERROR.
  IF NOT AVAIL ITEM then do:
         ASSIGN
             cError =  "Invalid Board. Try Help. " .
         return .
  end.
     
    /* RUN valid-wid-len NO-ERROR.*/

    if decimal(prmLength) = 0 then do:
        ASSIGN 
            cError =  "Length can not be 0. " .
        return .
        end.
   if decimal(prmWidth) = 0 then do:
       ASSIGN
           cError =  "Width can not be 0. " .
       return .
       end.

  IF prmFlute <> "" THEN DO:
 FIND FIRST  item WHERE  item.company = prmComp and ( item.industry = "1" ) 
     and ( item.mat-type = "P" ) AND ITEM.i-no = prmFlute   NO-LOCK NO-ERROR.
 IF NOT AVAIL ITEM  THEN DO:
     ASSIGN 
         cError = "Invalid Paper 1......Try help".
        RETURN.
 END.
END.

IF prmTest <> "" THEN DO:
 FIND FIRST  item WHERE  item.company = prmComp and ( item.industry = "1" ) 
     and ( item.mat-type = "P" ) AND ITEM.i-no = prmTest   NO-LOCK NO-ERROR.
 IF NOT AVAIL ITEM  THEN DO:
     ASSIGN 
         cError = "Invalid Paper 2......Try help".
        RETURN.
 END.
END.

IF prmBoard <> ""  THEN DO:
         find style where style.company = prmComp and
                            style.style = prmStyle
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then  DO:
              FIND FIRST item WHERE  item.company = prmComp and  (item.industry = lv-ind or lv-ind = "") 
                   and item.mat-type >= "1" and item.mat-type <= "4" AND item.i-no = prmBoard  NO-LOCK NO-ERROR.
              IF NOT AVAIL ITEM THEN DO:
                   ASSIGN
                       cError =  "Invalid Board. Try Help. " .
                   return .
                   END.
              END.
           ELSE DO:
               FIND FIRST item WHERE item.company = prmComp and (item.industry = lv-ind or lv-ind = "") 
                   and (item.mat-type = 'B' or item.mat-type = 'P' or item.mat-type = 'R') AND item.i-no = prmBoard NO-LOCK  NO-ERROR.
                   IF NOT AVAIL ITEM THEN DO:
                   ASSIGN
                       cError =  "Invalid Board. Try Help. " .
                   return .
                   END.
           END.
  END.

END.
