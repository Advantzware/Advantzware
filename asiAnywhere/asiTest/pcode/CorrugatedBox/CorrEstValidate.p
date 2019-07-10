

                                 
/*------------------------------------------------------------------------
    File        : CorrEstValidate.p
    Purpose     : Validation for add order

    Syntax      :

    Description : Validation for add order

    Author(s)   : Sewa
    Created     : sep 15, 2009
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
    DEFINE INPUT PARAMETER prmMassType          AS CHAR NO-UNDO.
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
    IF   prmCoatPassesFrom = ?      THEN    prmCoatPassesFrom = 1.     
    IF   prmPurchManuf     = ?      THEN    prmPurchManuf     = "".


   
def buffer bb for eb.
def buffer bf for ef.
DEF BUFFER b-eb FOR eb.
DEF BUFFER b-ef FOR ef.
DEF BUFFER del-eb FOR eb.
DEF BUFFER bf1-eb FOR eb.
DEFINE TEMP-TABLE ttTestLook NO-UNDO 
FIELD vCorrTest         AS CHARACTER .
DEF TEMP-TABLE tt-eb LIKE eb FIELD row-id AS ROWID INDEX row-id row-id.

DEF VAR ll-valid AS LOG INIT NO NO-UNDO.
DEF VAR li-pnt AS INT NO-UNDO.

 
  def buffer bqty for est-qty.
  def buffer best for est.
 
  def var ll-dum as log no-undo.
  def var lv-ef-recid as recid no-undo.
  def var lv-eb-recid as recid no-undo.
  DEF VAR ll-mass-del AS LOG NO-UNDO.
  def var li-est-type like est.est-type no-undo.
  

/*{sys/inc/var.i SHARED} */
DEF BUFFER recalc-mr FOR reftable.
def var li-new-estnum as INT no-undo.
DEF VAR li AS INT NO-UNDO.
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
def var lv-hld-eqty like est-qty.eqty no-undo.
def var xx as dec no-undo.
def var co as int no-undo.

def var k_frac as dec init 6.25 no-undo.
def var ls-add-what as cha no-undo.
def buffer bf-est for est.

DEF BUFFER xbox-design-hdr FOR box-design-hdr.
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
DEF NEW SHARED TEMP-TABLE tt-eb-set NO-UNDO LIKE eb.
DEF NEW SHARED var cocode     as   char  format "x(3)"  no-undo.
DEF NEW SHARED var locode     as   char  format "x(5)"  no-undo.
def new shared var xcal as dec no-undo.
def new shared var sh-wid as dec no-undo.
def new shared var sh-len as dec no-undo.
DEF new shared temp-table formule field formule as dec extent 12.


DEF NEW SHARED var  x  as   int no-undo.
DEF NEW SHARED var  y  as   int no-undo.
DEF NEW SHARED  VAR  k  as   int no-undo.
DEF VAR lv-hld-style LIKE eb.style NO-UNDO.
DEF VAR vTr-no AS CHAR NO-UNDO.

def var i          as   int no-undo.
def var j          as   int no-undo.

DEF VAR lv-cad-path AS cha NO-UNDO.  /* cad file - boximage path for Fibre */
DEF VAR lv-cad-ext AS cha NO-UNDO.
DEF VAR dieFile AS CHARACTER NO-UNDO.
DEF VAR cadFile AS CHARACTER NO-UNDO.
def var lv-estqty-recid as recid no-undo.
def var char-val as cha no-undo.
def var char-val2 as cha no-undo. 
def var date-val as cha no-undo.
     def var date-val2 as cha no-undo.
     def var lv-copy-qty as int extent 20 no-undo.
     DEF VAR ls-delete AS CHAR NO-UNDO.
     DEF VAR li-delete AS INT NO-UNDO.


DEF TEMP-TABLE tt-est-op LIKE est-op.
{cec/descalc.i new}
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
        ASSIGN
            cError = prmCustPart + " " +
              TRIM(lv-msg) + "..." .
     
      RETURN .
    END.
/*endcustpart*/    
    

 FIND  FIRST style  WHERE style.company  = prmComp
                      AND style.style    EQ prmStyle AND prmStyle <> ""
                      AND (style.industry EQ "2")    NO-LOCK NO-ERROR.
 
        IF NOT AVAIL  style  THEN DO:
    ASSIGN  cError =  "Invalid Style, try help..." .
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
   FIND  FIRST fgcat  WHERE fgcat.company EQ prmComp
                      AND (fgcat.procat  EQ prmCategory)  NO-LOCK NO-ERROR.
       IF NOT AVAIL  fgcat THEN DO:
           ASSIGN
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
    IF NOT AVAIL ITEM  then do:
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
            FIND FIRST flute WHERE flute.company EQ cocode AND flute.CODE = prmFlute NO-LOCK NO-ERROR.
            IF NOT AVAIL flute THEN DO:
             cError = "Invalid Flute, try help...".
            RETURN .
            END.
       END.

       IF prmTest <> ""  THEN DO:
              FOR EACH stack-flute WHERE stack-flute.company EQ cocode
                  AND stack-flute.loc     EQ prmLoc
                  AND stack-flute.code    EQ prmFlute  NO-LOCK:
                  
                  li-pnt = 1.                            
                  DO WHILE (NOT ll-valid) AND li-pnt LE 16:  
                      ll-valid = stack-flute.row-value[li-pnt] EQ prmTest.
                      li-pnt = li-pnt + 1.
                  END.
             IF ll-valid THEN LEAVE.
              END.
             IF NOT ll-valid THEN DO:
                 cError = "Invalid test, try help..." .
                 RETURN .
            END.  
           END.
                                  
    

END.  /* end of vali*/

/******************Delete***************************/

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


 END.

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

         
   /* RUN blank-cp (NO).*/

/**custpart*/
 
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
        ASSIGN
            cError = prmCustPart + " " +
              TRIM(lv-msg) + "..." .
     
      RETURN .
    END.
/*endcustpart*/    
  
 
 FIND  FIRST style  WHERE style.company  = prmComp
                      AND style.style    EQ prmStyle AND prmStyle <> ""
                      AND (style.industry EQ "2")  NO-LOCK NO-ERROR.
        IF NOT AVAIL  style  THEN DO:
            ASSIGN
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
   FIND  FIRST fgcat  WHERE fgcat.company EQ prmComp
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
    IF NOT AVAIL ITEM  then do:
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
            FIND FIRST flute WHERE flute.company EQ cocode AND flute.CODE = prmFlute NO-LOCK NO-ERROR.
            IF NOT AVAIL flute THEN DO:
             cError = "Invalid Flute, try help...".
            RETURN .
            END.
       END.
     
         IF prmTest <> ""  THEN DO:
              FOR EACH stack-flute WHERE stack-flute.company EQ cocode
                  AND stack-flute.loc     EQ prmLoc
                  AND stack-flute.code    EQ prmFlute  NO-LOCK:
                  
                  li-pnt = 1.                            
                  DO WHILE (NOT ll-valid) AND li-pnt LE 16:  
                    
                      ll-valid = stack-flute.row-value[li-pnt] EQ prmTest.
                      li-pnt = li-pnt + 1.
                       
                  END.
              END.
              IF ll-valid THEN LEAVE.
             IF NOT ll-valid THEN DO:
                 cError = "Invalid test, try help..." .
                 RETURN .
            END.  
           END.
           

  /* ====== end validation =======*/
  
END.

/*******************add a form ***********************************/

IF prmAction = "FormEstimate" THEN  DO:
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
                               
          
              FIND  FIRST style  WHERE style.company  = prmComp
                  AND style.style    EQ prmStyle
                  /*AND (style.industry EQ "1")*/  NO-LOCK NO-ERROR.
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
         FIND  FIRST fgcat  WHERE fgcat.company EQ prmComp
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
    IF NOT AVAIL ITEM  then do:
         ASSIGN
             cError =  "Invalid Board. Try Help. " .
         return .
     end.
               

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
                    FIND FIRST flute WHERE flute.company EQ cocode AND flute.CODE = prmFlute NO-LOCK NO-ERROR.
                    IF NOT AVAIL flute THEN DO:
                        cError = "Invalid Flute, try help...".
                        RETURN .
                     END.
                 END.

                 IF prmTest <> ""  THEN DO:
                     FOR EACH stack-flute WHERE stack-flute.company EQ cocode
                         AND stack-flute.loc     EQ prmLoc
                         AND stack-flute.code    EQ prmFlute  NO-LOCK:

                         li-pnt = 1.                            
                         DO WHILE (NOT ll-valid) AND li-pnt LE 16:  
                             ll-valid = stack-flute.row-value[li-pnt] EQ prmTest.
                             li-pnt = li-pnt + 1.
                          END.

                          IF ll-valid THEN LEAVE.
                          END.
                          IF NOT ll-valid THEN DO:
                              cError = "Invalid test, try help..." .
                              RETURN .
                          END.  
                    END.
 END.
