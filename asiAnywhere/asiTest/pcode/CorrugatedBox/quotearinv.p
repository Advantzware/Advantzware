            /*------------------------------------------------------------------------
    File        : quotearinv.p
    Purpose     : qoute

    Syntax      :

    Description : Return a Dataset of all quotelook

    Author(s)   : 
    Created     : Aug 18 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttQuotearinvpopup NO-UNDO 
    FIELD arinv-ino     AS CHAR
    FIELD arinv-name    AS CHAR
    FIELD arinv-qty     AS INT
    FIELD arinv-unitpr  AS INT
    FIELD arinv-qtyuom  AS CHAR .
   


DEFINE DATASET dsQuotearinvpopup FOR ttQuotearinvpopup.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuotearinvpopup.
       
    MESSAGE "prmQuote" prmQuote .

DEF VAR lv-num-rec AS INT NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-part-no LIKE quoteitm.part-no NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
  DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-inv FIELD selekt AS LOG LABEL "Selected"
                                 FIELD row-id AS ROWID.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 prmLoc   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
ASSIGN
    cocode = prmComp 
    locode = prmLoc .

FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.

FOR EACH tt-inv:
  DELETE tt-inv.
END.

FIND FIRST cust
    WHERE cust.company EQ prmComp 
      AND cust.cust-no EQ quotehd.cust-no 
    NO-LOCK NO-ERROR.

IF AVAIL cust THEN DO:
 
  RUN build-table.
END.

IF lv-num-rec GT 0 THEN DO:
  /*{src/adm/template/dialogmn.i}*/
END.


if prmAction =  "View" then do:
     
    FOR EACH tt-inv, FIRST ar-invl NO-LOCK WHERE ROWID(ar-invl) EQ tt-inv.row-id, FIRST itemfg NO-LOCK      
              WHERE itemfg.company EQ ar-invl.company  AND itemfg.i-no = ar-invl.i-no :
        CREATE ttQuotearinvpopup .
        ASSIGN
            ttQuotearinvpopup.arinv-ino      = ar-invl.i-no
            ttQuotearinvpopup.arinv-name     = itemfg.i-name
            ttQuotearinvpopup.arinv-qty      = ar-invl.inv-qty
            ttQuotearinvpopup.arinv-unitpr   = ar-invl.unit-pr
            ttQuotearinvpopup.arinv-qtyuom   = ar-invl.pr-qty-uom  .
        
   END.
END.  /*if prmAction <> "search" then do*/ 


if prmAction =  "AddItem" then do:
      MESSAGE "prmText" prmText quotehd.q-no  .

    FOR EACH tt-inv WHERE /*tt-inv.selekt*/ ,
      FIRST ar-invl WHERE ROWID(ar-invl) EQ tt-inv.row-id NO-LOCK,
      FIRST itemfg
      WHERE itemfg.company EQ ar-invl.company
        AND itemfg.i-no    EQ ar-invl.i-no AND LOOKUP(itemfg.i-no, prmText) <> 0
      NO-LOCK:
  
    FIND LAST quoteitm
        WHERE quoteitm.company EQ quotehd.company
          AND quoteitm.loc     EQ quotehd.loc
          AND quoteitm.q-no    EQ quotehd.q-no
        USE-INDEX q-line NO-LOCK NO-ERROR.
    li = (IF AVAIL quoteitm THEN quoteitm.line else 0) + 1.

    ASSIGN
     lv-part-no = ""
     lv-rowid   = ROWID(itemfg).
    RUN custom/getcpart.p (cocode, quotehd.cust-no,
                           INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).

    CREATE quoteitm.
    ASSIGN
     quoteitm.company    = quotehd.company
     quoteitm.loc        = quotehd.loc
     quoteitm.q-no       = quotehd.q-no
     quoteitm.line       = li
     quoteitm.upd-date   = TODAY
     quoteitm.upd-user   = prmUser
     quoteitm.part-no    = IF lv-part-no EQ "" THEN itemfg.part-no
                                               ELSE lv-part-no
     quoteitm.part-dscr1 = itemfg.i-name
     quoteitm.style      = itemfg.style
     quoteitm.price      = ar-invl.unit-pr
     quoteitm.qty        = ar-invl.inv-qty
     quoteitm.uom        = ar-invl.pr-qty-uom
     quoteitm.size       = STRING(itemfg.l-score[50]) + " x " +
                           STRING(itemfg.w-score[50]) + " x " +
                           STRING(itemfg.d-score[50])
     /*RCO400*/
     quoteitm.i-no = itemfg.i-no.

    CREATE quoteqty.
    ASSIGN
     quoteqty.company    = quoteitm.company
     quoteqty.loc        = quoteitm.loc
     quoteqty.q-no       = quoteitm.q-no
     quoteqty.line       = quoteitm.line
     quoteqty.qty        = quoteitm.qty
     quoteqty.price      = quoteitm.price
     quoteqty.uom        = quoteitm.uom
     quoteqty.quote-date = TODAY
     quoteqty.quote-user = prmUser .

    DEFINE VAR mcost AS DECIMAL NO-UNDO.
    DEFINE VAR labcost AS DECIMAL NO-UNDO.
    DEFINE VAR focost AS DECIMAL NO-UNDO.
    DEFINE VAR vocost AS DECIMAL NO-UNDO.

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-mat-cost,
                           OUTPUT mcost).

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-lab-cost,
                           OUTPUT labcost).

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-fix-cost,
                           OUTPUT focost).

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-var-cost,
                           OUTPUT vocost).
    ASSIGN
        quoteqty.mat-cost = mcost 
        quoteqty.lab-cost = labcost
        quoteqty.fo-cost  = focost
        quoteqty.vo-cost  = vocost .
  END.
   
END.  /*if prmAction <> "search" then do*/ 


PROCEDURE build-table :

  FOR EACH ar-invl
      WHERE ar-invl.company EQ cust.company
        AND ar-invl.posted  EQ YES
        AND ar-invl.cust-no EQ cust.cust-no
        AND ar-invl.i-no    GT ""
        AND ar-invl.inv-qty GT 0
      USE-INDEX inv-status NO-LOCK,

      FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK

      BREAK BY ar-invl.i-no
            BY ar-inv.inv-date DESC
            BY ar-invl.x-no    DESC:

    IF FIRST-OF(ar-invl.i-no) THEN DO:
      CREATE tt-inv.
      ASSIGN
       tt-inv.selekt = NO
       tt-inv.row-id = ROWID(ar-invl)
       lv-num-rec    = lv-num-rec + 1.
    END.
  END.

END PROCEDURE.
