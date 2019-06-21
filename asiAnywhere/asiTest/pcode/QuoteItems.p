
/*------------------------------------------------------------------------
    File        : QuoteItems.p   Copy of(est/b-qtitm.w)
    Purpose     : Quote Items

    Syntax      :

    Description : Return a Dataset of all Quote Items

    Author(s)   : 
    Created     : Mar 24 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttDispQuoteItems NO-UNDO 
    FIELD vPartNo       AS CHARACTER
    FIELD vIno          AS CHARACTER       
    FIELD vPartDscr1    AS CHARACTER 
    FIELD vPartDscr2    AS CHARACTER 
    FIELD vStyle        AS CHARACTER 
    FIELD vQty          AS INTEGER 
    FIELD vPrice        AS DECIMAL 
    FIELD vUom          AS CHARACTER 
    FIELD vSize         AS CHARACTER 
    FIELD vIdscr        AS CHARACTER 
    FIELD vIcoldscr     AS CHARACTER 
    FIELD vLine         AS INT

    .

DEFINE DATASET dsDispQuoteItems FOR ttDispQuoteItems.

DEFINE INPUT PARAMETER prmUser          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote         AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmPartNo        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItemNo        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPartDscr1     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPartDscr2     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQty           AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmPrice         AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmUom           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDimensions    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBoard         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmColor         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmLine          AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmStyle         AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDispQuoteItems.
DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

IF prmUser          = ? THEN ASSIGN prmUser         = "".
IF prmAction        = ? THEN ASSIGN prmAction       = "".
IF prmQuote         = ? THEN ASSIGN prmQuote        = 0.
IF prmPartNo        = ? THEN ASSIGN prmPartNo       = "".
IF prmItemNo        = ? THEN ASSIGN prmItemNo       = "".
IF prmPartDscr1     = ? THEN ASSIGN prmPartDscr1    = "".
IF prmPartDscr2     = ? THEN ASSIGN prmPartDscr2    = "".
IF prmQty           = ? THEN ASSIGN prmQty          = 0.
IF prmPrice         = ? THEN ASSIGN prmPrice        = 0.
IF prmUom           = ? THEN ASSIGN prmUom          = "".
IF prmDimensions    = ? THEN ASSIGN prmDimensions   = "".
IF prmBoard         = ? THEN ASSIGN prmBoard        = "".
IF prmColor         = ? THEN ASSIGN prmColor        = "".
IF prmStyle         = ? THEN ASSIGN prmStyle        = "".


DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.
DEF VAR ll-new-file AS LOG NO-UNDO.
DEF VAR lv-part-no LIKE quoteitm.part-no NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
  DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
  DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc AS CHAR NO-UNDO.

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
    locode = prmLoc
    g_company = prmComp 
    g_loc     = prmLoc .
 ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").


FUNCTION display-qty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
  IF AVAIL quoteitm THEN
     FIND FIRST quoteqty WHERE ASI.quoteqty.company = ASI.quoteitm.company
          AND ASI.quoteqty.loc = ASI.quoteitm.loc
          AND ASI.quoteqty.q-no = ASI.quoteitm.q-no
          AND ASI.quoteqty.line = ASI.quoteitm.line  USE-INDEX qt-qty NO-LOCK NO-ERROR.
  IF AVAIL quoteqty THEN RETURN int(quoteqty.qty).
  ELSE IF AVAIL quoteitm THEN RETURN int(quoteitm.qty).
  ELSE RETURN 0.   /* Function return value. */

END FUNCTION.

FUNCTION display-price RETURNS DECIMAL
  ( /* parameter-definitions */ ) :

  IF AVAIL quoteitm THEN
     FIND FIRST quoteqty WHERE ASI.quoteqty.company = ASI.quoteitm.company
          AND ASI.quoteqty.loc = ASI.quoteitm.loc
          AND ASI.quoteqty.q-no = ASI.quoteitm.q-no
          AND ASI.quoteqty.line = ASI.quoteitm.line  USE-INDEX qt-qty NO-LOCK NO-ERROR.
  

  IF AVAIL quoteqty THEN RETURN quoteqty.price.
  ELSE IF AVAIL quoteitm THEN RETURN quoteitm.price.
  ELSE RETURN 0.00.   /* Function return value. */

END FUNCTION.

FUNCTION display-uom RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

  IF AVAIL quoteitm THEN
     FIND FIRST quoteqty WHERE ASI.quoteqty.company = ASI.quoteitm.company
          AND ASI.quoteqty.loc = ASI.quoteitm.loc
          AND ASI.quoteqty.q-no = ASI.quoteitm.q-no
          AND ASI.quoteqty.line = ASI.quoteitm.line  USE-INDEX qt-qty NO-LOCK NO-ERROR.
  

  IF AVAIL quoteqty THEN RETURN quoteqty.uom.
  ELSE IF AVAIL quoteitm THEN RETURN quoteitm.uom.
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.


IF prmAction = "Select" THEN DO:   

    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
        
        FOR EACH quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no NO-LOCK:
            create ttDispQuoteItems .
            ASSIGN 
                ttDispQuoteItems.vPartNo        =  quoteitm.part-no     
                ttDispQuoteItems.vIno           =  quoteitm.i-no
                ttDispQuoteItems.vPartDscr1     =  quoteitm.part-dscr1
                ttDispQuoteItems.vPartDscr2     =  quoteitm.part-dscr2
                ttDispQuoteItems.vStyle         =  quoteitm.style
                ttDispQuoteItems.vQty           =  display-qty()
                ttDispQuoteItems.vPrice         =  display-price()
                ttDispQuoteItems.vUom           =  display-uom()
                ttDispQuoteItems.vSize          =  quoteitm.size
                ttDispQuoteItems.vIdscr         =  quoteitm.i-dscr
                ttDispQuoteItems.vIcoldscr      =  quoteitm.i-coldscr
                ttDispQuoteItems.vLine          =  quoteitm.LINE 

            .
        END.
    END.    
END.

/*********create item***********************************/

IF prmAction =  "AddValdate" THEN DO:
     
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
     IF ll-new-file THEN DO:
      ASSIGN
       lv-part-no = prmPartNo
       lv-rowid   = ?.
      RUN custom/getcpart.p (cocode, quotehd.cust-no,
                             INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
    END.

    IF NOT CAN-FIND(itemfg WHERE ROWID(itemfg) EQ lv-rowid) AND
       NOT CAN-FIND(FIRST itemfg
                    WHERE itemfg.company  EQ cocode
                      AND itemfg.part-no  EQ prmPartNo
                      AND itemfg.part-no  NE ""
                      AND (itemfg.cust-no EQ quotehd.cust-no OR
                           itemfg.i-code  EQ "S"))
    THEN DO:
      cError =  "Invalid Cust Part#, try help..." .
     RETURN .
    END.

  DEF VAR lv-uom LIKE uom.uom NO-UNDO.
  DEF VAR uom-list AS CHAR INIT "M,C,EA,CS" NO-UNDO.
  
    RUN sys/ref/uom-fg.p (NO ,OUTPUT uom-list).
    ASSIGN
     prmUom  = CAPS(prmUom )
     lv-uom = prmUom .

    IF LOOKUP(lv-uom,uom-list) LE 0 THEN DO:
      cError =  "UOM must be " + TRIM(uom-list) .
      RETURN .
    END.
 

END.

IF prmAction = "Add" THEN DO:
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:

        def var li-next-line as int no-undo.
        DEF BUFFER bQuoteItm FOR quoteitm.

       find last bQuoteItm use-index q-line where bQuoteItm.company = quotehd.company
           and bQuoteItm.loc = quotehd.loc
           and bQuoteItm.q-no = quotehd.q-no
           no-lock no-error.
       li-next-line = if avail bQuoteItm then bQuoteItm.line + 1 else 1.
       CREATE quoteitm .
       assign
           quoteitm.company     = quotehd.company
           quoteitm.loc         =  quotehd.loc
           quoteitm.q-no        = quotehd.q-no
           quoteitm.line        = li-next-line
           quoteitm.upd-date    = TODAY
           quoteitm.upd-user    = prmUser
           quoteitm.part-no     = prmPartNo
           quoteitm.i-no        = prmItemNo
           quoteitm.part-dscr1  = prmPartDscr1
           quoteitm.part-dscr2  = prmPartDscr2
           quoteitm.style       = prmStyle
           quoteitm.qty         = prmQty
           quoteitm.price       = prmPrice
           quoteitm.uom         = prmUom
           quoteitm.size        = prmDimensions
           quoteitm.i-dscr      = prmBoard
           quoteitm.i-coldscr   = prmColor  .
                         
           
       FIND FIRST quoteqty OF quoteitm NO-LOCK NO-ERROR.
       IF NOT AVAIL quoteqty THEN DO:
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
           
           RELEASE itemfg.

           IF ll-new-file THEN DO:
               ASSIGN
                   lv-part-no = quoteitm.part-no
                   lv-rowid   = ?.
               RUN custom/getcpart.p (cocode, quotehd.cust-no,
                                      INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
               FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.
          END.

          IF NOT AVAIL itemfg THEN
              FIND FIRST itemfg
              WHERE itemfg.company  EQ quoteitm.company
              AND itemfg.part-no  EQ quoteitm.part-no
              AND itemfg.part-no  NE ""
              AND (itemfg.cust-no EQ quotehd.cust-no OR
                   itemfg.i-code  EQ "S")
              NO-LOCK NO-ERROR.
          
          DEFINE VAR mcost AS DECIMAL NO-UNDO.
          DEFINE VAR labcost AS DECIMAL NO-UNDO.
          DEFINE VAR focost AS DECIMAL NO-UNDO.
          DEFINE VAR vocost AS DECIMAL NO-UNDO.

          IF AVAIL itemfg THEN DO:
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

      END. 
        ASSIGN
            prmLine = quoteitm.line
            prmAction = "View" .

    END.
END.

/******************update****************************************/
.
IF prmAction = "Update" THEN DO:
     
    FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = prmQuote AND quoteitm.LINE = prmLine EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL quoteitm THEN DO:
        ASSIGN
           quoteitm.part-dscr1  = prmPartDscr1
           quoteitm.part-dscr2  = prmPartDscr2
           quoteitm.size        = prmDimensions
           quoteitm.i-dscr      = prmBoard
           quoteitm.i-coldscr   = prmColor  .
    END.
    ASSIGN
        prmAction = "View" .

END.
/******************* delete*********************/
IF prmAction = "Delete" THEN DO:
    FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = prmQuote AND quoteitm.LINE = prmLine EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL quoteitm THEN DO:
        DELETE quoteitm .
    END.

END.


IF prmAction = "View" THEN DO:   

    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
        
        FOR EACH quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no AND quoteitm.LINE = prmLine NO-LOCK:
            create ttDispQuoteItems .
            ASSIGN 
                ttDispQuoteItems.vPartNo        =  quoteitm.part-no     
                ttDispQuoteItems.vIno           =  quoteitm.i-no
                ttDispQuoteItems.vPartDscr1     =  quoteitm.part-dscr1
                ttDispQuoteItems.vPartDscr2     =  quoteitm.part-dscr2
                ttDispQuoteItems.vStyle         =  quoteitm.style
                ttDispQuoteItems.vQty           =  display-qty()   
                ttDispQuoteItems.vPrice         =  display-price() 
                ttDispQuoteItems.vUom           =  display-uom()   
                ttDispQuoteItems.vSize          =  quoteitm.size
                ttDispQuoteItems.vIdscr         =  quoteitm.i-dscr
                ttDispQuoteItems.vIcoldscr      =  quoteitm.i-coldscr
                ttDispQuoteItems.vLine          =  quoteitm.LINE
            .
        END.
    END.    
END.
