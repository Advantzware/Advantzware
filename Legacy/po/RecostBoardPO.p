/*------------------------------------------------------------------------
    File        : po\RecostBoardPO.p
    Purpose     : Given a purchase order header, sum each line qty for like
                  vendor, board, size and scoring and set each po line cost
                  with vendor cost for aggregate qty
                                                    
    Syntax      :

    Description :

    Author(s)   : BV
    Created     : 10/08/2013
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriPO AS ROWID.
DEFINE INPUT PARAMETER iplMessage AS LOG NO-UNDO. /* YES will prompt a summary message*/
                                                
{po/RecostBoardPo.i "NEW"}

DEF TEMP-TABLE tt-e-item NO-UNDO
    FIELD std-uom AS CHAR.

DEF TEMP-TABLE tt-e-item-vend NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20
    FIELD rec_key AS CHAR.

DEF TEMP-TABLE tt-e-item-vend-2 NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20
    FIELD rec_key AS CHAR.

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
DEFINE VARIABLE glUpdate AS LOG NO-UNDO.
DEF VARIABLE k_frac AS DEC INIT "6.25" NO-UNDO.
ASSIGN cocode = g_company
       locode = g_loc.
{sys/inc/f16to32.i}

/* ***************************  Main Block  *************************** */

glUpdate = NO.
RUN ProcessPOLines. /*Build internal temp-tables*/
FIND FIRST ttPOGroups WHERE ttPOGroups.Multi EQ YES NO-LOCK NO-ERROR.
IF AVAIL ttPOGroups THEN DO: /*more than one PO line with matching item, vendor, & size*/
    RUN GetNewCosts.  /*Check vendor matrix for costs with aggregate qty*/
    RUN UpdatePOLines(YES).  /*See if cost is better than current po-ordls, YES = Compare only*/
    IF glUpdate THEN DO: /*If Better costs were found*/
        IF iplMessage THEN RUN ShowCostUpdates. /*Present updates to user and get confirmation*/
        RUN UpdatePOLines(NO).  /*Update the PO line costs with better costs*/
    END.    
    ELSE
        IF iplMessage THEN 
            MESSAGE "No board cost or setup improvements available after grouping purchase order items."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.  /*Avail POGroups*/
ELSE
    IF iplMessage THEN 
        MESSAGE "No purchase order items can be grouped for new costs."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.


/* **********************  Internal Procedures  *********************** */

PROCEDURE GetNewCosts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/



DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE dCost AS DECIMAL NO-UNDO.
DEFINE VARIABLE dQty AS DECIMAL NO-UNDO.
DEFINE VARIABLE dDimCharge AS DECIMAL NO-UNDO.
DEFINE VARIABLE dSetup AS DECIMAL NO-UNDO.
DEFINE VARIABLE dPBQty AS DECIMAL NO-UNDO.
DEFINE VARIABLE dPBStp AS DECIMAL NO-UNDO.
DEFINE VARIABLE dPBCst AS DECIMAL NO-UNDO.
DEFINE VARIABLE dPBCns AS DECIMAL NO-UNDO.

FOR EACH ttPOGroups 
    WHERE ttPOGroups.Multi:
    FIND FIRST e-item NO-LOCK
           WHERE e-item.company EQ cocode
             AND e-item.i-no    EQ ttPOGroups.INo
           NO-ERROR.
   IF AVAIL e-item THEN DO:
      CREATE tt-e-item.
      ASSIGN tt-e-item.std-uom = e-item.std-uom.
  
      FIND FIRST e-item-vend NO-LOCK
          WHERE e-item-vend.company EQ e-item.company
            AND e-item-vend.i-no    EQ e-item.i-no
            AND e-item-vend.vend-no EQ ttPOGroups.VendNo
          NO-ERROR.
  
      IF AVAIL e-item-vend THEN DO:

         CREATE tt-e-item-vend.
         tt-e-item-vend.rec_key = e-item-vend.rec_key.
         DO i = 1 TO 10:
            ASSIGN
               tt-e-item-vend.run-qty[i] = e-item-vend.run-qty[i]
               tt-e-item-vend.run-cost[i] = e-item-vend.run-cost[i]
               tt-e-item-vend.setups[i] = e-item-vend.setups[i].
         END.         
     
         IF AVAIL e-item-vend THEN DO:            
                DO i = 1 TO 10:
                    ASSIGN
                        tt-e-item-vend.run-qty[i + 10] = e-item-vend.runQtyXtra[i]
                        tt-e-item-vend.run-cost[i + 10] = e-item-vend.runCostXtra[i]
                        tt-e-item-vend.setups[i + 10] = e-item-vend.setupsXtra[i].
                END. /*Do i 1 to 10*/
            END. /*avail bf-reftableQty*/
        END. /* avail e-item-vend*/
    END.
    IF AVAIL tt-e-item-vend THEN DO:
        IF tt-e-item.std-uom NE ttPOGroups.TotalQtyUom THEN 
            /*convert aggregate qty to matrix qty*/
            RUN sys/ref/convquom.p(INPUT ttPOGroups.TotalQtyUom,
                                   INPUT tt-e-item.std-uom,
                                   INPUT ttPOGroups.BasisWeight,
                                   INPUT ttPOGroups.Len,
                                   INPUT ttPOGroups.Wid,
                                   INPUT 0,
                                   INPUT ttPOGroups.TotalQty,
                                   OUTPUT dQty).
                                     
        ASSIGN
            dSetup    = 0
            dCost   = 0.
        RUN est/dim-charge.p (INPUT tt-e-item-vend.rec_key,
                              INPUT ttPOGroups.Wid,
                              INPUT ttPOGroups.Len,
                              INPUT-OUTPUT dDimCharge).

        DO i = 1 TO EXTENT(tt-e-item-vend.run-qty):
            IF tt-e-item-vend.run-qty[i] LT dQty THEN NEXT.
            ASSIGN
                dCost = tt-e-item-vend.run-cost[i] + dDimCharge
/*                 dCost   = (tt-e-item-vend.run-cost[i] + dDimCharge) * dQty */
                dSetup  = tt-e-item-vend.setups[i].
            LEAVE.
        END.
        ASSIGN 
            ttPOGroups.NewCost = dCost + ttPOGroups.AdderCost
            ttPoGroups.NewCostUOM = tt-e-item.std-uom
            ttPOGroups.NewSetup = dSetup
            .
    END. /*avail tt-e-item-vend*/
END. /*each ttPOGroups*/

END PROCEDURE.

PROCEDURE GetPoLineScores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipriPOOrdl AS RECID.
DEFINE OUTPUT PARAMETER opcScores AS CHAR.
DEFINE BUFFER b-ref1 FOR reftable.
DEFINE BUFFER b-ref2 FOR reftable.
DEFINE BUFFER bf-po-ordl FOR po-ordl.

DEFINE VARIABLE cScore AS CHAR NO-UNDO.
DEFINE VARIABLE i AS INT NO-UNDO.
DEFINE VARIABLE lSpace AS LOG NO-UNDO.

FIND bf-po-ordl WHERE RECID(bf-po-ordl) EQ ipriPOOrdl NO-LOCK.
{po/po-ordls.i "bf-"} /*finds scores if they exist for the po-ordl*/
IF NOT AVAIL b-ref1 AND NOT AVAIL b-ref2 THEN DO:
    RUN po/po-ordls.p (INPUT ipriPOOrdl).  /*build scores reftable*/
    {po/po-ordls.i "bf-"} /*finds scores again*/
END.
IF AVAIL b-ref1 OR AVAIL b-ref2 THEN DO:
    cScore = "".
    IF AVAIL b-ref1 THEN
        DO i = 1 TO 12:
            IF b-ref1.val[i] NE 0 THEN
                cScore = cScore + TRIM(STRING(b-ref1.val[i],">>>.99")) + " ".
        END.
    IF AVAIL b-ref2 THEN
        DO i = 1 TO 8:
            IF b-ref2.val[i] NE 0 THEN
                cScore = cScore + TRIM(STRING(b-ref2.val[i],">>>.99")) + " ".
        END.
    IF cScore NE "" THEN DO:
        opcScores = "".
        DO i = 1 TO LENGTH(cScore):
            IF SUBSTR(cScore,i,1) NE " " THEN
                ASSIGN
                    opcScores = opcScores + SUBSTRING(cScore,i,1)
                    lSpace = YES.
            ELSE IF lSpace THEN
                ASSIGN
                    opcScores = opcScores + "  "
                    lSpace   = NO.
        END. /*i to length(cScore)*/
    END. /*cScore ne ""*/
END. /*avail b-ref1 or b-ref2*/

END PROCEDURE.

PROCEDURE ProcessPOLines :
/*------------------------------------------------------------------------------
  Purpose: Build Temp tables for processing    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-po-ord FOR po-ord.
DEFINE BUFFER bf-po-ordl FOR po-ordl.
DEFINE BUFFER bf-item FOR ITEM.

DEFINE VARIABLE dQty AS DECIMAL NO-UNDO.
DEFINE VARIABLE cScores AS CHAR NO-UNDO.
DEFINE VARIABLE cAdders AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dAdderCost AS DECIMAL     NO-UNDO.

FOR EACH ttPOGroups:
    DELETE ttPOGroups.
END.
FOR EACH ttPOLineXref:
    DELETE ttPOLIneXref.
END.
FIND bf-po-ord WHERE ROWID(bf-po-ord) EQ ipriPO NO-LOCK NO-ERROR.
IF AVAIL bf-po-ord THEN DO:
    FOR EACH bf-po-ordl 
        WHERE bf-po-ordl.company EQ cocode
          AND bf-po-ordl.po-no EQ bf-po-ord.po-no
          AND bf-po-ordl.item-type
        NO-LOCK,
    FIRST bf-item 
        WHERE bf-item.company EQ cocode
          AND bf-item.i-no EQ bf-po-ordl.i-no
          AND bf-item.mat-type EQ "B"
        NO-LOCK:

        RUN GetPoLineScores(INPUT RECID(bf-po-ordl),
                            OUTPUT cScores).
        RUN GetAdders(INPUT ROWID(bf-po-ordl),
                      OUTPUT cAdders,
                      OUTPUT dAdderCost).

        FIND FIRST ttPOGroups 
            WHERE ttPOGroups.INo EQ bf-po-ordl.i-no
              AND ttPOGroups.VendNo EQ bf-po-ordl.vend-no
              AND ttPOGroups.Len EQ bf-po-ordl.s-len
              AND ttPOGroups.Wid EQ bf-po-ordl.s-wid
              AND ttPOGroups.Scores EQ cScores
              AND ttPOGroups.Adders EQ cAdders NO-ERROR.
        IF AVAIL ttPOGroups THEN DO:
            dQty = bf-po-ordl.ord-qty.
            IF bf-po-ordl.pr-qty-uom NE ttPOGroups.TotalQtyUOM THEN DO:
                /*convert uom so that it can be summed with existing qty*/
                RUN sys/ref/convquom.p (INPUT bf-po-ordl.pr-qty-uom,
                                INPUT ttPOGroups.TotalQtyUOM,
                                INPUT bf-item.basis-w,
                                INPUT bf-po-ordl.s-len,
                                INPUT bf-po-ordl.s-wid,
                                INPUT 0,
                                INPUT bf-po-ordl.ord-qty,
                                OUTPUT dQty).
            END.
            ASSIGN 
                ttPOGroups.Multi = YES
                ttPoGroups.TotalQty = ttPOGroups.TotalQty + dQty
                ttPOGroups.LineCount = ttPOGroups.LineCount + 1.
            
        END.
        ELSE DO:
            CREATE ttPOGroups.
            ASSIGN
                ttPOGroups.INo = bf-po-ordl.i-no
                ttPOGroups.VendNo = bf-po-ordl.vend-no
                ttPOGroups.Len = bf-po-ordl.s-len
                ttPOGroups.Wid = bf-po-ordl.s-wid
                ttPOGroups.Scores = cScores
                ttPOGroups.Adders = cAdders
                ttPOGroups.AdderCost = dAdderCost
                ttPOGroups.TotalQty = bf-po-ordl.ord-qty
                ttPOGroups.TotalQtyUOM = bf-po-ordl.pr-qty-uom
                ttPoGroups.Multi = NO
                ttPOGroups.BasisWeight = bf-item.basis-w
                ttPOGroups.LineCount = 1.
        END.
        CREATE ttPOLineXRef.
        ASSIGN 
            POGroupRowId = ROWID(ttPOGroups)
            PoOrdlRowId  = ROWID(bf-po-ordl).
    END.
END.

END PROCEDURE.

PROCEDURE ShowCostUpdates :
/*------------------------------------------------------------------------------
  Purpose: Present Results to the user
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-po-ordl FOR po-ordl.
DEFINE VARIABLE lUpdateCost AS LOG NO-UNDO.

FOR EACH ttPOGroups
    WHERE ttPOGroups.Multi
      AND ttPOGroups.UpdateCost:

    MESSAGE "A reduced cost of " ttPOGroups.NewCost " " ttPOGroups.NewCostUOM " was found." SKIP
        "Total Qty: " ttPOGroups.TotalQty " " ttPOGroups.TotalQtyUom SKIP
        "Item #: " ttPOGroups.INo SKIP
        "Vendor: " ttPOGroups.VendNo SKIP
        "Width: " ttPOGroups.Wid SKIP
        "Length: " ttPOGroups.Len SKIP
        "Scores: " ttPOGroups.Scores SKIP 
        "Adders: " ttPOGroups.Adders SKIP(1)
        "Do you want to apply this cost to the order lines?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lUpdateCost.
    ttPOGroups.UpdateCost = lUpdateCost.

END. /*each ttPOGroups*/
END PROCEDURE.

PROCEDURE UpdatePOLines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iplCompareOnly AS LOG NO-UNDO.

DEFINE BUFFER bf-po-ordl FOR po-ordl.
DEFINE VARIABLE xPOGroupCost LIKE po-ordl.cost NO-UNDO.
DEFINE VARIABLE xNewPOLineSetup LIKE po-ordl.setup NO-UNDO.
DEFINE VARIABLE cAdders AS CHARACTER   NO-UNDO.

FOR EACH ttPOGroups
    WHERE ttPOGroups.Multi
      AND (iplCompareOnly OR ttPOGroups.UpdateCost),
EACH ttPOLineXref
    WHERE ttPOLineXRef.POGroupRowId EQ ROWID(ttPOGroups) NO-LOCK:
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ ttPOLineXref.POOrdlRowId NO-LOCK NO-ERROR.
    IF AVAIL bf-po-ordl THEN DO:
        ASSIGN 
            xPOGroupCost = ttPOGroups.NewCost
            xNewPOLineSetup = ttPOGroups.NewSetup / ttPOGroups.LineCount.
        IF bf-po-ordl.pr-uom NE ttPOGroups.NewCostUom THEN
            RUN sys/ref/convcuom.p(INPUT ttPOGroups.NewCostUom,
                                   INPUT bf-po-ordl.pr-uom,
                                   INPUT ttPOGroups.BasisWeight,
                                   INPUT ttPOGroups.Len,
                                   INPUT ttPOGroups.Wid,
                                   INPUT 0,
                                   INPUT xPOGroupCost, 
                                   OUTPUT xPOGroupCost).
        
        IF xPoGroupCost LT bf-po-ordl.cost OR xNewPOLineSetup LT bf-po-ordl.setup THEN DO:
            ASSIGN 
                glUpdate = YES 
                ttPOGroups.UpdateCost = YES.
            IF NOT iplCompareOnly THEN DO:  /*Update POLines*/
                FIND CURRENT bf-po-ordl EXCLUSIVE-LOCK.
                IF  bf-po-ordl.cost > xPOGroupCost THEN  bf-po-ordl.cost = xPOGroupCost.
                IF bf-po-ordl.setup > xNewPOLineSetup THEN bf-po-ordl.setup = xNewPOLineSetup.
                FIND CURRENT bf-po-ordl NO-LOCK.
            END.
        END.
    END. /*avail bf-po-ordl*/
END. /*Each tt*/
END PROCEDURE.

PROCEDURE GetAdders :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipriPOOrdl AS ROWID.
DEFINE OUTPUT PARAMETER opcAdders AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.

DEFINE BUFFER bf-po-ordl FOR po-ordl.
DEFINE BUFFER bf-po-ordl-add FOR po-ordl-add.

FIND bf-po-ordl 
    WHERE ROWID(bf-po-ordl) EQ ipriPOOrdl
    NO-LOCK NO-ERROR.
IF AVAIL bf-po-ordl THEN DO:
    FOR EACH bf-po-ordl-add
        WHERE bf-po-ordl-add.company EQ bf-po-ordl.company
          AND bf-po-ordl-add.po-no   EQ bf-po-ordl.po-no
          AND bf-po-ordl-add.LINE    EQ bf-po-ordl.LINE
        NO-LOCK
        BY bf-po-ordl-add.adder-i-no:
        ASSIGN 
            opcAdders = opcAdders + bf-po-ordl-add.adder-i-no + ","
            opdCost = opdCost + bf-po-ordl-add.cost.            
    END.
END. /*avail bf-po-ordl*/

END PROCEDURE.

