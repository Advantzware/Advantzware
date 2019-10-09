&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/cre-rell.p
    Purpose     :

    Syntax      :

    Description :order entry - Create actual releases from planned release line 

    Author(s)   : 01/98 JLF
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER in-recid AS RECID.


{sys/inc/VAR.i SHARED}

{oe/d-selbin.i NEW}
{sys/inc/oereordr.i} 

 
DEFINE SHARED VARIABLE out-recid         AS RECID     NO-UNDO.
DEFINE SHARED VARIABLE relh-recid        AS RECID     NO-UNDO.

DEFINE VARIABLE cWhse          LIKE oe-rell.loc NO-UNDO.
DEFINE VARIABLE lcLocBin       LIKE oe-rell.loc-bin NO-UNDO.
DEFINE VARIABLE lFgFile        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lNone          AS LOGICAL   INIT YES NO-UNDO.
DEFINE VARIABLE cAllOrOne      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRowidList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE li             AS INTEGER   NO-UNDO.
DEFINE VARIABLE ll             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBinTag        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cJobNo         AS CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE VARIABLE cSelectedValue AS character NO-UNDO. /*all,one,notag*/.
DEFINE VARIABLE cSCode            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lrOeRell          AS ROWID     NO-UNDO.
DEFINE VARIABLE iNxtRelNo         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcBolWhse         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rtn-char        AS CHARACTER NO-UNDO.
DEFINE VARIABLE RelSkipRecalc-log AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-rec-found       AS LOGICAL   NO-UNDO.
     
DEFINE BUFFER b-reftable FOR reftable.
DEFINE BUFFER bf-rell    FOR oe-rell .

DO:
    {sys/inc/addrelse.i}
END.

DO TRANSACTION:
    {sys/inc/relmerge.i}

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.NAME    EQ "BOLWHSE"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.NAME    = "BOLWHSE"
            sys-ctrl.descrip = "Default Warehouse for Adding Release/BOL"
            sys-ctrl.log-fld = NO.
        MESSAGE "System control record NOT found. " sys-ctrl.descrip
            UPDATE sys-ctrl.char-fld.
    END.
    IF AVAILABLE sys-ctrl THEN lcBolWhse = sys-ctrl.char-fld.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.NAME    EQ "BOLPRINT"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.NAME    = "BOLPRINT"
            sys-ctrl.descrip = "Print Bill of Lading Headers on Plain Paper"
            sys-ctrl.log-fld = NO.
        MESSAGE "System control record NOT found. " sys-ctrl.descrip
            UPDATE sys-ctrl.char-fld.
    END.
    IF AVAILABLE sys-ctrl THEN lcLocBin = sys-ctrl.char-fld.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.NAME    EQ "AUTOPOST"
        NO-LOCK NO-ERROR.
    lFgFile = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "FGFILE".

END.

RUN sys/ref/nk1look.p (INPUT cocode, "RelSkipRecalc", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
RelSkipRecalc-log = LOGICAL(v-rtn-char) NO-ERROR.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.71
         WIDTH              = 55.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DO:

    /** If Shipping From Bill Of Lading Then Set Ship Code = B
        Or If Shipping From Finiished Goods Then Set Ship Code = I **/
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

    FIND oe-rel NO-LOCK 
        WHERE RECID(oe-rel) EQ in-recid.
    FIND oe-relh NO-LOCK 
        WHERE RECID(oe-relh) EQ out-recid.
    IF relh-recid NE ? THEN
        FIND oe-relh  NO-LOCK
            WHERE RECID(oe-relh) EQ relh-recid.

    RUN get-next-r-no.

    RUN update-oe-rel.

    FIND FIRST oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ cocode
        AND oe-ordl.ord-no  EQ oe-rel.ord-no
        AND oe-ordl.i-no    EQ oe-rel.i-no
        AND oe-ordl.line    EQ oe-rel.line
       NO-ERROR
       .

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ oe-rel.i-no
        NO-ERROR.
    
    lBinTag = AVAILABLE oe-ordl             AND
        (addrelse-cha EQ "Bin/Tag" OR addrelse-cha EQ "No Tags") AND
        CAN-FIND(FIRST fg-bin
                 WHERE fg-bin.company EQ cocode
                   AND fg-bin.i-no    EQ oe-ordl.i-no
                   AND fg-bin.qty     GT 0).
    
    IF lBinTag THEN 
        RUN rel-bin-process.

    IF AVAILABLE itemfg             
        AND oe-rel.tot-qty GT 0          
        AND NOT lBinTag           
        AND cWhse NE "SHIPTO"       
        AND (itemfg.i-code EQ "S" OR oe-ordl.job-no EQ "" OR cWhse EQ "FIFO") THEN 
        RUN oe/fifoloop.p (ROWID(oe-rel), cSelectedValue EQ "notag", OUTPUT lNone).

    IF lNone THEN 
        RUN create-oe-rell (OUTPUT lrOeRell).

    FIND oe-rell WHERE ROWID(oe-rell) EQ lrOeRell NO-LOCK NO-ERROR.

    /* Fill in correct bin/loc */
    IF AVAILABLE oe-rell  THEN
        RUN find-bin-loc.

    /* Set values for invoice only */
    IF avail(oe-rell) AND oe-rell.s-code = "I" THEN
        RUN process-type-i (INPUT ROWID(oe-rell)).

    IF lBinTag THEN 
    DO:
        RUN bin-tag-process.
    END. /* if lBinTag */
    ELSE 
    DO TRANSACTION:
        FIND CURRENT oe-rel EXCLUSIVE.
        oe-rel.qty = oe-rel.tot-qty.   
        FIND CURRENT oe-rel NO-LOCK.
    END.
    RUN update-rel-stat (INPUT ROWID(oe-rel)).

    IF AVAILABLE itemfg AND RelSkipRecalc-log THEN 
    DO TRANSACTION:
        /* Corrects data integrity issue until auditing can identify problem */
        /* Run if recalc is skipped in oe-rell trigger */
        FIND CURRENT itemfg EXCLUSIVE-LOCK.
        RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT itemfg.q-alloc,
            OUTPUT itemfg.q-back).
        itemfg.q-avail = itemfg.q-onh +
            (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE itemfg.q-ono) -
            itemfg.q-alloc.
        FIND CURRENT itemfg NO-LOCK NO-ERROR.      
    END.

END.
/* end ---------------------------------- copr. 1998  advanced software, inc. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-bin-tag-process) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bin-tag-process Procedure 
PROCEDURE bin-tag-process :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF cSelectedValue NE "Notag" THEN 
    DO:
        IF cSelectedValue NE "Cancel" THEN
            RUN oe/d-selbin.w (4, ROWID(oe-rell), cAllOrOne, oe-rell.i-no,
                OUTPUT cRowidList).
        IF cRowidList = "" THEN 
        DO:
            FIND CURRENT oe-rell EXCLUSIVE-LOCK NO-ERROR.
            DELETE oe-rell.
        END.
    END.
    ELSE 
    DO:
        FIND CURRENT oe-rel EXCLUSIVE.
        oe-rel.qty = oe-rel.tot-qty.
    END.


    FIND CURRENT oe-rell NO-LOCK NO-ERROR.
    IF NOT AVAILABLE oe-rell THEN
        FIND FIRST oe-rell NO-LOCK
            WHERE oe-rell.company EQ oe-relh.company
              AND oe-rell.r-no    EQ oe-relh.r-no
            USE-INDEX r-no NO-ERROR.
    out-recid = IF AVAILABLE oe-rell THEN RECID(oe-rell) ELSE ?.

    IF out-recid EQ ? THEN 
    DO:
        FIND CURRENT oe-relh NO-ERROR.
        IF AVAILABLE oe-relh THEN DELETE oe-relh.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-fg-bin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-fg-bin Procedure 
PROCEDURE create-fg-bin :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprOeRell AS ROWID       NO-UNDO.
    DEFINE BUFFER bfOeRell FOR oe-rell.
    DEFINE VARIABLE xLoc LIKE fg-bin.loc NO-UNDO.
    DEFINE VARIABLE xBin LIKE fg-bin.loc-bin NO-UNDO.
    FIND bfOeRell NO-LOCK WHERE ROWID(bfOeRell) EQ iprOeRell NO-ERROR.

    IF NOT AVAILABLE bfOeRell THEN 
    DO:
        MESSAGE "Internal error - no actual release available (create-fg-bin)"
            VIEW-AS ALERT-BOX.
        RETURN.
    END.
    ASSIGN 
        xBin = bfOeRell.loc-bin
        xLoc = bfOeRell.loc
        .

    CREATE fg-bin.
    ASSIGN
        fg-bin.company = cocode
        fg-bin.i-no    = bfOeRell.i-no
        fg-bin.job-no  = bfOeRell.job-no
        fg-bin.job-no2 = bfOeRell.job-no2
        fg-bin.loc     = xLoc
        fg-bin.loc-bin = xBin
        fg-bin.tag     = bfOeRell.tag
        fg-bin.cust-no = bfOeRell.cust-no
        . 
    RELEASE fg-bin.

    /*Create a bin so that is shows up in IF4 -FG Bin (blank i-no)*/
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ cocode 
          AND fg-bin.loc EQ xLoc
          AND fg-bin.loc-bin EQ xBin
          AND fg-bin.i-no = ""
        NO-ERROR.
    IF NOT AVAILABLE fg-bin THEN 
    DO:
        CREATE fg-bin.
        ASSIGN 
            fg-bin.company = cocode
            fg-bin.i-no    = ""
            fg-bin.loc     = xLoc
            fg-bin.loc-bin = xBin
            .

    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-oe-rell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-oe-rell Procedure 
PROCEDURE create-oe-rell :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opr-oerell AS ROWID NO-UNDO.

  
    CREATE oe-rell.
    ASSIGN
        out-recid          = RECID(oe-rell)
        oe-rell.company    = oe-rel.company
        oe-rell.r-no       = oe-relh.r-no
        oe-rell.rel-no     = iNxtRelNo
        oe-rell.loc        = IF lcBolWhse NE "ShipFromWhse" THEN locode ELSE oe-rel.spare-char-1
        oe-rell.ord-no     = oe-rel.ord-no
        oe-rell.qty        = oe-rel.tot-qty
        oe-rell.i-no       = oe-rel.i-no
        oe-rell.job-no     = oe-ordl.job-no
        oe-rell.job-no2    = oe-ordl.job-no2
        oe-rell.po-no      = oe-rel.po-no
        oe-rell.line       = oe-rel.line
        oe-rell.lot-no     = oe-rel.lot-no
        oe-rell.frt-pay    = oe-rel.frt-pay
        oe-rell.fob-code   = oe-rel.fob-code
        oe-rell.sell-price = oe-rel.sell-price
        oe-rell.zeroPrice  = oe-rel.zeroPrice
        oe-rell.printed    = NO
        oe-rell.posted     = NO
        oe-rell.deleted    = NO
        /** Set link to the planned releases **/
        oe-rell.link-no    = oe-rel.r-no
        oe-rell.s-code     = IF oe-rel.s-code <> "" THEN oe-rel.s-code ELSE
                     IF oe-ordl.is-a-component THEN "S" ELSE
                     IF AVAILABLE oe-ctrl AND oe-ctrl.ship-from THEN "B" ELSE "I".
    opr-oerell = ROWID(oe-rell).

    IF oe-rell.s-code EQ "I" THEN 
        oe-rell.partial = oe-ordl.partial.
   

    IF oe-rell.qty-case EQ 0 THEN
        oe-rell.qty-case = IF AVAILABLE itemfg AND itemfg.case-count GT 0
            THEN itemfg.case-count
            ELSE
            IF oe-ordl.cas-cnt GT 0 THEN oe-ordl.cas-cnt
            ELSE 1.

    ASSIGN
        oe-rell.cases   = TRUNC((oe-rell.qty - oe-rell.partial) /
                            oe-rell.qty-case,0)
        oe-rell.partial = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case)
        . 

    IF oe-rell.qty LT 0 OR cSelectedValue = "NoTag" THEN oe-rell.tag = "".  
    RUN oe/rel-stat-upd.p (ROWID(oe-rell)).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-find-bin-loc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-bin-loc Procedure 
PROCEDURE find-bin-loc :
    /*------------------------------------------------------------------------------
      Purpose: Assign correct location and bin to oe-rell    
      Parameters:  <none>
      Notes: 
      
      Logic For Determining OT1 (Actual Release) WHSE, BIN
    
    1)	If the N-K BOLWHSE character value = "Shipto" then the WHSE and BIN
        are pulled from the customer shipto  (AF1, ShipTo tab) specified 
        on the OU1 Release.
    
    2)	If BOLWHSE character value = "ShipFromWhse", a default WHSE location 
        that matches the ShipFrom set in the OU1 Release will be defined.  
        The BIN location is then determined by finding a bin that matches 
        on the following:
        a.	Job, Item, Exact Qty ,  ShipFrom Whse
        b.	Job, Item, Any Qty > 0, ShipFrom Whse
        c.	Job, Item, ShipFrom Whse (any Qty)
        d.	Item, Order, Any Qty > 0, ShipFrom Whse
        e.	Item, Order, ShipFrom Whse (any Qty)
        f.	Item, Any Qty > 0, ShipFrom Whse (any Job, Order)
        g.	Item, ShipFrom Whse
        If the logic above doesn't locate a bin, then the Bin is set to the 
        character value of N-K BOLPRINT.  If this is not a valid bin for the 
        ShipFrom Whse, it will be created.
    
    3)	If BOLWHSE is not "ShipTo" or "ShipFromWhse" then the default WHSE 
        is set based on the global default warehouse for the company.  
        This can be overridden if a bin exists with the following hierarchical 
        matching criteria:
        a.	Job, Item,  Exact Qty,  Default Whse
        b.	Job, Item, Exact Qty  (any Whse)
        c.	Job, Item, Any Qty > 0, Default Whse
        d.	Job, Item, Any Qty>0  (any Whse)
        e.	Job, Item, Default Whse (any Qty)
        f.	Job, Item (any Qty, any Whse)
        g.	Item, Order, Any Qty > 0, Default Whse
        h.	Item, Order, Default Whse (any Qty)
        i.	Item, Order, Any Qty > 0 (any Whse)
        j.	Item, Order
        k.	Item, Any Qty > 0,Default Whse (any Job, Order)
        l.	Item, Default Whse
        m.	Item, Any Qty > 0
        n.	Item
        If the logic above doesn't locate a bin, then the default Whse and Bin 
        from the FG item file is used.  If those values are blank, the Whse and 
        Bin for the first shipto of the Internal Customer (Customer X) is used.
          
    ------------------------------------------------------------------------------*/

    FIND CURRENT oe-rell EXCLUSIVE-LOCK.
    IF oe-rel.spare-char-1 GT "" 
       AND lcBolWhse EQ "ShipFromWhse" THEN
        oe-rell.loc = oe-rel.spare-char-1.
  
    /* lcBolWhse is an NK1 flag. oe-rel.spare-char-1 is a ship-from */
    /* chosen by the user, so should try to find a bin for it       */
    IF lcBolWhse EQ "SHIPTO" THEN 
    DO:
        FIND FIRST shipto NO-LOCK
            WHERE shipto.company EQ cocode
              AND shipto.cust-no EQ oe-rel.cust-no
              AND shipto.ship-no EQ oe-rel.ship-no
            USE-INDEX ship-no NO-ERROR.
        IF AVAILABLE shipto THEN 
            ASSIGN
                oe-rell.loc     = shipto.loc
                oe-rell.loc-bin = shipto.loc-bin
                .
    END.
  
    ELSE 
    DO:
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ cocode
              AND fg-bin.job-no  EQ oe-ordl.job-no
              AND fg-bin.job-no2 EQ oe-ordl.job-no2
              AND fg-bin.i-no    EQ oe-rell.i-no
              AND fg-bin.qty     GE oe-rell.qty
              AND fg-bin.loc     EQ oe-rell.loc
            USE-INDEX job NO-ERROR.
    
        IF NOT AVAILABLE fg-bin AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.job-no  EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.qty     GE oe-rell.qty
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE fg-bin THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.job-no  EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.loc     EQ oe-rell.loc
                  AND fg-bin.qty     GT 0
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE fg-bin AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.job-no  EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.qty     GT 0
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE fg-bin THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.job-no  EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.loc     EQ oe-rell.loc
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE fg-bin AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.job-no  EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no    EQ oe-rell.i-no
                USE-INDEX job NO-ERROR.
    
        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.ord-no  EQ oe-rel.ord-no
                  AND fg-bin.loc     EQ oe-rell.loc
                  AND fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.ord-no  EQ oe-rel.ord-no
                  AND fg-bin.loc     EQ oe-rell.loc
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.ord-no  EQ oe-rel.ord-no
                  AND fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.ord-no  EQ oe-rel.ord-no
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.loc     EQ oe-rell.loc
                  AND fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.
   
        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.loc     EQ oe-rell.loc
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ oe-rell.i-no
                  AND fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ oe-rell.i-no
                USE-INDEX co-ino NO-ERROR.
 
        IF AVAILABLE fg-bin THEN 
        DO:        
            IF oe-rell.loc EQ "" OR oe-rell.loc-bin EQ "" THEN
                ASSIGN
                    oe-rell.loc     = fg-bin.loc
                    oe-rell.loc-bin = fg-bin.loc-bin
                    .
            IF addrelse-cha NE "No Tags" AND cSelectedValue NE "NoTag" THEN
                oe-rell.tag      = fg-bin.tag.
        
            ASSIGN
                oe-rell.job-no   = fg-bin.job-no
                oe-rell.job-no2  = fg-bin.job-no2
                oe-rell.qty-case = fg-bin.case-count
                .
       
        END.
                           
        ELSE 
            IF lFgFile THEN 
            DO:
                FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ cocode
                      AND itemfg.i-no    EQ oe-rell.i-no
                    NO-ERROR.
                IF oe-rell.loc EQ "" OR oe-rell.loc-bin EQ "" THEN 
                DO:
          
                    ASSIGN
                        oe-rell.loc     = itemfg.def-loc
                        oe-rell.loc-bin = itemfg.def-loc-bin
                        .
                END.
            END.
    END.
  
    IF oe-rell.loc EQ "" OR oe-rell.loc-bin EQ "" THEN 
    DO:
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ oe-rell.i-no
            NO-ERROR.
        IF AVAILABLE itemfg THEN
            ASSIGN
                oe-rell.loc     = itemfg.def-loc
                oe-rell.loc-bin = itemfg.def-loc-bin
                .
        IF oe-rell.loc EQ "" OR oe-rell.loc-bin EQ "" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ cocode
                  AND cust.active  EQ "X" 
                NO-ERROR.
            IF AVAILABLE cust THEN 
            DO:
                FIND FIRST shipto WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ cust.cust-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN
                    ASSIGN   
                        oe-rell.loc     = shipto.loc
                        oe-rell.loc-bin = shipto.loc-bin.
            END.            
        END.
    END.

    /* lcLocBin is from an NK1 bolprint */
    IF (oe-rell.loc-bin EQ "" 
        OR (oe-rel.spare-char-1 NE "" AND oe-rell.loc NE oe-rel.spare-char-1))
        AND lcBOLWhse EQ "ShipFromWhse"
        THEN 
    DO:
        IF oe-rel.spare-char-1 NE "" AND oe-rell.loc NE oe-rel.spare-char-1 THEN
            oe-rell.loc = oe-rel.spare-char-1.
        oe-rell.loc-bin = lcLocBin.
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ cocode
              AND fg-bin.loc     EQ oe-rell.loc
              AND fg-bin.loc-bin EQ oe-rell.loc-bin
            NO-ERROR.
        IF NOT AVAILABLE fg-bin THEN 
        DO:
            RUN create-fg-bin (INPUT ROWID(oe-rell)).        
        END.

    END.

    FIND CURRENT oe-rell NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-next-r-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-next-r-no Procedure 
PROCEDURE get-next-r-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /** Find last actual release for this order number and add 1 to
        the get the next release. **/
    /* === rel-no logic moved to line (oe-rell) ========*/


    FOR EACH bf-rell
        WHERE bf-rell.company EQ cocode
        AND bf-rell.ord-no  EQ oe-rel.ord-no NO-LOCK 
        BY bf-rell.rel-no DESCENDING:
    
        iNxtRelNo =  bf-rell.rel-no.
        LEAVE.  
    END.
    iNxtRelNo = iNxtRelNo + 1.
/*========== */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-process-type-i) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-type-i Procedure 
PROCEDURE process-type-i :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprOeRell AS ROWID       NO-UNDO.
    DEFINE BUFFER bf-oe-rell FOR oe-rell.


    FIND bf-oe-rell 
        WHERE ROWID(bf-oe-rell) EQ iprOeRell 
        EXCLUSIVE-LOCK.


    ASSIGN 
        bf-oe-rell.loc-bin = ""
        /*   bf-oe-rell.job-no  = "" */
        /*   bf-oe-rell.job-no2 = 0  */
        .



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rel-bin-process) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rel-bin-process Procedure 
PROCEDURE rel-bin-process :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    ASSIGN
        ll        = NO
        cJobNo = TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99")
        .

    IF cJobNo EQ "-00" THEN cJobNo = "".

  
    cSCode  = IF oe-rel.s-code <> "" THEN oe-rel.s-code ELSE
        IF oe-ordl.is-a-component THEN "S" 
            ELSE IF AVAILABLE oe-ctrl AND oe-ctrl.ship-from THEN "B" 
            ELSE "I".
    IF addrelse-cha EQ "Bin/Tag" AND oe-rel.tot-qty > 0 AND cSCode NE "I" THEN  /*task# 09200502*/
        RUN oe/d-relbin.w (cocode, oe-rel.ord-no,oe-rel.i-no,oe-rel.po-no,cJobNo,oe-rel.tot-qty,OUTPUT cSelectedValue).
    ELSE cSelectedValue = "NoTag".

    ASSIGN
        cAllOrOne = /*STRING(ll,"ALL/ONE")*/ cSelectedValue  /*all,one,notag*/
        lBinTag    = cAllOrOne NE "ONE" OR
                   CAN-FIND(FIRST fg-bin
                            WHERE fg-bin.company EQ cocode
                              AND fg-bin.i-no    EQ oe-ordl.i-no
                              AND fg-bin.job-no  EQ oe-ordl.job-no
                              AND fg-bin.job-no2 EQ oe-ordl.job-no2
                              AND fg-bin.qty     GT 0).
                              
    /* If user specified 'no tag' then override lBinTag to avoid prompting */
    IF cSelectedValue = "NoTag" THEN 
        lBinTag = NO.       


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update-oe-rel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-oe-rel Procedure 
PROCEDURE update-oe-rel :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    FIND CURRENT oe-relh EXCLUSIVE.
    FIND CURRENT oe-rel EXCLUSIVE.
    ASSIGN
        oe-relh.printed      = NO
        oe-relh.spare-char-3 = ""
        oe-rel.rel-no        = iNxtRelNo
        oe-rel.b-ord-no = oe-relh.b-ord-no
        .
    FIND CURRENT oe-relh NO-LOCK.
    FIND CURRENT oe-rel NO-LOCK.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update-rel-stat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-rel-stat Procedure 
PROCEDURE update-rel-stat :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprOeRel AS ROWID       NO-UNDO.
    DEFINE BUFFER bf-oe-rel FOR oe-rel.
    FIND  bf-oe-rel WHERE ROWID(bf-oe-rel) EQ iprOeRel EXCLUSIVE NO-ERROR.
    IF NOT AVAILABLE bf-oe-rel THEN 
    DO:
        MESSAGE "Internal error - no oe-rel found (update-rel-stat)"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.
    RUN oe/rel-stat.p (ROWID(bf-oe-rel), OUTPUT bf-oe-rel.stat).
    FIND CURRENT bf-oe-rel NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

