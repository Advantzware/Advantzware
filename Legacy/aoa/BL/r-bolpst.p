/*------------------------------------------------------------------------
  File: r-bole&p.w
  Description: BOL Edit List & Posting

    1) handle lUserChoice to print exception report
    2) handle email capability
    3) create capability to handle PROCEDURE email-reorderitems
    4) Create capability for: BOL posting Exception Report 
    5) PROCEDURE email-reorderitems :
    6) implment delete logic on oe-bolh marked as deleted
*/

/* ***************************  Definitions  ***************************/

/* Post BOL Create Invoice.rpa */
{aoa/tempTable/ttPostBolCreateInvoice.i}
DEFINE NEW SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_track_usage AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE g_header_line AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_groups AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE init_menu AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE g_developer AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_version AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_rec_key AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_pageno AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_mainmenu AS WIDGET-HANDLE NO-UNDO.

g_lookup-var = "".
{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttPostBOLCreateInvoice.
{aoa/includes/pPostBOLCreateInvoice.i}

DEFINE VARIABLE cTransactionTime AS CHARACTER NO-UNDO LABEL "Time" FORMAT "x(20)":U.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i &new=NEW}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i NEW SHARED}
{custom/globdefs.i &NEW=NEW}

/* subject business logic */
ASSIGN
    cocode = ipcCompany
    locode = cLocation
    g_company = ipcCompany
    g_loc     = cLocation
    .
{oe/oe-bolp1.i NEW}
{oe/d-selbin.i NEW} /* w-bin definition */

DEFINE VARIABLE cTagDisplay        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTagNumber         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTagNumber2        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDisplayFullTag    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iLineCount         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lCheckQty          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cReturnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecordFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cExternalProgram   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInvoiceStatusType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lInvoiceStatusLog  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lInvalidDate       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUserChoice        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrintInvoice      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iCountNotPosted    AS INTEGER   NO-UNDO.
DEFINE VARIABLE hExtProgramHandle  AS HANDLE    NO-UNDO.
DEFINE VARIABLE cAutoSelectShipFromAlpha AS CHARACTER NO-UNDO.
DEFINE VARIABLE lTaglessBOLExists  AS LOG        NO-UNDO.
DEFINE VARIABLE cAutoSelectTagAlpha AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAutoSelectShipFrom AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAutoSelectShipFrom AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cLogFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDebugLog AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseLogs AS LOGICAL NO-UNDO.
/* for sel-bins */
DEF NEW SHARED VAR out-recid AS RECID NO-UNDO.
DEFINE BUFFER xoe-boll FOR oe-boll.
DEFINE BUFFER bf-oe-boll FOR oe-boll.
DEFINE STREAM sDebug.
lUseLogs = NO. /* Use debug logging */
if search("logs/" + "r-bolpst" + ".txt") ne ? then 
  lUseLogs = true.
cDebugLog = "logs/" + "r-bolpst" + STRING(TODAY,"99999999") + STRING(TIME) + STRING(RANDOM(1,10)) + ".txt".
IF lUseLogs THEN 
  OUTPUT STREAM sDebug TO VALUE(cDebugLog).

cLogFile = "logs/" + "r-bolpst" + STRING(TODAY,"99999999") + STRING(TIME) + STRING(RANDOM(1,10)) + ".errs".
IF lUseLogs THEN 
  OUTPUT TO VALUE(cLogFile).
{oe/closchk.i NEW}

DEFINE TEMP-TABLE tt-email NO-UNDO
    FIELD tt-recid AS RECID
    FIELD bol-no LIKE oe-boll.bol-no
    FIELD ord-no LIKE oe-boll.ord-no
    FIELD i-no   LIKE itemfg.i-no
    FIELD qty      AS INTEGER
    FIELD cust-no  AS CHARACTER
        INDEX tt-cust IS PRIMARY cust-no DESCENDING
        .
    
DEFINE TEMP-TABLE ttSelectedOeRell LIKE oe-rell.

/* ************************  Function Prototypes ********************** */
FUNCTION fDebugMsg RETURNS CHARACTER 
	(INPUT ipcMessage AS CHARACTER ) FORWARD.

/* ***************************  Main Block  *************************** */    
PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /* security check need {methods/prgsecur.i} in definition section */
    FIND FIRST oe-ctrl NO-LOCK
         WHERE oe-ctrl.company EQ cocode
         NO-ERROR.
    lPrintInvoice = oe-ctrl.u-inv.

    FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "BolPostTime"
        NO-ERROR.

    IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "Fixed Time" THEN
         ASSIGN cTransactionTime = STRING(int(SUBSTRING(STRING(sys-ctrl.dec-fld),1,2)) * 60 * 60 + INT(SUBSTRING(STRING(sys-ctrl.dec-fld),3,4)) * 60 , "hh:mm:ss").
    ELSE ASSIGN cTransactionTime = STRING(TIME,"hh:mm:ss").
      
    RELEASE sys-ctrl.

    FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "BOLPOST"
        NO-ERROR.
    lCheckQty = sys-ctrl.char-fld EQ "Bin>Qty".
  
    /* Invstatus to determine invoice status when created  */
    RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "L", NO, NO, "", "", 
        OUTPUT cReturnChar, OUTPUT lRecordFound).
    lInvoiceStatusLog = LOGICAL(cReturnChar).

    /* Invstatus to determine invoice status when created  */
    RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "C", NO, NO, "", "", 
        OUTPUT cInvoiceStatusType, OUTPUT lRecordFound).


    DO TRANSACTION:
    /* No prompt for creation of nk1 */
        {sys/inc/fgreorder.i}
    END.
    
    /* Best default for GUI applications is...                              */
    PAUSE 0 BEFORE-HIDE.

    FIND FIRST oe-ctrl NO-LOCK
         WHERE oe-ctrl.company EQ cocode
         NO-ERROR.
    lPrintInvoice = oe-ctrl.u-inv.
 
    RUN pCheckDate.  
    IF lInvalidDate THEN RETURN NO-APPLY.

    iCountNotPosted = 0.
        
    FOR EACH w-ord.
        DELETE w-ord.
    END.

    EMPTY TEMP-TABLE tt-email.

    RUN pRunReport. 

    IF lPost THEN DO:
        IF CAN-FIND(FIRST w-bolh) THEN DO:

            fDebugMsg("Start Post pPostBols").
            RUN pPostBols.
            /* close transfer order here, Non-UI procedure */
            RUN oe/closchk.p (0).
            /* WFk- 5/4/12- This is here to make sure it is the last thing in*/
            /* the posting process.  Posting relies on a cleanup routine     */
            /* for releases instead of fixing the real problem.              */
            FOR EACH w-bolh,
                FIRST oe-bolh NO-LOCK
                WHERE RECID(oe-bolh) EQ w-bolh.w-recid
                :
                FOR EACH oe-boll NO-LOCK
                    WHERE oe-boll.b-no EQ oe-bolh.b-no
                    :
                    FIND FIRST oe-ordl NO-LOCK
                         WHERE oe-ordl.company EQ oe-boll.company
                           AND oe-ordl.ord-no  EQ oe-boll.ord-no
                           AND oe-ordl.line    EQ oe-boll.line
                         NO-ERROR.
                    RUN oe/cleanrel.p (INPUT ROWID(oe-ordl)).    
                END. /* each oe-boll */
            END. /* each w-bolh */

            FOR EACH w-ord:
                /* Non-UI procedure*/
                RUN oe/close.p (w-ord.rec-id, YES).  
            END.

            FIND FIRST tt-email NO-LOCK NO-ERROR.
            IF AVAILABLE tt-email THEN RUN email-reorderitems.
        END. /* If can-find w-bolh */

    END. /* if lPost */
    RUN pProcessNoPostRecs.  
    FOR EACH ttPostBOLCreateInvoice:
        fDebugMsg("After pRunReport each ttPostBolCreateInvoice " + STRING(ttPostBOLCreateInvoice.bolNo)).        
        FIND FIRST oe-bolh NO-LOCK
            WHERE oe-bolh.company EQ cocode 
              AND oe-bolh.bol-no EQ ttPostBOLCreateInvoice.bolNo
              NO-ERROR.
        IF AVAILABLE oe-bolh AND oe-bolh.posted = TRUE THEN 
          ASSIGN 
             ttPostBolCreateInvoice.bolStatus = "Posted"
             ttPostBolCreateInvoice.reason    = ""
             .
        ELSE DO:
          ASSIGN 
             ttPostBolCreateInvoice.bolStatus = "Not Posted"
             .
          IF lPost = YES AND ttPostBolCreateInvoice.reason EQ "" THEN 
            ttPostBolCreateInvoice.reason = "Undetermined".
          FIND FIRST w-except 
              WHERE w-except.bol-no EQ ttPostBolCreateInvoice.bolNo
              NO-ERROR. 
          IF AVAILABLE w-except THEN 
            ttPostBolCreateInvoice.reason = "Insufficient Inventory".
        END.
          
    END.    
END. /* main block */

OUTPUT STREAM sDebug CLOSE.
/* **********************  Internal Procedures  *********************** */

PROCEDURE pAutoSelectTags:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
            
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprOeBoll AS ROWID NO-UNDO.
    
    DEFINE VARIABLE lSelectTags   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lNoneSelected AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE hTToe-rel     AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hBufOeRell       AS HANDLE NO-UNDO.
    DEFINE VARIABLE hQueryOeRell     AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBufDynmicOeRell AS HANDLE NO-UNDO.
    DEFINE VARIABLE li               AS INTEGER NO-UNDO.
    
    /* Needed for processing selected tags */
    DEFINE VARIABLE li-selected-qty  AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-update-qty-no AS LOG     NO-UNDO.
    DEFINE VARIABLE ll-BOLQtyPopup   AS LOG     NO-UNDO.
    DEFINE VARIABLE ll-change-qty    AS LOG. 
    DEFINE VARIABLE v-qty            AS INTEGER NO-UNDO.
    
    /* specially defined for r-bolpst */
    DEFINE VARIABLE ip-rowid         AS ROWID   NO-UNDO.
    DEFINE VARIABLE nufile           AS LOGICAL NO-UNDO. /* set to indicate a new record, not needed here */
    DEFINE VARIABLE fil_id           AS RECID   NO-UNDO. /* set when record is added to the recid of created oe-boll */
    DEFINE VARIABLE boll_id          AS RECID   NO-UNDO. /* set when record is added to the recid of created oe-boll */
    DEFINE VARIABLE bolh_id          AS RECID   NO-UNDO.
    DEFINE VARIABLE iWBinSeq         AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-oerel FOR oe-rel.
    /* defining temporarily */
    DEFINE VARIABLE op-rowid-list AS CHARACTER .   
    DEFINE BUFFER b-reftable FOR reftable. /* used to find and copy lot-no when creating oe-boll */
    
    FIND xoe-boll EXCLUSIVE-LOCK
        WHERE ROWID(xoe-boll) EQ iprOeBoll 
        NO-ERROR.
            
    /* Get ship-from location */
    FIND FIRST oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ xoe-boll.company
          AND oe-ordl.ord-no  EQ xoe-boll.ord-no
          AND oe-ordl.line    EQ xoe-boll.line
          NO-ERROR.

    fDebugMsg("avail order " + STRING(AVAILABLE(oe-ordl))).
    IF NOT AVAILABLE oe-ordl THEN 
        RETURN.
  
    FIND FIRST oe-rell NO-LOCK 
       WHERE oe-rell.r-no EQ xoe-boll.r-no
       NO-ERROR.
       
    IF AVAILABLE oe-rell THEN 
    FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no
        NO-ERROR. 
        
    IF NOT AVAILABLE oe-rel THEN   
    FIND FIRST oe-rel NO-LOCK
        WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
            AND oe-rel.stat    EQ "P"
            NO-ERROR.
    fDebugMsg("avail oerel " + STRING(AVAILABLE(oe-rel))).
    IF NOT AVAILABLE oe-rel THEN 
        RETURN.
    FIND FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no eq xoe-boll.r-no 
        NO-ERROR.

    fDebugMsg("avail oe-relh" + STRING(AVAILABLE(oe-relh))).        
    IF NOT AVAIL oe-relh THEN
      RETURN.
      
    out-recid = RECID(oe-relh).
    /* Run Standard Tag Selection but only for specific ship from location */
   
    ASSIGN lSelectTags = NO
           iWBinSeq    = 0
           lSelectTags = FALSE.
    EMPTY TEMP-TABLE w-bin.

    fDebugMsg("start fifoloop").     
    RUN oe/fifoloopTags.p (ROWID(xoe-boll), lSelectTags, oe-rel.spare-char-1 /*oe-boll.ship-from */, OUTPUT lNoneSelected, OUTPUT hTToe-rel).
    fDebugMsg("end fifoloop").     
    /* From fifoloop, process returned dynamic temp-table to retrieve tag records selected */
    hBufOeRell = hTToe-rel:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY hQueryOeRell.
    hQueryOeRell:SET-BUFFERS(hBufOeRell).
    hQueryOeRell:QUERY-PREPARE("FOR EACH ttoe-rell").
    hQueryOeRell:QUERY-OPEN().
    hBufDynmicOeRell = BUFFER ttSelectedOeRell:HANDLE.
    
    /* Run a query to access the TEMP-TABLE            */
    /* Display the oe-rell number and the oe-ordl name */
    REPEAT:
        
        hQueryOeRell:GET-NEXT() NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            LEAVE.
        END.
        
        IF hQueryOeRell:QUERY-OFF-END THEN LEAVE.
        CREATE ttSelectedOeRell.
        fDebugMsg("get-next hqueiryoe-rell").            
        hBufDynmicOeRell:BUFFER-COPY(hBufOeRell).
        fDebugMsg( "auto bin returned " + STRING(ttSelectedOeRell.tag)).
        /* In case duplicate w-bin's were created, check first if exists */
        FIND FIRST w-bin 
            WHERE w-bin.company EQ ttSelectedOeREll.company
              AND w-bin.i-no EQ ttSelectedOeREll.i-no
              AND w-bin.loc EQ ttSelectedOeREll.loc
              AND w-bin.loc-bin EQ ttSelectedOeREll.loc-bin
              AND w-bin.tag EQ ttSelectedOeREll.tag
              AND w-bin.job-no EQ ttSelectedOeREll.job-no
              AND w-bin.job-no2 EQ ttSelectedOeREll.job-no2
              AND w-bin.cust-no EQ ttSelectedOeREll.cust-no
              AND w-bin.cust-no EQ ttSelectedOeREll.cust-no
              NO-ERROR.
        IF NOT AVAILABLE w-bin THEN                   
          CREATE w-bin.    
        BUFFER-COPY ttSelectedOeRell EXCEPT qty TO w-bin.
        FIND FIRST fg-bin NO-LOCK  
              WHERE fg-bin.company    EQ ttSelectedOeRell.company
                  AND fg-bin.i-no     EQ  ttSelectedOeRell.i-no
                  AND fg-bin.loc      EQ  ttSelectedOeRell.loc
                  AND fg-bin.loc-bin  EQ  ttSelectedOeRell.loc-bin
                  AND fg-bin.tag      EQ  ttSelectedOeRell.tag
                  NO-ERROR.      
        ASSIGN 
            iWBinSeq         = iWBinSeq + 1
            w-bin.selekt     = "?" /* what is this */
            w-bin.seq        = iWBinSeq
            w-bin.selekt-log = YES 
            w-bin.rec-id     = RECID(fg-bin)
            w-bin.units      = 0
            w-bin.to-rel     = 0
            w-bin.to-bol     = 0
            w-bin.rfid       = ""
            w-bin.qty        = INTEGER(ttSelectedOeRell.qty)
            .
    END. /* Repeat */
        
    hQueryOeRell:QUERY-CLOSE().
    hBufOeRell:BUFFER-RELEASE().
    
    DELETE OBJECT hTToe-rel.
    DELETE OBJECT hQueryOeRell.

    FOR EACH w-bin:
        ASSIGN li-selected-qty = li-selected-qty + w-bin.qty.
    END.
    fDebugMsg("selected qty " + STRING(li-selected-qty) ).
    /* Was not able to assign full quantity from tagged inventory */
    IF li-selected-qty LT xoe-boll.qty THEN DO:
        
        CREATE w-nopost.
        ASSIGN
            w-nopost.ord-no   = xoe-boll.ord-no
            w-nopost.i-no     = xoe-boll.i-no
            w-nopost.bol-no   = xoe-boll.BOL-no
            w-nopost.rel-no   = xoe-boll.REL-no
            w-nopost.b-ord-no = xoe-boll.b-ord-no
            w-nopost.cust-no  = xoe-boll.cust-no
            w-nopost.po-no    = xoe-boll.PO-NO
            w-nopost.reason   = "Insufficient Inventory"
            .        
      RETURN.
    END.
      
    /* If full quantity was selected, then process creation of oe-boll lines */
    v-qty     = xoe-boll.qty.    
    IF ll-change-qty AND li-selected-qty GT v-qty THEN DO:
        /* Hard code user selection Transfer scheduled release quantity instead of total of bins selected */
        ll-change-qty = FALSE.
    END.

    /* Create oe-boll records based on tags selected */
    op-rowid-list = "".
    FIND FIRST bf-oerel
         WHERE ROWID(bf-oerel) EQ ROWID(oe-rel)
        NO-ERROR.
    
    /* This loop will exit when v-qty is reduced to zero (in sel-bins.i) */
    FOR EACH w-bin , 
        FIRST fg-bin
          WHERE RECID(fg-bin) EQ w-bin.rec-id
          NO-LOCK,
        FIRST itemfg
          WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ fg-bin.i-no
          NO-LOCK
          BREAK 
             BY w-bin.seq 
             BY w-bin.tag:

            
        FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ xoe-boll.b-no NO-LOCK.
        ASSIGN 
          bolh_id = RECID(oe-bolh)        
          ip-rowid = ROWID(xoe-boll)
          .
        /* creates oe-boll if does not exist (based on ip-rowid), and assigns fg-bin values to it */
        {oe/sel-bins.i "oe-bol"}
        oe-boll.weight = oe-boll.qty / 100 * itemfg.weight-100.
    
        IF NOT AVAILABLE oe-ordl THEN
            FIND FIRST oe-ordl WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ xoe-boll.ord-no
                AND oe-ordl.i-no    EQ xoe-boll.i-no 
                NO-LOCK NO-ERROR.
    
        op-rowid-list = op-rowid-list + STRING(ROWID(oe-boll)) + ",".

    END. /* for each w-bin, create the oe-boll record */

    /* Special for r-bolpst - Delete the original non-tag bol lines */
    FIND oe-bolh 
        WHERE RECID(oe-bolh) EQ bolh_id
        NO-LOCK NO-ERROR.
        
    /* Mark oe-boll lines as complete */
    FIND oe-bolh 
        WHERE RECID(oe-bolh) EQ bolh_id
        NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN 
    DO:
        FOR EACH oe-boll 
            WHERE oe-boll.b-no EQ oe-bolh.b-no
            EXCLUSIVE-LOCK
            BREAK BY oe-boll.ord-no
            BY oe-boll.i-no:
            IF FIRST-OF(oe-boll.i-no) THEN 
            DO:
                {oe/oe-bolpc.i ALL}
            END.
        END. /* each oe-boll */
    END. /* avail oe-bolh */
        fDebugMsg("leaving fifoloop").     
END PROCEDURE.

PROCEDURE pCheckDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Validate in front end */
END PROCEDURE.

PROCEDURE pCreateNoPostRec :
    DEFINE INPUT PARAMETER cNoPostReason  LIKE w-nopost.reason NO-UNDO.

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ oe-boll.company
          AND itemfg.i-no    EQ oe-boll.i-no
        NO-ERROR.
    fDebugMsg("pCreateNoPostREc  " + STRING(oe-boll.bol-no)).
    CREATE w-nopost.
    ASSIGN
        w-nopost.ord-no   = oe-boll.ord-no
        w-nopost.i-no     = oe-boll.i-no
        w-nopost.i-name   = IF AVAILABLE itemfg THEN itemfg.i-name ELSE "Not on File"
        w-nopost.bol-date = oe-bolh.BOL-date
        w-nopost.bol-no   = oe-bolh.BOL-no
        w-nopost.rel-no   = oe-boll.REL-no
        w-nopost.b-ord-no = oe-boll.b-ord-no
        w-nopost.cust-no  = oe-bolh.cust-no
        w-nopost.po-no    = oe-boll.PO-NO
        w-nopost.reason   = cNoPostReason
        .
    DELETE w-bolh.
END PROCEDURE.

PROCEDURE pCreateReorder :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprOEBollRowID AS ROWID NO-UNDO.

    DEFINE BUFFER bOEBoll FOR oe-boll.

    DEFINE VARIABLE iQtyOnHand    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQtyAvailable AS INTEGER NO-UNDO.
    DEFINE VARIABLE iReordQty     AS INTEGER NO-UNDO.

    FIND FIRST bOEBoll NO-LOCK
         WHERE ROWID(bOEBoll) EQ iprOEBollRowID
         NO-ERROR.
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ cocode
           AND itemfg.i-no    EQ bOEBoll.i-no
         NO-ERROR.

    iQtyOnHand = 0.
    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
        :
        iQtyOnHand = iQtyOnHand + fg-bin.qty.
    END.

    iQtyAvailable = iQtyOnHand - itemfg.q-alloc.
 
    IF itemfg.ord-level GT iQtyAvailable THEN DO:
        iReordQty = itemfg.ord-level - iQtyAvailable.

        IF iReordQty LT itemfg.ord-min AND itemfg.ord-min NE 0 THEN 
        iReordQty = itemfg.ord-min.

        IF iReordQty GT itemfg.ord-max AND itemfg.ord-max NE 0 THEN 
        iReordQty = itemfg.ord-max.
    END.
    ELSE iReordQty = 0.

    IF iReordQty GT 0 THEN DO:
        CREATE tt-email.
        ASSIGN 
            tt-email.bol-no  = bOEBoll.bol-no
            tt-email.ord-no  = bOEBoll.ord-no
            tt-email.i-no    = bOEBoll.i-no
            tt-email.qty     = iReordQty
            tt-email.cust-no = oe-bolh.cust-no
            .
    END.
END PROCEDURE.

PROCEDURE pExceptionRpt :
    /* -------------------------------------------------- oe/oe-bolp7.p 11/01 JLF */
    /* BOL posting Exception Report                                               */
    /* -------------------------------------------------------------------------- */
    FIND FIRST period NO-LOCK
        WHERE period.company EQ gcompany
          AND period.pst     LE dtPostDate
          AND period.pend    GE dtPostDate
        NO-ERROR.
 
    FOR EACH w-except,
        FIRST oe-bolh
        WHERE oe-bolh.company EQ cocode
          AND oe-bolh.bol-no  EQ w-except.bol-no
        NO-LOCK
        BREAK BY w-except.bol-no
              BY w-except.ord-no
              BY w-except.rel-no
              BY w-except.b-ord-no
        :
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ w-except.i-no
            NO-ERROR.
        CREATE ttPostBOLCreateInvoice.
        ASSIGN
            iTagNumber  = 0
            cTagNumber2 = 0
            iTagNumber  = LENGTH(w-except.tag)
            cTagNumber2 = iTagNumber - 5
            cTagDisplay = IF NOT iDisplayFullTag AND iTagNumber NE 0 THEN SUBSTR(w-except.tag,cTagNumber2,6)
                          ELSE w-except.tag
            ttPostBOLCreateInvoice.bolDate   = oe-bolh.bol-date
            ttPostBOLCreateInvoice.bolNo     = oe-bolh.bol-no
            ttPostBOLCreateInvoice.carrier   = oe-bolh.carrier
            ttPostBOLCreateInvoice.trailer   = oe-bolh.trailer
            ttPostBOLCreateInvoice.freight   = oe-bolh.freight
            ttPostBOLCreateInvoice.cwt       = oe-bolh.cwt
            ttPostBOLCreateInvoice.totWgt    = oe-bolh.tot-wt
            ttPostBOLCreateInvoice.custNo    = oe-bolh.cust-no
            ttPostBOLCreateInvoice.shipId    = oe-bolh.ship-id
            ttPostBOLCreateInvoice.deleted   = oe-bolh.deleted
            ttPostBOLCreateInvoice.iNo       = w-except.i-no
            ttPostBOLCreateInvoice.tag       = cTagDisplay
            ttPostBOLCreateInvoice.iName     = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""
            ttPostBOLCreateInvoice.poNo      = w-except.po-no
            ttPostBOLCreateInvoice.ordNo     = w-except.ord-no
            ttPostBOLCreateInvoice.relNo     = w-except.rel-no /* STRING(w-except.rel-no,">>9") + "-" + STRING(w-except.b-ord-no,"99") */ 
            ttPostBolCreateInvoice.bOrdNo    = w-except.b-ord-no
            ttPostBOLCreateInvoice.loc       = w-except.loc
            ttPostBOLCreateInvoice.locBin    = w-except.loc-bin
            ttPostBOLCreateInvoice.cases     = w-except.cases
            ttPostBOLCreateInvoice.qtyCase   = w-except.qty-case
            ttPostBOLCreateInvoice.partial   = w-except.partial
            ttPostBOLCreateInvoice.weight    = w-except.weight  
            ttPostBOLCreateInvoice.bolStatus = "Exception"  
            ttPostBOLCreateInvoice.reason    = "Exception"
            .
    END.
END PROCEDURE.

PROCEDURE pPostBols :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* DEFINE VARIABLE lv-exception AS LOG     NO-UNDO. */
    DEFINE VARIABLE dActualQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE hRelLib    AS HANDLE  NO-UNDO.

    /* Deletes xreport of report */
    {sa/sa-sls01.i}

    DISABLE TRIGGERS FOR LOAD OF itemfg.
    FOR EACH w-bolh,
        FIRST oe-bolh NO-LOCK
        WHERE RECID(oe-bolh) EQ w-bolh.w-recid
        BREAK BY oe-bolh.b-no
              BY oe-bolh.bol-no
        :
        IF NOT FIRST-OF(oe-bolh.b-no) THEN DELETE w-bolh.
    END.

    FIND FIRST sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ cocode
           AND sys-ctrl.name    EQ 'EDIBOLPost'
         NO-ERROR.
    /**********************  POSTING BLOCK  ****************************/
    post-blok:
    DO TRANSACTION.
        bolh:
        FOR EACH w-bolh,
            FIRST oe-bolh NO-LOCK
            WHERE RECID(oe-bolh) EQ w-bolh.w-recid,
            FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
              AND cust.cust-no EQ oe-bolh.cust-no                  
            BREAK BY oe-bolh.bol-no
                  BY oe-bolh.ord-no
                  BY oe-bolh.rel-no
            :
            /* Create tt-fg-bin */
            IF FIRST-OF(oe-bolh.bol-no) AND lPrintInvoice AND lCheckQty THEN
            RUN oe/bolcheck.p (ROWID(oe-bolh)).
            
            /* Find out if autoSelectingTags for this customer */
            RUN sys/ref/nk1look.p (cocode, "BOLPOST", "C", YES, YES /* Cust# */, oe-bolh.cust-no, "" /* ship-to value */, 
                OUTPUT cAutoSelectShipFrom, OUTPUT lRecordFound).
            RUN sys/ref/nk1look.p (cocode, "BOLPOST", "L", YES, YES /* Cust# */, oe-bolh.cust-no, "" /* ship-to value */, 
                OUTPUT cAutoSelectShipFromAlpha, OUTPUT lRecordFound).
                lAutoSelectShipFrom = LOGICAL(cAutoSelectShipFromAlpha).
            FIND FIRST w-except WHERE w-except.bol-no EQ oe-bolh.bol-no NO-ERROR.
            fDebugMsg("cAutoSelectShipFrom " + cAutoSelectShipFrom + " " + STRING(lAutoSelectShipFrom) + 
                                          " avail w-except " + STRING(AVAILABLE(w-except))).
            IF lAutoSelectShipFrom  THEN DO:
              lTaglessBOLExists = FALSE.
                FOR EACH bf-oe-boll NO-LOCK                   
                   WHERE bf-oe-boll.b-no EQ oe-bolh.b-no                   
                   :
                  IF bf-oe-boll.tag EQ "" THEN
                     lTaglessBOLExists = TRUE.
                END.
                fDebugMsg("decide to lTaglessBolExists " + string(lTaglessBOLExists)).                
                IF AVAIL w-except OR lTaglessBOLExists THEN DO:
                
                    /* Try to assign tags to fulfill BOL Qty */
                    FOR EACH bf-oe-boll NO-LOCK                   
                       WHERE bf-oe-boll.b-no EQ oe-bolh.b-no                   
                       :
                        fDebugMsg("start pUaotSelTags " + string(lTaglessBOLExists)).
                        RUN pAutoSelectTags (INPUT ROWID(bf-oe-boll)).
                    END.
                     
                    FOR EACH w-except 
                      WHERE w-except.bol-no EQ oe-bolh.bol-no 
                      :
                          DELETE w-except.
                    END.
                    /* check if suffient inventory again after selecting tags */
                    RUN oe/bolcheck.p (ROWID(oe-bolh)).
                END.
            END.
            FIND FIRST w-except WHERE w-except.bol-no EQ oe-bolh.bol-no NO-ERROR.
            IF AVAILABLE w-except THEN NEXT bolh.

            iLineCount = iLineCount + 1.

            IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
                FIND FIRST sys-ctrl-shipto NO-LOCK
                     WHERE sys-ctrl-shipto.company      EQ sys-ctrl.company
                       AND sys-ctrl-shipto.name         EQ sys-ctrl.name
                       AND sys-ctrl-shipto.cust-vend    EQ YES
                       AND sys-ctrl-shipto.cust-vend-no EQ w-bolh.cust-no
                       AND sys-ctrl-shipto.log-fld      EQ YES
                     NO-ERROR.
            END. /* avail sys-ctrl */

            cExternalProgram = "sbo/oerel-recalc-act.p".
            RUN VALUE(cExternalProgram) PERSISTENT SET hExtProgramHandle NO-ERROR.
            hRelLib = hExtProgramHandle.

            FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no,
                EACH oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ oe-boll.company
                AND oe-ordl.ord-no EQ oe-boll.ord-no
                AND oe-ordl.line EQ oe-boll.LINE:

                FOR EACH oe-rel NO-LOCK
                    WHERE oe-rel.company EQ oe-ordl.company
                      AND oe-rel.ord-no  EQ oe-ordl.ord-no
                      AND oe-rel.i-no    EQ oe-ordl.i-no
                      AND oe-rel.line    EQ oe-ordl.line
                      AND oe-rel.stat    EQ "P"
                      AND oe-rel.link-no GT 0 
                      AND oe-rel.rel-no  GT 0
                    :
                    /* Set actual quantity */
                    IF AVAILABLE oe-rel AND VALID-HANDLE(hRelLib) THEN 
                        RUN recalc-act-qty IN hRelLib (INPUT ROWID(oe-rel), OUTPUT dActualQty).
                END.
            END.

            IF VALID-HANDLE(hRelLib) THEN
                DELETE OBJECT hRelLib.

            FOR EACH oe-boll NO-LOCK
                WHERE oe-boll.company EQ oe-bolh.company
                  AND oe-boll.b-no    EQ oe-bolh.b-no
                :
                RUN oe/bol-pre-post.p (ROWID(oe-boll), v-term).
                IF fgreorder-log AND cust.ACTIVE EQ "E" THEN
                    RUN pCreateReorder (ROWID(oe-boll)).
            END. /* each oe-boll */
        END. /* for each oe-bolh */

        FOR EACH tt-fg-bin:
            DELETE tt-fg-bin.
        END.
        /* Non-UI process,                                                                                     */
        /* Requires where report.term-id EQ v-term, FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id*/
        /* Requires shared buffer xoe-ord                                                                      */
        FIND FIRST report WHERE report.term-id = v-term NO-LOCK NO-ERROR.
        fDebugMsg("run oe/oe-bolp3 " + v-term + " avail report " + STRING(AVAIL(report))).
        RUN oe/oe-bolp3.p (v-term).
    END. /* post-blok*/
    
    DELETE-BLOK:
    FOR EACH oe-bolh EXCLUSIVE-LOCK
        WHERE oe-bolh.company  EQ cocode
          AND oe-bolh.deleted  EQ YES
          AND oe-bolh.bol-no   GE iStartBOL
          AND oe-bolh.bol-no   LE iEndBOL
          AND oe-bolh.bol-date GE dtStartBOLDate
          AND oe-bolh.bol-date LE dtEndBOLDate
          AND oe-bolh.cust-no  GE cStartCustNo
          AND oe-bolh.cust-no  LE cEndCustNo
          AND oe-bolh.trailer  NE "HOLD"
          AND oe-bolh.stat     EQ "R"
        USE-INDEX deleted
        :
        IF lCustList AND
           NOT CAN-FIND(FIRST ttCustList
                        WHERE ttCustList.cust-no EQ oe-bolh.cust-no
                          AND ttCustList.log-fld EQ TRUE) THEN
        NEXT.
        FOR EACH oe-boll
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no
            :
            DELETE oe-boll.
        END. /* each oe-boll */
        DELETE oe-bolh.
    END. /* each oe-bolh */
END PROCEDURE.

PROCEDURE pProcessNoPostRecs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    iCountNotPosted = 0.
    FOR EACH w-nopost NO-LOCK
      :
      fDebugMsg("process w-nopost " + STRING(w-nopost.bol-no)).
      iCountNotPosted = iCountNotPosted + 1.
      FIND FIRST ttPostBolCreateInvoice
        WHERE ttPostBolCreateInvoice.bolNo EQ w-nopost.bol-no
          AND ttPostBolCreateInvoice.iNo  EQ w-nopost.i-no
        NO-ERROR.
      IF AVAILABLE ttPostBolCreateInvoice THEN 
        ASSIGN 
           ttPostBolCreateInvoice.bolStatus = "Not Posted"
           ttPostBolCreateInvoice.reason    = w-nopost.reason
           .
      ELSE DO:
          CREATE ttPostBolCreateInvoice.
          ASSIGN 
              ttPostBolCreateInvoice.bolNo     = w-nopost.bol-no
              ttPostBolCreateInvoice.bolDate   = w-nopost.bol-date
              ttPostBolCreateInvoice.iNo       = w-nopost.i-no
              ttPostBolCreateInvoice.iName     = w-nopost.i-name
              ttPostBolCreateInvoice.relNo     = w-nopost.rel-no
              ttPostBolCreateInvoice.bOrdNo    = w-nopost.b-ord-no
              ttPostBolCreateInvoice.custNo    = w-nopost.cust-no
              ttPostBolCreateInvoice.poNo      = w-nopost.po-no
              ttPostBolCreateInvoice.bolStatus = "Not Posted"
              ttPostBolCreateInvoice.reason    = w-nopost.reason
              .
      END. /* else...not avail ttPostBolCreateInvoice */
    END. /* each w-nopost */
END PROCEDURE.

PROCEDURE pRunReport :
    /* -------------------------------------------------- oe/oe-bolp2.p 07/97 FWK */
    /* BILL OF LADING POSTING REPORT MODULE 2 - O/E Module                        */
    /* -------------------------------------------------------------------------- */
    DEFINE BUFFER b-oe-boll FOR oe-boll.
    fDebugMsg("In Run Report").
    FIND FIRST period NO-LOCK                
        WHERE period.company EQ gcompany
          AND period.pst     LE dtPostDate
          AND period.pend    GE dtPostDate
        NO-ERROR.

    FOR EACH w-bolh:
        DELETE w-bolh.
    END.

    FOR EACH w-nopost:
        DELETE w-nopost.
    END.

    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company  EQ cocode
          AND oe-bolh.posted   EQ NO
          AND (oe-bolh.printed EQ YES OR lPost EQ NO)
          AND oe-bolh.bol-no   GE iStartBOL
          AND oe-bolh.bol-no   LE iEndBOL
          AND oe-bolh.bol-date GE dtStartBOLDate
          AND oe-bolh.bol-date LE dtEndBOLDate
          AND oe-bolh.cust-no  GE cStartCustNo
          AND oe-bolh.cust-no  LE cEndCustNo
          AND oe-bolh.trailer  NE "HOLD"
          AND oe-bolh.stat     EQ "R"
        USE-INDEX post
        :
        IF lCustList AND
           NOT CAN-FIND(FIRST ttCustList
                        WHERE ttCustList.cust-no EQ oe-bolh.cust-no) THEN
        NEXT.
        CREATE w-bolh.
        ASSIGN
            w-bolh.bol-no   = oe-bolh.bol-no
            w-bolh.ord-no   = oe-bolh.ord-no
            w-bolh.w-recid  = RECID(oe-bolh)
            w-bolh.rel-no   = oe-bolh.rel-no
            w-bolh.b-ord-no = oe-bolh.b-ord-no
            w-bolh.cust-no  = oe-bolh.cust-no
            .
    END. /* each oe-bolh */

    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company  EQ cocode
          AND oe-bolh.deleted  EQ YES
          AND oe-bolh.posted   EQ YES
          AND oe-bolh.bol-no   GE iStartBOL
          AND oe-bolh.bol-no   LE iEndBOL
          AND oe-bolh.bol-date GE dtStartBOLDate
          AND oe-bolh.bol-date LE dtEndBOLDate
          AND oe-bolh.cust-no  GE cStartCustNo
          AND oe-bolh.cust-no  LE cEndCustNo
          AND oe-bolh.trailer  NE "HOLD"
          AND oe-bolh.stat     EQ "R"
        USE-INDEX deleted
        :
        IF lCustList AND
           NOT CAN-FIND(FIRST ttCustList
                        WHERE ttCustList.cust-no EQ oe-bolh.cust-no) THEN
        NEXT.
        CREATE w-bolh.
        ASSIGN
            w-bolh.bol-no   = oe-bolh.bol-no
            w-bolh.ord-no   = oe-bolh.ord-no
            w-bolh.w-recid  = RECID(oe-bolh)
            w-bolh.rel-no   = oe-bolh.rel-no
            w-bolh.b-ord-no = oe-bolh.b-ord-no
            w-bolh.cust-no  = oe-bolh.cust-no
            .
    END. /* each oe-bolh */

    FOR EACH w-bolh,
        FIRST oe-bolh NO-LOCK
        WHERE RECID(oe-bolh) EQ w-bolh.w-recid
        BREAK BY oe-bolh.b-no
              BY oe-bolh.bol-no
        :
        IF NOT FIRST-OF(oe-bolh.b-no) OR
           NOT CAN-FIND(FIRST oe-boll
                        WHERE oe-boll.company EQ oe-bolh.company
                          AND oe-boll.b-no    EQ oe-bolh.b-no
                          AND oe-boll.loc     GE cStartLoc
                          AND oe-boll.loc     LE cEndLoc
                          AND oe-boll.loc-bin GE cStartLocBin
                          AND oe-boll.loc-bin LE cEndLocBin) THEN
        DELETE w-bolh.
    END. /* each w-bolh */
    
    FIND FIRST w-bolh NO-ERROR.
    fDebugMsg("In Run Report - Avail w-bolh?" + STRING(AVAILABLE(w-bolh))).
    MAINBLOK:
    FOR EACH w-bolh
          BY w-bolh.bol-no 
          BY w-bolh.ord-no
          BY w-bolh.rel-no 
          BY w-bolh.b-ord-no
        :
        FIND oe-bolh NO-LOCK WHERE RECID(oe-bolh) EQ w-bolh.w-recid.
        v-tot-post = v-tot-post + 1.
        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no
            BREAK BY oe-boll.company
                  BY oe-boll.b-no
                  BY oe-boll.ord-no
                  BY oe-boll.rel-no
                  BY oe-boll.b-ord-no
            :
            fDebugMsg("run-report each oe-boll " + STRING(oe-boll.bol-no)).
            RELEASE oe-ord.
            RELEASE oe-ordl.
            IF NOT oe-bolh.deleted THEN DO:
                FIND FIRST oe-ord NO-LOCK
                     WHERE oe-ord.company EQ oe-bolh.company
                       AND oe-ord.ord-no = oe-boll.ord-no 
                     NO-ERROR.
                IF NOT AVAILABLE oe-ord THEN DO:
                    RUN pCreateNoPostRec ("Order Was Not Found").
                    NEXT mainblok.
                END.
                /* 04301302 - If customer 'x' and shipto = shipfrom, don't post */
                FIND cust NO-LOCK
                    WHERE cust.company EQ oe-bolh.company
                      AND cust.cust-no EQ oe-bolh.cust-no 
                    NO-ERROR.
                IF AVAIL(cust) AND cust.ACTIVE EQ "X" AND oe-bolh.ship-id = oe-boll.loc THEN DO:
                    RUN pCreateNoPostRec ("Cannot transfer to the same location").
                    NEXT mainblok.
                END.
                FIND FIRST oe-ordl NO-LOCK
                     WHERE oe-ordl.company = oe-boll.company
                       AND oe-ordl.ord-no = oe-boll.ord-no
                       AND oe-ordl.line   = oe-boll.line  NO-ERROR.
                IF NOT AVAILABLE oe-ordl THEN 
                DO:
                    RUN pCreateNoPostRec ("Order Lines Were Not Found").
                    NEXT mainblok.
                END.

                FIND FIRST oe-rell NO-LOCK
                     WHERE oe-rell.company = oe-boll.company
                       AND oe-rell.r-no = oe-boll.r-no
                       AND oe-rell.i-no = oe-boll.i-no
                       AND oe-rell.line = oe-boll.line
                     USE-INDEX r-no NO-ERROR.
                IF NOT AVAILABLE oe-rell THEN DO:
                    RUN pCreateNoPostRec ("Release Lines Were Not Found").
                    NEXT mainblok.
                END.

                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company = oe-boll.company
                       AND itemfg.i-no = oe-boll.i-no
                     NO-ERROR.
                IF NOT AVAILABLE itemfg THEN DO:
                    RUN pCreateNoPostRec ("Finish Good Item Was Not Found").
                    NEXT mainblok.
                END.
            
                IF oe-boll.loc EQ "" OR oe-boll.loc-bin EQ "" THEN DO:
                    RUN pCreateNoPostRec ("Warehouse or Bin is Blank").
                    NEXT mainblok.
                END.

                IF NOT CAN-FIND(FIRST b-oe-boll
                                WHERE b-oe-boll.company EQ oe-bolh.company
                                  AND b-oe-boll.b-no    EQ oe-bolh.b-no
                                  AND b-oe-boll.qty     NE 0) THEN DO:
                    RUN pCreateNoPostRec ("BOL Qty is Zero").
                    NEXT mainblok.
                END.
            END.
            fDebugMsg("In Run Report - Create ttPostBolCreateInvoice").    
            CREATE ttPostBOLCreateInvoice.
            ASSIGN
                ttPostBOLCreateInvoice.bolDate = oe-bolh.BOL-date
                ttPostBOLCreateInvoice.bolNo   = oe-bolh.BOL-no
                ttPostBOLCreateInvoice.carrier = oe-bolh.CARRIER
                ttPostBOLCreateInvoice.trailer = oe-bolh.TRAILER
                ttPostBOLCreateInvoice.freight = oe-bolh.FREIGHT
                ttPostBOLCreateInvoice.cwt     = oe-bolh.CWT
                ttPostBOLCreateInvoice.totWgt  = oe-bolh.TOT-WT
                ttPostBOLCreateInvoice.custNo  = oe-bolh.cust-no
                ttPostBOLCreateInvoice.shipID  = oe-bolh.ship-id
                ttPostBOLCreateInvoice.deleted = oe-bolh.deleted
                .

            ASSIGN
                ttPostBOLCreateInvoice.iNo     = oe-boll.i-no
                ttPostBOLCreateInvoice.iName   = itemfg.i-name WHEN AVAILABLE itemfg
                ttPostBOLCreateInvoice.poNo    = oe-boll.po-no
                ttPostBOLCreateInvoice.ordNo   = oe-boll.ord-no
                ttPostBOLCreateInvoice.relNo   = oe-boll.rel-no
                ttPostBOLCreateInvoice.bOrdNo  = oe-boll.b-ord-no
                ttPostBOLCreateInvoice.loc     = oe-boll.loc
                ttPostBOLCreateInvoice.locBin  = oe-boll.loc-bin
                ttPostBOLCreateInvoice.tag     = oe-boll.tag                                       
                ttPostBOLCreateInvoice.cases   = oe-boll.CASES
                ttPostBOLCreateInvoice.qtyCase = oe-boll.qty-CASE
                ttPostBOLCreateInvoice.partial = oe-boll.PARTIAL
                ttPostBOLCreateInvoice.weight  = oe-boll.WEIGHT
                .
            IF SUBSTR(oe-boll.tag,1,15) EQ oe-boll.i-no THEN        
                ttPostBOLCreateInvoice.tag = SUBSTR(oe-boll.tag,16,8). 
            IF AVAILABLE oe-ord  AND
               AVAILABLE oe-ordl AND
               oe-ordl.ship-qty + oe-boll.qty GT
               oe-ordl.qty * (1 + (oe-ordl.over-pct / 100)) THEN
            ttPostBOLCreateInvoice.reason = "Qty Shipped will exceed Qty Ordered + Allowable Overrun".
        END. /* each oe-boll */
    END. /* each w-bolh */
END PROCEDURE.

{aoa/BL/pBuildCustList.i}

/* ************************  Function Implementations ***************** */

FUNCTION fDebugMsg RETURNS CHARACTER 
	(INPUT ipcMessage AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE result AS CHARACTER NO-UNDO.
    IF lUseLogs THEN DO:
        OUTPUT STREAM sDebug CLOSE. OUTPUT STREAM sDebug TO VALUE(cDebugLog) append.
        PUT STREAM sDebug UNFORMATTED ipcMessage SKIP.
    END.
    RETURN result.
END FUNCTION.
