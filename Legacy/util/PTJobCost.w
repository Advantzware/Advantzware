&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE ttJobMat
    FIELD job-no             LIKE job.job-no
    FIELD job-no2            LIKE job.job-no2
    FIELD csr                LIKE job.user-id
    FIELD create-date        LIKE job.create-date
    FIELD start-date         LIKE job.start-date
    FIELD complete-date      LIKE job.complete-date
    FIELD est-no             LIKE job.est-no
    FIELD est-type           LIKE job.est-type
    FIELD est-desc           AS CHAR    FORMAT "x(8)"
    FIELD nbrComp            AS INT     FORMAT ">>>9"
    FIELD form-no            LIKE job-hdr.frm
    FIELD fg-item            LIKE job-hdr.i-no
    FIELD category           AS CHAR
    FIELD run-no             LIKE job-hdr.job-no2
    FIELD ord-no             LIKE job-hdr.ord-no
    FIELD make-qty           LIKE job-hdr.qty
    FIELD length             AS DECIMAL
    FIELD width              AS DECIMAL
    FIELD std-fix-cost       LIKE job-hdr.std-fix-cost
    FIELD std-lab-cost       LIKE job-hdr.std-lab-cost
    FIELD std-mat-cost       LIKE job-hdr.std-mat-cost
    FIELD std-tot-cost       LIKE job-hdr.std-tot-cost
    FIELD std-var-cost       LIKE job-hdr.std-var-cost
    FIELD fixOH              LIKE probe.fo-cost
    FIELD varOH              LIKE probe.vo-cost
    FIELD OHqty              LIKE probe.est-qty
    FIELD mat-qty            AS DECIMAL
    FIELD msf                AS DECIMAL
    FIELD board-msf          AS DECIMAL FORMAT "->,>>>,>>9.99<"
    FIELD board-cost         AS DECIMAL FORMAT "->,>>>,>>9.99<"
    FIELD non-board-cost     AS DECIMAL FORMAT "->,>>>,>>9.99<"
    FIELD order-no           LIKE oe-ord.ord-no
    FIELD customer           LIKE oe-ord.cust-no
    FIELD name               LIKE cust.name
    FIELD ord-line           AS INT     FORMAT ">>9"
    FIELD ord-line-item-no   LIKE oe-ordl.i-no
    FIELD ord-line-blank     LIKE oe-ordl.blank-no
    FIELD ord-line-price     LIKE oe-ordl.price
    FIELD ord-line-cost-uom  LIKE oe-ordl.cost
    FIELD ord-line-tot-cost  LIKE oe-ordl.t-cost
    FIELD ord-line-comm-pct  LIKE oe-ordl.s-comm
    FIELD ord-line-tot-frt   LIKE oe-ordl.t-freight
    FIELD ord-line-fix-oh    LIKE oe-ordl.fixoh
    FIELD ord-line-var-oh    LIKE oe-ordl.varoh
    FIELD ord-line-price-uom LIKE oe-ordl.pr-uom
    .
{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiCreateBegin fiCreateEnd Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiCreateBegin fiCreateEnd fiJob 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "Cancel" 
    SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "OK" 
    SIZE 15 BY 1.14.

DEFINE VARIABLE fiCreateBegin AS DATE      FORMAT "99/99/99":U 
    LABEL "Create Date Begin" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiCreateEnd   AS DATE      FORMAT "99/99/99":U 
    LABEL "Create Date End" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiJob         AS CHARACTER FORMAT "X(256)":U 
    LABEL "Processing Job" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
    fiCreateBegin AT ROW 1.71 COL 19 COLON-ALIGNED WIDGET-ID 2
    fiCreateEnd AT ROW 3.62 COL 19 COLON-ALIGNED WIDGET-ID 4
    fiJob AT ROW 5.52 COL 19 COLON-ALIGNED WIDGET-ID 6
    Btn_OK AT ROW 1.52 COL 49
    Btn_Cancel AT ROW 2.76 COL 49
    SPACE(1.19) SKIP(3.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "Job Cost Report"
    DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME L-To-R,COLUMNS                                            */
ASSIGN 
    FRAME gDialog:SCROLLABLE = FALSE
    FRAME gDialog:HIDDEN     = TRUE.

/* SETTINGS FOR FILL-IN fiJob IN FRAME gDialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Job Cost Report */
    DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
    DO:
        RUN ipRunReport.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Hide all frames. */
    HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     ENABLE the User Interface
      Parameters:  <none>
      Notes:       Here we display/view/enable the widgets in the
                   user-interface.  In addition, OPEN all queries
                   associated with each FRAME and BROWSE.
                   These statements here are based on the "Other 
                   Settings" section of the widget Property Sheets.
    ------------------------------------------------------------------------------*/
    DISPLAY fiCreateBegin fiCreateEnd fiJob 
        WITH FRAME gDialog.
    ENABLE fiCreateBegin fiCreateEnd Btn_OK Btn_Cancel 
        WITH FRAME gDialog.
    VIEW FRAME gDialog.
    {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
    /*------------------------------------------------------------------------------
      Purpose:     Super Override
      Parameters:  
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    RUN SUPER.

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        fiCreateBegin:screen-value IN FRAME gDialog = STRING(DATE(MONTH(TODAY),1,YEAR(TODAY)))
        fiCreateEnd:screen-value                    = STRING(TODAY).
    APPLY 'entry' TO fiCreateBegin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRunReport gDialog 
PROCEDURE ipRunReport :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
    DEF VAR chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
    DEF VAR chWorksheet        AS COMPONENT-HANDLE NO-UNDO.
    DEF VAR chHyper            AS COMPONENT-HANDLE NO-UNDO. 
    DEF VAR chFile             AS CHARACTER        NO-UNDO.
    DEF VAR iRow               AS INT              NO-UNDO.
    DEF VAR ihdrct             AS INT              NO-UNDO.

    CREATE "Excel.Application" chExcelApplication.
    IF SEARCH("template\PTjobCost.xlt") = ? THEN 
    DO:
        FILE-INFO:FILE-NAME = SEARCH("P:\ASI\Repositories\Advantzware\Resources\template\PTjobCost.xlt").
    END.
    ELSE 
    DO:
        FILE-INFO:FILE-NAME = SEARCH("template\PTjobCost.xlt").
    END.
    ASSIGN 
        chFile = SEARCH(FILE-INFO:FULL-PATHNAME).
  
    IF SEARCH (chFile) = ? THEN 
    DO:
        MESSAGE 
            'Spreadsheet File: ' FILE-INFO:FULL-PATHNAME
            'cannot be found. Please verify that the file exists.'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.

    ASSIGN
        chFile                            = FILE-INFO:FULL-PATHNAME
        chWorkbook                        = chExcelApplication:Workbooks:Open(chfile)
        chExcelApplication:VISIBLE        = FALSE
        chExcelApplication:DisplayAlerts  = FALSE
        chExcelApplication:ScreenUpdating = FALSE
        iRow                              = 3.
    chWorkbook:WorkSheets(1):Activate.
    chWorkSheet = chExcelApplication:Sheets:item(1).
    
    FOR EACH job NO-LOCK WHERE 
        job.company = "001" AND 
        job.create-date >= date(fiCreateBegin:screen-value IN FRAME gDialog) AND
        job.create-date <= date(fiCreateEnd:screen-value)
        BY job.create-date: 
    
        ASSIGN
            fiJob:screen-value = STRING(job.job-no)
            ihdrct             = 0.

        FIND FIRST est NO-LOCK WHERE
            est.company = job.company AND
            est.loc = job.loc AND
            est.est-no = job.est-no
            USE-INDEX est-no.
   
        FOR EACH job-hdr OF job NO-LOCK:
            ASSIGN 
                ihdrct = ihdrct + 1.
        END.

        CREATE ttJobMat. /* roll this up to job-hdr */
        ASSIGN
            ttJobMat.job-no        = job.job-no
            ttJobMat.job-no2       = job.job-no2 /* Run No */
            ttJobMat.csr           = job.user-id
            ttJobMat.create-date   = job.create-date
            ttJobMat.start-date    = job.start-date
            ttJobMat.complete-date = job.complete-date
            ttJobMat.est-no        = job.est-no
            ttJobMat.est-type      = est.est-type
            ttJobMat.nbrComp       = ihdrct
            .
  
        CASE ttJobMat.est-type:
            WHEN 1 THEN 
                ASSIGN 
                    ttJobMat.est-desc = "Single".
            WHEN 2 THEN 
                ASSIGN 
                    ttJobMat.est-desc = "2-Piece".
            WHEN 3 THEN 
                ASSIGN 
                    ttJobMat.est-desc = "Tandem".
            WHEN 4 THEN 
                ASSIGN 
                    ttJobMat.est-desc = "Combo".
            WHEN 5 THEN 
                ASSIGN 
                    ttJobMat.est-desc = "Single".
            WHEN 6 THEN 
                ASSIGN 
                    ttJobMat.est-desc = "Set".
        END CASE.
    
        IF ihdrct = 1 THEN 
        DO:
            FIND FIRST job-hdr OF job NO-LOCK NO-ERROR.
            IF AVAIL job-hdr THEN 
            DO:
                ASSIGN
                    ttJobMat.form-no      = job-hdr.frm
                    ttJobMat.fg-item      = job-hdr.i-no
                    ttJobMat.run-no       = job-hdr.job-no2
                    ttJobMat.ord-no       = job-hdr.ord-no
                    ttJobMat.make-qty     = job-hdr.qty
                    ttJobMat.std-fix-cost = job-hdr.std-fix-cost
                    ttJobMat.std-lab-cost = job-hdr.std-lab-cost
                    ttJobMat.std-mat-cost = job-hdr.std-mat-cost
                    ttJobMat.std-tot-cost = job-hdr.std-tot-cost
                    ttJobMat.std-var-cost = job-hdr.std-var-cost.
                FIND LAST probe NO-LOCK WHERE
                    probe.company = job.company AND
                    probe.est-no = job.est-no AND 
                    probe.est-qty EQ INTEGER(job-hdr.qty)
                    NO-ERROR.
                IF AVAIL probe THEN ASSIGN
                        ttJobMat.fixOH = probe.fo-cost
                        ttJobMat.varOH = probe.vo-cost
                        ttJobMat.OHqty = probe.est-qty.
            
            END.    
            FIND FIRST itemfg NO-LOCK WHERE
                itemfg.company = job.company AND
                itemfg.i-no = job-hdr.i-no
                NO-ERROR.
            ASSIGN
                ttJobMat.category = itemfg.procat.
        
            FOR EACH job-mat NO-LOCK WHERE
                job-mat.company = job-hdr.company AND
                job-mat.job-no = job-hdr.job-no AND
                job-mat.job-no2 = job-hdr.job-no2:
    
                FIND FIRST item NO-LOCK WHERE
                    item.company = job.company AND
                    item.i-no = job-mat.rm-i-no
                    NO-ERROR.
                IF item.mat-type = "B" THEN 
                DO:
                    ASSIGN
                        ttJobMat.length     = job-mat.len
                        ttJobMat.width      = job-mat.wid
                        ttJobMat.mat-qty    = job-mat.qty
                        ttJobMat.board-msf  = ttJobMat.board-msf + (job-mat.qty * job-mat.len * job-mat.wid / 144000)
                        ttJobMat.board-cost = ttJobMat.board-cost + job-mat.std-cost.
                END.
                ELSE 
                DO:
                    ASSIGN
                        ttJobMat.length         = 0
                        ttJobMat.width          = 0
                        ttJobMat.mat-qty        = job-mat.qty
                        ttJobMat.board-msf      = ttJobMat.board-msf + (job-mat.qty * job-mat.len * job-mat.wid / 144000)
                        ttJobMat.non-board-cost = ttJobMat.non-board-cost + job-mat.std-cost.
                END.
            END.
        END.
        ELSE 
        DO:
            FOR EACH job-hdr OF job:
                ASSIGN
                    ttJobMat.fg-item      = "MULT"
                    ttJobMat.ord-no       = job-hdr.ord-no
                    ttJobMat.make-qty     = ttJobMat.make-qty + job-hdr.qty
                    ttJobMat.std-fix-cost = ttJobMat.std-fix-cost + job-hdr.std-fix-cost
                    ttJobMat.std-lab-cost = ttJobMat.std-lab-cost + job-hdr.std-lab-cost
                    ttJobMat.std-mat-cost = ttJobMat.std-mat-cost + job-hdr.std-mat-cost
                    ttJobMat.std-tot-cost = ttJobMat.std-tot-cost + job-hdr.std-tot-cost
                    ttJobMat.std-var-cost = ttJobMat.std-var-cost + job-hdr.std-var-cost.
            END.
    
            FOR EACH job-mat NO-LOCK WHERE
                job-mat.company = job.company AND
                job-mat.job-no = job.job-no AND
                job-mat.job-no2 = job.job-no2:
    
                FIND FIRST item NO-LOCK WHERE
                    item.company = job.company AND
                    item.i-no = job-mat.rm-i-no
                    NO-ERROR.
                
                IF item.mat-type = "B" THEN 
                DO:
                    ASSIGN
                        ttJobMat.length     = job-mat.len
                        ttJobMat.width      = job-mat.wid
                        ttJobMat.mat-qty    = job-mat.qty
                        ttJobMat.board-msf  = ttJobMat.board-msf + (job-mat.qty * job-mat.len * job-mat.wid / 144000)
                        ttJobMat.board-cost = ttJobMat.board-cost + job-mat.std-cost.
                END.
                ELSE 
                DO:
                    ASSIGN
                        ttJobMat.length         = 0
                        ttJobMat.width          = 0
                        ttJobMat.mat-qty        = job-mat.qty
                        ttJobMat.board-msf      = ttJobMat.board-msf + (job-mat.qty * job-mat.len * job-mat.wid / 144000)
                        ttJobMat.non-board-cost = ttJobMat.non-board-cost + job-mat.std-cost.
                END.
            END.
        END.

        FIND FIRST job-hdr OF job NO-LOCK NO-ERROR.
        FIND FIRST itemfg NO-LOCK WHERE
            itemfg.company = job.company AND
            itemfg.i-no = job-hdr.i-no
            NO-ERROR.
        IF AVAILABLE itemfg THEN 
            ASSIGN 
                ttJobMat.category           = itemfg.procat
                ttJobMat.ord-line-price     = itemfg.sell-price
                ttJobMat.ord-line-price-uom = itemfg.sell-uom
                .
        FIND oe-ord NO-LOCK WHERE
            oe-ord.company = job.company AND
            oe-ord.ord-no = job-hdr.ord-no
            NO-ERROR.
        
        IF AVAIL oe-ord THEN 
        DO:
            FIND cust NO-LOCK WHERE
                cust.company = oe-ord.company AND
                cust.cust-no = oe-ord.cust-no
                NO-ERROR.
            
            FIND oe-ordl NO-LOCK WHERE
                oe-ordl.company = oe-ord.company AND
                oe-ordl.ord-no = oe-ord.ord-no AND
                oe-ordl.i-no = job-hdr.i-no AND
                oe-ordl.job-no = job-hdr.job-no AND
                oe-ordl.job-no2 = job-hdr.job-no2 AND
                oe-ordl.qty = job-hdr.qty
                NO-ERROR.
            
            IF NOT AVAIL oe-ordl THEN FIND oe-ordl NO-LOCK WHERE
                    oe-ordl.company = oe-ord.company AND
                    oe-ordl.ord-no = oe-ord.ord-no AND
                    oe-ordl.i-no = job-hdr.i-no AND
                    oe-ordl.job-no = job-hdr.job-no AND
                    oe-ordl.job-no2 = job-hdr.job-no2
                    NO-ERROR.
            IF NOT AVAIL oe-ordl THEN FIND oe-ordl NO-LOCK WHERE
                    oe-ordl.company = oe-ord.company AND
                    oe-ordl.ord-no = oe-ord.ord-no AND
                    oe-ordl.i-no = job-hdr.i-no AND
                    oe-ordl.job-no = job-hdr.job-no
                    NO-ERROR.
        
            IF AVAIL oe-ordl THEN 
            DO:
                ASSIGN
                    ttJobMat.order-no           = oe-ord.ord-no
                    ttJobMat.ord-line           = oe-ordl.line
                    ttJobMat.ord-line-item-no   = oe-ordl.i-no
                    ttJobMat.ord-line-price     = oe-ordl.price
                    ttJobMat.ord-line-price-uom = oe-ordl.pr-uom
                    ttJobMat.ord-line-cost-uom  = oe-ordl.cost
                    ttJobMat.ord-line-tot-cost  = oe-ordl.t-cost
                    ttJobMat.ord-line-comm-pct  = oe-ordl.s-comm
                    ttJobMat.ord-line-tot-frt   = oe-ordl.t-freight
                    ttJobMat.ord-line-fix-oh    = oe-ordl.fixoh
                    ttJobMat.ord-line-var-oh    = oe-ordl.varoh
                    ttJobMat.customer           = oe-ord.cust-no
                    ttJobMat.name               = cust.name.
            END.
        END.
        PROCESS EVENTS.
    END.
                    
    FOR EACH ttJobMat:
        ASSIGN
            chWorkSheet:Range("A" + STRING(iRow)):VALUE  = ttJobMat.job-no        
            chWorkSheet:Range("B" + STRING(iRow)):VALUE  = ttJobMat.job-no2
            chWorkSheet:Range("C" + STRING(iRow)):VALUE  = ttJobMat.csr       
            chWorkSheet:Range("D" + STRING(iRow)):VALUE  = ttJobMat.start-date    
            chWorkSheet:Range("E" + STRING(iRow)):VALUE  = ttJobMat.create-date   
            chWorkSheet:Range("F" + STRING(iRow)):VALUE  = ttJobMat.complete-date 

            chWorkSheet:Range("G" + STRING(iRow)):VALUE  = ttJobMat.est-no        
            chWorkSheet:Range("H" + STRING(iRow)):VALUE  = ttJobMat.est-type      
            chWorkSheet:Range("I" + STRING(iRow)):VALUE  = ttJobMat.est-desc      

            chWorkSheet:Range("J" + STRING(iRow)):VALUE  = ttJobMat.nbrComp       
            chWorkSheet:Range("K" + STRING(iRow)):VALUE  = ttJobMat.fg-item       
            chWorkSheet:Range("L" + STRING(iRow)):VALUE  = ttJobMat.category       
            chWorkSheet:Range("M" + STRING(iRow)):VALUE  = ttJobMat.make-qty      
            chWorkSheet:Range("N" + STRING(iRow)):VALUE  = ttJobMat.std-fix-cost  
            chWorkSheet:Range("O" + STRING(iRow)):VALUE  = ttJobMat.std-lab-cost  
            chWorkSheet:Range("P" + STRING(iRow)):VALUE  = ttJobMat.std-mat-cost  
            chWorkSheet:Range("Q" + STRING(iRow)):VALUE  = ttJobMat.std-tot-cost  
            chWorkSheet:Range("R" + STRING(iRow)):VALUE  = ttJobMat.std-var-cost 
            chWorkSheet:Range("S" + STRING(iRow)):VALUE  = ttJobMat.fixOH 
            chWorkSheet:Range("T" + STRING(iRow)):VALUE  = ttJobMat.varOH
            chWorkSheet:Range("U" + STRING(iRow)):VALUE  = ttJobMat.OHqty
            chWorkSheet:Range("V" + STRING(iRow)):VALUE  = ttJobMat.board-msf
            chWorkSheet:Range("W" + STRING(iRow)):VALUE  = ttJobMat.board-cost
            chWorkSheet:Range("X" + STRING(iRow)):VALUE  = ttJobMat.non-board-cost
            chWorkSheet:Range("Y" + STRING(iRow)):VALUE  = ttJobMat.order-no     
            chWorkSheet:Range("Z" + STRING(iRow)):VALUE  = ttJobMat.customer      
            chWorkSheet:Range("AA" + STRING(iRow)):VALUE = ttJobMat.name          
            chWorkSheet:Range("AB" + STRING(iRow)):VALUE = ttJobMat.ord-line      
            chWorkSheet:Range("AC" + STRING(iRow)):VALUE = ttJobMat.ord-line-price
            chWorkSheet:Range("AD" + STRING(iRow)):VALUE = ttJobMat.ord-line-price-uom
            chWorkSheet:Range("AE" + STRING(iRow)):VALUE = ttJobMat.ord-line-cost-uom
            chWorkSheet:Range("AF" + STRING(iRow)):VALUE = ttJobMat.ord-line-tot-cost
            chWorkSheet:Range("AG" + STRING(iRow)):VALUE = ttJobMat.ord-line-comm-pct
            chWorkSheet:Range("AH" + STRING(iRow)):VALUE = ttJobMat.ord-line-tot-frt
            chWorkSheet:Range("AI" + STRING(iRow)):VALUE = ttJobMat.ord-line-fix-oh
            chWorkSheet:Range("AJ" + STRING(iRow)):VALUE = ttJobMat.ord-line-var-oh
            iRow                                         = iRow + 1.
    END.

    ASSIGN
        chExcelApplication:VISIBLE        = TRUE
        chExcelApplication:ScreenUpdating = TRUE.
    chWorkbook:WorkSheets(1):Columns("A:AH"):AutoFit.
    chExcelApplication:VISIBLE = TRUE.

    RELEASE OBJECT chWorkbook         NO-ERROR.
    RELEASE OBJECT chWorkSheet        NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

