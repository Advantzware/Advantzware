&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/AmazonInvoice.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.
 
DEFINE TEMP-TABLE ttInv
  FIELD iInvNo AS INT 
  INDEX byInvNo iInvNo
  .
  
DEFINE VARIABLE cInvNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTempInvoice AS INTEGER NO-UNDO.
DEFINE STREAM s1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fiCsvFile btSelectFile ~
tbOverwriteEDI btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiCsvFile tbOverwriteEDI 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 12 BY .76.

DEFINE BUTTON btn-process 
     LABEL "&Generate 810 Invoice" 
     SIZE 24 BY .76.

DEFINE BUTTON btSelectFile 
     LABEL "Select CSV File" 
     SIZE 35 BY 1.14.

DEFINE VARIABLE fiCsvFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "CSV File" 
     VIEW-AS FILL-IN 
     SIZE 81.8 BY 1 TOOLTIP "Enter the nameof a csv file where inovice number is first" NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 6.91.

DEFINE VARIABLE tbOverwriteEDI AS LOGICAL INITIAL no 
     LABEL "Overwrite Existing EDI data?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiCsvFile AT ROW 3.86 COL 8.2 COLON-ALIGNED WIDGET-ID 6
     btSelectFile AT ROW 5.29 COL 16 WIDGET-ID 8
     tbOverwriteEDI AT ROW 7.57 COL 6 WIDGET-ID 4
     btn-process AT ROW 9.1 COL 14
     btn-cancel AT ROW 9.1 COL 47
     "Selection Parameters" VIEW-AS TEXT
          SIZE 28 BY .43 AT ROW 2.19 COL 5
     "" VIEW-AS TEXT
          SIZE 1.4 BY .62 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "(Check this only if instructed to)" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 7.48 COL 41 WIDGET-ID 10
     "Note:  The CSV fiile must have the invoice number as the first column" VIEW-AS TEXT
          SIZE 72 BY .62 AT ROW 6.48 COL 11 WIDGET-ID 12
          FGCOLOR 12 
     RECT-17 AT ROW 1.71 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.2 BY 10.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Generate EDI Invoice"
         HEIGHT             = 10.19
         WIDTH              = 107
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generate EDI Invoice */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generate EDI Invoice */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Generate 810 Invoice */
DO:
  DEF VAR v-process AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
          " for the specified invoice number?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelectFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelectFile C-Win
ON CHOOSE OF btSelectFile IN FRAME FRAME-A /* Select CSV File */
DO:
    DEFINE VARIABLE cFilePath AS CHARACTER NO-UNDO.

    SYSTEM-DIALOG GET-FILE cFilePath
        TITLE "Select CSV file of invoice numbers".

    fiCSVFile:SCREEN-VALUE = cFilePath.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO fiCSVFile.
  END.
  WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fiCsvFile tbOverwriteEDI 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fiCsvFile btSelectFile tbOverwriteEDI btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckMissingBuyer C-Win 
PROCEDURE pCheckMissingBuyer :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cShipto AS CHAR.
            IF AVAIL ar-inv THEN             
               cShipTo = ar-inv.ship-id.
            else
               cShipto = IF inv-head.sold-no NE "" THEN inv-head.sold-no ELSE inv-head.bill-to.
        FIND FIRST edshipto NO-LOCK
            WHERE edshipto.partner = edcode.partner
            AND edshipto.ship-to = cShipTo
            AND edshipto.ref-type = "BY"
            NO-ERROR.

        IF AVAIL eddoc THEN 
        DO:
            FIND FIRST edivtran OF eddoc. 
            IF NOT AVAIL edivtran THEN 
              RETURN.
              
            FIND FIRST EDShipto NO-LOCK WHERE 
                EDShipto.Partner EQ EDIVTran.partner
                AND EDShipto.ship-to EQ EDIVTran.By-code 
                AND EDShipto.Ref-type EQ "BY"
                NO-ERROR.

             
            IF edivtran.by-code EQ "" THEN do:
                edivtran.by-code = cShipTo.
            FIND FIRST EDShipto NO-LOCK WHERE 
                EDShipto.Partner EQ EDIVTran.partner
                AND EDShipto.ship-to EQ EDIVTran.By-code 
                AND EDShipto.Ref-type EQ "BY"
                NO-ERROR.                
            END.
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ (IF AVAIL ar-inv THEN ar-inv.company ELSE inv-head.company)
                AND shipto.ship-id EQ edivtran.by-code
                AND shipto.cust-no EQ (IF AVAIL ar-inv THEN ar-inv.cust-no ELSE inv-head.cust-no)
                NO-ERROR.
          
            IF AVAIL shipto AND NOT AVAIL edshipto THEN 
            DO:
                CREATE edshipto.
                 
                ASSIGN
                    edshipto.partner  = edivtran.partner
                    edshipto.ref-type = "BY"
                    edshipto.by-code  = edivtran.by-code    /* partner must assign this */
                    edshipto.cust     = edivtran.cust
                    edshipto.ship-to  = edivtran.by-code
                    .
                ASSIGN
                    edshipto.name    = shipto.ship-name
                    edshipto.addr1   = shipto.ship-addr[1]
                    edshipto.addr2   = shipto.ship-addr[2]
                    edshipto.city    = shipto.ship-city
                    edshipto.state   = shipto.ship-state
                    edshipto.zip     = shipto.ship-zip
                    edshipto.country = "US"
                    .          
                   //  message "edshipto created" view-as alert-box.
            END.           

      END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
SESSION:SET-WAIT-STATE("General").


STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").




/* ***************************  Definitions  ************************** */



/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name    AS cha       NO-UNDO.
DEFINE VARIABLE init-dir     AS CHA       NO-UNDO.
DEFINE VARIABLE lv-comp-curr AS cha       NO-UNDO.
DEFINE VARIABLE lv-audit-dir AS CHARACTER NO-UNDO.

/* {methods/defines/hndldefs.i} */
/* {methods/prgsecur.i} */

{custom/gcompany.i}
{custom/gloc.i}

 
DEFINE VARIABLE lDeleteOrig AS LOGICAL NO-UNDO.
IF gCompany EQ "" THEN 
  gcompany = '001'.
IF gLoc EQ "" THEN 
gloc = "MAIN".



ASSIGN
    cocode = gcompany
    locode = gloc.

FIND FIRST company WHERE company.company = gcompany NO-LOCK NO-ERROR.
IF AVAILABLE company THEN lv-comp-curr = company.curr-code.


RUN rc/genrcvar.p. 

ASSIGN 
       lDeleteOrig = tbOverwriteEDI
       .
IF SEARCH(fiCSVFile) NE ? THEN DO:
    INPUT STREAM s1 FROM value(fiCSVFile).
    REPEAT:
          IMPORT STREAM s1 DELIMITER "," cInvNo.
          iTempInvoice = INTEGER(cInvNo) NO-ERROR. 
          IF ERROR-STATUS:ERROR THEN 
            NEXT.
          CREATE ttInv.
          ASSIGN ttInv.iInvNo = iTempInvoice.
    END.
    INPUT STREAM s1 CLOSE.
END.       

MAIN:
FOR EACH ttInv:

    IF ttInv.iInvNo EQ 0 THEN 
        LEAVE MAIN.
       
   
    FIND FIRST ar-inv WHERE ar-inv.company EQ cocode 
        AND ar-inv.inv-no EQ ttInv.iInvNo
        NO-ERROR.
    IF NOT AVAILABLE ar-inv THEN 
        FIND FIRST inv-head WHERE inv-head.company EQ cocode 
            AND inv-head.inv-no EQ ttInv.iInvNo
            NO-ERROR.
    IF NOT AVAILABLE ar-inv AND NOT AVAILABLE inv-head THEN 
    DO:
       NEXT MAIN.
    END. 
    
    IF lDeleteOrig THEN 
    DO:
        FOR EACH edivtran EXCLUSIVE-LOCK 
          WHERE edivtran.company EQ cocode
            AND edivtran.invoice-no EQ string(IF AVAILABLE ar-inv THEN ar-inv.inv-no ELSE inv-head.inv-no)
          :

            FOR EACH edivaddon EXCLUSIVE-LOCK
                WHERE edivaddon.seq = edivtran.seq
                  AND edivaddon.partner EQ edivtran.partner:
                DELETE edivaddon.
            END.
            FOR EACH edivline EXCLUSIVE-LOCK 
                WHERE edivline.seq = edivtran.seq
                  AND edivline.partner = edivtran.partner:
                DELETE edivline.
              
            END.
            FIND FIRST eddoc EXCLUSIVE-LOCK 
               WHERE eddoc.seq EQ edivtran.seq 
                 AND eddoc.partner EQ edivtran.partner
               NO-ERROR.
            IF AVAILABLE eddoc THEN 
                DELETE eddoc.
            DELETE edivtran.
        END.
    END.
    ELSE DO: 
        FIND FIRST edivtran NO-LOCK 
            WHERE edivtran.company EQ cocode
              AND edivtran.invoice-no EQ string(IF AVAILABLE ar-inv THEN ar-inv.inv-no ELSE inv-head.inv-no)
            NO-ERROR.   
        IF AVAILABLE edivtran THEN      
            FIND FIRST eddoc EXCLUSIVE-LOCK 
                WHERE eddoc.seq EQ edivtran.seq 
                AND eddoc.partner EQ edivtran.partner
                NO-ERROR.
        IF AVAILABLE eddoc THEN 
          eddoc.posted = NO.
        
    END.
    FIND FIRST eddoc NO-LOCK 
        WHERE eddoc.seq EQ edivtran.seq 
          AND eddoc.partner EQ edivtran.partner
        NO-ERROR.
    RUN pCheckMissingBuyer.
    
    /* Create eddoc for invoice if required */
    FIND FIRST edivtran NO-LOCK 
       WHERE edivtran.company EQ cocode
         AND edivtran.invoice-no EQ string(IF AVAILABLE ar-inv THEN ar-inv.inv-no ELSE inv-head.inv-no)
       NO-ERROR.
    IF NOT AVAIL edivtran THEN 
        RUN ed/asi/o810hook.p (IF AVAILABLE(ar-inv) THEN RECID(ar-inv) ELSE RECID(inv-head), NO, NO).  
    FIND FIRST edivtran NO-LOCK 
    WHERE edivtran.company EQ cocode
      AND edivtran.invoice-no EQ string(IF AVAILABLE ar-inv THEN ar-inv.inv-no ELSE inv-head.inv-no)
    NO-ERROR. 
    
    if avail edivtran then 
    FIND FIRST eddoc exclusive-lock
    WHERE eddoc.seq EQ edivtran.seq 
      AND eddoc.partner EQ edivtran.partner
    NO-ERROR.
    
    IF AVAILABLE eddoc THEN 
      eddoc.posted = NO.
          
    release eddoc.    
    
    FIND FIRST edmast NO-LOCK
        WHERE edmast.cust EQ (IF AVAILABLE (ar-inv) THEN ar-inv.cust-no ELSE inv-head.cust-no)
        NO-ERROR.
    IF AVAILABLE edmast THEN 
    DO: 
        FIND FIRST edcode NO-LOCK
            WHERE edcode.partner EQ edmast.partner
            NO-ERROR.
        IF NOT AVAILABLE edcode THEN 
            FIND FIRST edcode NO-LOCK
                WHERE edcode.partner EQ edmast.partnerGrp
                NO-ERROR.
    END.  
    
    IF AVAILABLE edcode AND edcode.sendFileOnPrint THEN 
    DO:
       
        RUN ed/asi/write810.p (INPUT cocode, INPUT (IF AVAILABLE ar-inv THEN ar-inv.inv-no ELSE inv-head.inv-no)).
    END.
END. /* MainLoop */

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
APPLY "close" TO THIS-PROCEDURE.

RETURN NO-APPLY.

/* end ---------------------------------- copr. 2006  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

