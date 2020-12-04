&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\po-purge.w

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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.
DEFINE VARIABLE hPgmSecurity                 AS HANDLE    NO-UNDO.
DEFINE VARIABLE lResult                      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cLocation                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPurge                       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lIsReceiptOrInvoiceAvailable AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hdOutputProcs                AS HANDLE    NO-UNDO.

DEFINE STREAM sPoOrd.
DEFINE STREAM sPoOrdl.
DEFINE STREAM sPoAll.
DEFINE STREAM sPoOrdlAdd.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

/* Need to undo the records in case of any error, so defined temp tables without No-Undo */

DEFINE TEMP-TABLE ttPoOrdl 
    LIKE po-ordl.

DEFINE TEMP-TABLE ttPoOrdlAdd
    LIKE po-ordl-add.

DEFINE TEMP-TABLE ttPoAll
    LIKE po-all.    

DEFINE TEMP-TABLE ttPoOrd
    LIKE po-ord.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_po end_po begin_date end_date ~
fiBeginVend fiEndVend tbPurgeLinkedPO btSimulate btn-process btn-cancel ~
fiDirectory 
&Scoped-Define DISPLAYED-OBJECTS begin_po end_po begin_date end_date ~
fiBeginVend fiEndVend tbPurgeLinkedPO fiDirectory 

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
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btSimulate 
     LABEL "Simulate Purge" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning PO Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Beginning PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending PO Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Ending PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeginVend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Begining Vendor" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiDirectory AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 56 BY .71 NO-UNDO.

DEFINE VARIABLE fiEndVend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ending Vendor" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 10.48.

DEFINE VARIABLE tbPurgeLinkedPO AS LOGICAL INITIAL no 
     LABEL "Purge PO's even if linked to invoices or receipts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 50.6 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_po AT ROW 7.43 COL 19.4 COLON-ALIGNED HELP
          "Enter Beginning Purchase Order Number"
     end_po AT ROW 7.43 COL 59.4 COLON-ALIGNED HELP
          "Enter Ending Purchase Order Number"
     begin_date AT ROW 9.1 COL 19.4 COLON-ALIGNED HELP
          "Enter Beginning Purchase Order Number"
     end_date AT ROW 9.1 COL 59.4 COLON-ALIGNED HELP
          "Enter Ending Purchase Order Number"
     fiBeginVend AT ROW 10.76 COL 19.4 COLON-ALIGNED WIDGET-ID 4
     fiEndVend AT ROW 10.76 COL 59.4 COLON-ALIGNED WIDGET-ID 6
     tbPurgeLinkedPO AT ROW 11.91 COL 21.4 WIDGET-ID 2
     btSimulate AT ROW 15.52 COL 14.2 WIDGET-ID 8
     btn-process AT ROW 15.52 COL 37.6
     btn-cancel AT ROW 15.52 COL 61
     fiDirectory AT ROW 13.86 COL 12.4 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     "Records to view or recover purged information will be stored in directory:" VIEW-AS TEXT
          SIZE 70 BY .62 AT ROW 13.24 COL 14.2 WIDGET-ID 10
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.2 BY 17.52.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Purge Purchase Orders"
         HEIGHT             = 17.71
         WIDTH              = 90.2
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Purchase Orders */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Purchase Orders */
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
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
    lPurge = YES.
    run run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimulate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimulate C-Win
ON CHOOSE OF btSimulate IN FRAME FRAME-A /* Simulate Purge */
DO:
  lPurge = No.
  RUN run-process.
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
  /* check security */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.   
  
  RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
  RUN epCanAccess IN hPgmSecurity ("util/po-purge.w", "", OUTPUT lResult).
  DELETE OBJECT hPgmSecurity.
  IF NOT lResult THEN DO:
     MESSAGE "Access is denied." VIEW-AS ALERT-BOX .
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
  
  RUN FileSys_GetTempDirectory(
      OUTPUT cLocation
      ).
  cLocation = cLocation + "\POPurge-" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")+ STRING(DAY(TODAY),"99") + "-" + STRING(TIME,"99999"). 
 
  fiDirectory = cLocation.   
  RUN enable_UI.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
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
  DISPLAY begin_po end_po begin_date end_date fiBeginVend fiEndVend 
          tbPurgeLinkedPO fiDirectory 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_po end_po begin_date end_date fiBeginVend fiEndVend 
         tbPurgeLinkedPO btSimulate btn-process btn-cancel fiDirectory 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteOrphanRecord C-Win 
PROCEDURE pDeleteOrphanRecord PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes: Deletes records having blank company or no parent/header record.
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBeginPo     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiEndPo       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcBeginRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndRecKey   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl-add FOR po-ordl-add.
    DEFINE BUFFER bf-po-all      FOR po-all.

    FOR EACH bf-po-ordl-add NO-LOCK 
        WHERE(bf-po-ordl-add.company EQ ipcCompany OR bf-po-ordl-add.company EQ "")
          AND bf-po-ordl-add.po-no   GE ipiBeginPo
          AND bf-po-ordl-add.po-no   LE ipiEndPo:
        IF(DYNAMIC-FUNCTION("sfGetRecKeyPrefix",bf-po-ordl-add.rec_key)     GE ipcBeginRecKey
           AND DYNAMIC-FUNCTION("sfGetRecKeyPrefix",bf-po-ordl-add.rec_key) LE ipcEndRecKey)
           AND(bf-po-ordl-add.company EQ "" OR 
           NOT CAN-FIND(FIRST po-ord 
                        WHERE po-ord.company EQ bf-po-ordl-add.company
                          AND po-ord.po-no   EQ bf-po-ordl-add.po-no)) THEN DO:
           CREATE ttPoOrdlAdd.
           BUFFER-COPY bf-po-ordl-add TO ttPoOrdlAdd.                   
           IF lPurge THEN DO:  
               EXPORT STREAM sPoOrdlAdd bf-po-ordl-add.                  
               FIND CURRENT bf-po-ordl-add EXCLUSIVE-LOCK NO-ERROR.
               DELETE bf-po-ordl-add.
           END.
        END.   
    END.                                            
                         
    FOR EACH bf-po-all NO-LOCK 
        WHERE (bf-po-all.company EQ ipcCompany OR bf-po-all.company EQ "")
          AND bf-po-all.po-no    GE ipiBeginPo
          AND bf-po-all.po-no    LE ipiEndPo: 
        IF(DYNAMIC-FUNCTION("sfGetRecKeyPrefix",bf-po-all.rec_key)     GE ipcBeginRecKey
           AND DYNAMIC-FUNCTION("sfGetRecKeyPrefix",bf-po-all.rec_key) LE ipcEndRecKey)
           AND(bf-po-all.company EQ "" OR 
              NOT CAN-FIND(FIRST po-ord 
                            WHERE po-ord.company EQ bf-po-all.company
                              AND po-ord.po-no   EQ bf-po-all.po-no)) THEN DO: 
           CREATE ttPoAll.
           BUFFER-COPY bf-Po-All TO ttPoAll.
           IF lPurge THEN DO:  
               EXPORT STREAM sPoAll bf-po-all.                
               FIND CURRENT bf-po-all EXCLUSIVE-LOCK NO-ERROR.
               DELETE bf-po-all. 
           END.
        END.                     
    END.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/******************************************************* util/cln-po.p 10/96  */
/* Purge PO Program                                                           */
/******************************************************************************/

DISABLE TRIGGERS FOR LOAD OF po-ord.
DISABLE TRIGGERS FOR LOAD OF po-ordl.

DEFINE BUFFER b-ref1         FOR reftable.
DEFINE BUFFER b-ref2         FOR reftable.
DEFINE BUFFER bf-po-ordl-add FOR po-ordl-add.
DEFINE BUFFER bf-po-all      FOR po-all.

DEFINE VARIABLE fpo LIKE po-ord.po-no INITIAL 0 NO-UNDO.
DEFINE VARIABLE tpo LIKE fpo INITIAL 999999 NO-UNDO.
DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBeginRecKeyPrefix AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndRecKeyPrefix   AS CHARACTER NO-UNDO.
  
SESSION:SET-WAIT-STATE("General").

DO WITH FRAME {&frame-name}:
  ASSIGN
   begin_po
   end_po
   begin_date
   end_date
   tbPurgeLinkedPO
   fiBeginVend
   fiEndVend
   .
END.

SESSION:SET-WAIT-STATE("").

ASSIGN
    fpo                = begin_po
    tpo                = end_po
    v-process          = NO
    cBeginRecKeyPrefix = STRING(YEAR(begin_date),"9999") +
                         STRING(MONTH(begin_date),"99")  +
                         STRING(DAY(begin_date),"99")
    cEndRecKeyPrefix   = STRING(YEAR(end_date),"9999") +
                         STRING(MONTH(end_date),"99")  +
                         STRING(DAY(end_date),"99")    
                         .

IF lPurge THEN 
    MESSAGE "Are you sure you want to delete the purchase orders within the " +
        "selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
IF v-process OR NOT lPurge THEN DO:  
  RUN FileSys_CreateDirectory(
     INPUT  cLocation,
     OUTPUT lSuccess,
     OUTPUT cMessage
     ).   
  IF lPurge THEN DO:
      OUTPUT STREAM sPoOrd     TO VALUE (cLocation + "/Po-ord.d")      APPEND.
      OUTPUT STREAM sPoOrdl    TO VALUE (cLocation + "/Po-ordl.d")     APPEND.
      OUTPUT STREAM sPoAll     TO VALUE (cLocation + "/Po-All.d")      APPEND.
      OUTPUT STREAM sPoOrdlAdd TO VALUE (cLocation + "/Po-ordl-add.d") APPEND.
  END.    
  MAINBLOCK: 
  FOR EACH po-ord
      WHERE po-ord.company EQ cocode
        AND po-ord.po-no   GE fpo
        AND po-ord.po-no   LE tpo
        AND po-ord.po-date GE begin_date
        AND po-ord.po-date LE end_date
        AND po-ord.vend-no GE fiBeginVend
        AND po-ord.vend-no LE fiEndVend: 
                    
    FOR EACH bf-po-ordl-add NO-LOCK
        WHERE bf-po-ordl-add.company EQ po-ord.company
          AND bf-po-ordl-add.po-no   EQ po-ord.po-no:
        CREATE ttPoOrdlAdd.
        BUFFER-COPY bf-po-ordl-add TO ttpoOrdlAdd.  
        IF tbPurgeLinkedPO AND lPurge THEN DO: 
            FIND CURRENT bf-po-ordl-add EXCLUSIVE-LOCK NO-ERROR.  
            DELETE bf-po-ordl-add. 
        END.      
    END.
    FOR EACH bf-po-all NO-LOCK
        WHERE bf-po-all.company EQ po-ord.company
          AND bf-po-all.po-no   EQ po-ord.po-no:
        CREATE ttPoAll.
        BUFFER-COPY bf-po-all TO ttPoAll.
        IF tbPurgeLinkedPO AND lPurge THEN DO:
            FIND CURRENT bf-po-all EXCLUSIVE-LOCK NO-ERROR.     
            DELETE bf-po-all. 
        END.                  
    END.                      
    FOR EACH po-ordl 
        WHERE po-ordl.company EQ po-ord.company 
          AND po-ordl.po-no   EQ po-ord.po-no:
      {po/po-ordls.i}
      IF AVAIL b-ref1 AND lPurge THEN 
          DELETE b-ref1.
      IF AVAIL b-ref2 AND lPurge THEN 
          DELETE b-ref2.
      
      IF NOT tbPurgeLinkedPO THEN DO:
          IF lPurge THEN DO:
              RUN po/del-po-ordl.p(
                  BUFFER po-ordl,
                  INPUT  NO /* Supress the messages */
                  )NO-ERROR.   
              IF ERROR-STATUS:ERROR THEN
                  UNDO MAINBLOCK,NEXT MAINBLOCK.     
          END. 
          ELSE DO:          
              IF po-ordl.t-rec-qty NE 0 OR po-ordl.t-inv-qty NE 0 THEN 
                  lIsReceiptOrInvoiceAvailable = YES.
              
              IF NOT lIsReceiptOrInvoiceAvailable THEN
                  FOR EACH ap-inv
                      WHERE ap-inv.company EQ po-ordl.company
                        AND ap-inv.posted  EQ NO
                      NO-LOCK,
                      EACH ap-invl
                      WHERE ap-invl.i-no EQ ap-inv.i-no
                        AND ap-invl.line EQ ((po-ordl.po-no * 1000) + po-ordl.line)
                      NO-LOCK:
                    lIsReceiptOrInvoiceAvailable = YES.
                    LEAVE.
                  END.
              
              IF NOT lIsReceiptOrInvoiceAvailable THEN
                IF po-ordl.item-type THEN
                  lIsReceiptOrInvoiceAvailable = CAN-FIND(FIRST rm-rctd
                                WHERE rm-rctd.company    EQ po-ordl.company
                                  AND rm-rctd.rita-code  EQ "R"
                                  AND rm-rctd.i-no       EQ po-ordl.i-no
                                  AND INT(rm-rctd.po-no) EQ po-ordl.po-no
                                USE-INDEX rita-code) NO-ERROR.
                ELSE
                  lIsReceiptOrInvoiceAvailable = CAN-FIND(FIRST fg-rctd
                                WHERE fg-rctd.company    EQ po-ordl.company
                                  AND fg-rctd.rita-code  EQ "R"
                                  AND fg-rctd.i-no       EQ po-ordl.i-no
                                  AND INT(fg-rctd.po-no) EQ po-ordl.po-no
                                USE-INDEX rita-code) NO-ERROR.
              
              IF lIsReceiptOrInvoiceAvailable THEN 
                  UNDO MainBlock, NEXT MainBLOCK.  
          END.           
      END.                    
      CREATE ttPoOrdl.
      BUFFER-COPY po-ordl TO ttPoOrdl. 
      IF lPurge THEN 
        DELETE po-ordl.   
    END.
    CREATE ttpoOrd.
    BUFFER-COPY po-ord TO ttPoOrd.         
    IF lPurge THEN
        DELETE po-ord.      
  END.
  RUN pDeleteOrphanRecord(
      INPUT cocode,
      INPUT begin_po,
      INPUT end_po,
      INPUT cBeginRecKeyPrefix,
      INPUT cEndRecKeyPrefix
      ).
  IF lPurge THEN 
    FOR EACH ttPoOrd:
        EXPORT STREAM sPoOrd ttPoOrd.
    END.  
  RUN Output_TempTableToCSV IN hdOutputProcs (
      INPUT TEMP-TABLE ttPoOrd:HANDLE,
      INPUT cLocation + "\Po-ord.csv",
      INPUT TRUE,  /* Export Header */
      INPUT FALSE, /* Auto increment File name */
      OUTPUT lSuccess,
      OUTPUT cMessage
      ).        
  IF lPurge THEN 
      FOR EACH ttPoOrdl:
          EXPORT STREAM sPoOrdl ttpoOrdl.    
      END. 
  RUN Output_TempTableToCSV IN hdOutputProcs (
      INPUT TEMP-TABLE ttPoOrdl:HANDLE,
      INPUT cLocation + "\Po-ordl.csv",
      INPUT TRUE,  /* Export Header */
      INPUT FALSE, /* Auto increment File name */
      OUTPUT lSuccess,
      OUTPUT cMessage
      ).   
  IF lPurge THEN     
      FOR EACH ttPoOrdlAdd:
          EXPORT STREAM sPoOrdlAdd ttPoOrdlAdd. 
      END.
  RUN Output_TempTableToCSV IN hdOutputProcs (
      INPUT TEMP-TABLE ttPoOrdlAdd:HANDLE,
      INPUT cLocation + "\Po-ordl-Add.csv",
      INPUT TRUE,  /* Export Header */
      INPUT FALSE, /* Auto increment File name */
      OUTPUT lSuccess,
      OUTPUT cMessage
      ).        
  IF lPurge THEN     
      FOR EACH ttPoAll:
          EXPORT STREAM sPoAll ttPoAll.    
      END. 
  RUN Output_TempTableToCSV IN hdOutputProcs (
      INPUT TEMP-TABLE ttPoAll:HANDLE,
      INPUT cLocation + "\Po-All.csv",
      INPUT TRUE,   /* Export Header */
      INPUT FALSE,  /* Auto increment File name */
      OUTPUT lSuccess,
      OUTPUT cMessage
      ).
  OUTPUT STREAM sPoOrd     CLOSE.
  OUTPUT STREAM sPoOrdl    CLOSE.
  OUTPUT STREAM sPoOrdlAdd CLOSE.
  OUTPUT STREAM sPoAll     CLOSE.   
                       
  MESSAGE (IF lPurge THEN TRIM(c-win:TITLE) ELSE "Simulation") + " Process Is Completed." VIEW-AS ALERT-BOX.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

RETURN NO-APPLY.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

