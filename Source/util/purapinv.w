&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap-ctrl.w.w

  Description: G/L Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

DEFINE VARIABLE v-process     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cFileLOcation AS CHARACTER NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

DEFINE TEMP-TABLE ttAPInv
    LIKE ap-inv.
DEFINE TEMP-TABLE ttAPInvl
    LIKE ap-invl.
DEFINE TEMP-TABLE ttAPPay
    LIKE ap-pay.
DEFINE TEMP-TABLE ttAPPayl
    LIKE ap-payl. 
DEFINE TEMP-TABLE ttAPDis
    LIKE ap-dis.
DEFINE TEMP-TABLE ttAPDisl
    LIKE ap-disl.             
          
DEFINE STREAM st-apinv.
DEFINE STREAM st-apinvl.
DEFINE STREAM st-appay.
DEFINE STREAM st-appayl.
DEFINE STREAM sAPDis.
DEFINE STREAM sAPDisl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_date end_date tb_open ~
btSimulate btn-process btn-cancel fiDirectory 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date tb_open fiDirectory 

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
     LABEL "Smulate Purge" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiDirectory AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 48 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 8.57.

DEFINE VARIABLE tb_open AS LOGICAL INITIAL no 
     LABEL "Purge Unpaid/Open AP Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 7.67 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 7.67 COL 60 COLON-ALIGNED HELP
          "Enter Ending Date"
     tb_open AT ROW 9.81 COL 27.2
     btSimulate AT ROW 15.29 COL 13.4 WIDGET-ID 2
     btn-process AT ROW 15.29 COL 36.2
     btn-cancel AT ROW 15.29 COL 58.8
     fiDirectory AT ROW 12.1 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     "Records to view or recover purged information will be stored in directory:" VIEW-AS TEXT
          SIZE 70 BY .91 AT ROW 11.29 COL 10.8 WIDGET-ID 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.2 BY 17.67.

DEFINE FRAME FRAME-B
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
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
         TITLE              = "Purge Paid AP Invoices"
         HEIGHT             = 17.67
         WIDTH              = 90.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
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
ON END-ERROR OF C-Win /* Purge Paid AP Invoices */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Paid AP Invoices */
DO:
  IF VALID-HANDLE(hdOutputProcs) THEN 
      DELETE PROCEDURE hdOutputProcs.    
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

  MESSAGE "Are you sure you want to delete the Paid AP Invoices within the " +
          "selection parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimulate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimulate C-Win
ON CHOOSE OF btSimulate IN FRAME FRAME-A /* Smulate Purge */
DO:
    v-process = NO.
    MESSAGE "Are you sure you want to simulate the purge of Paid AP Invoices within the " +
            "selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO 
        UPDATE lResponse AS LOGICAL.
        
    IF lResponse THEN 
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
  RUN FileSys_GetTempDirectory(
      OUTPUT cFileLocation
      ).
      
  ASSIGN 
      cFileLocation = cFileLocation + "\APPurge-" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")+ STRING(DAY(TODAY),"99") + "-" + STRING(TIME,"99999") 
      fiDirectory   = cFileLocation
      .  
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
  DISPLAY begin_date end_date tb_open fiDirectory 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_date end_date tb_open btSimulate btn-process btn-cancel 
         fiDirectory 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPurgeOrphanRecords C-Win 
PROCEDURE pPurgeOrphanRecords PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:Private procedure to delete orphan records i.e no parent record or
         blank company
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtBeginigDate AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipdtEndingDate  AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER iplPurge        AS LOGICAL   NO-UNDO.
    
    /*Delete records with blank company*/
    FOR EACH ap-inv EXCLUSIVE-LOCK
        WHERE ap-inv.company  EQ ""
          AND ap-inv.posted   EQ YES
          AND ap-inv.inv-date GE ipdtBeginigDate
          AND ap-inv.inv-date LE ipdtEndingDate
          AND NOT CAN-FIND(FIRST ttAPInv 
                           WHERE ttAPInv.company EQ ap-inv.company
                             AND ttAPInv.i-no    EQ ap-inv.i-no
                             )
        USE-INDEX posted:
        FOR EACH ap-invl EXCLUSIVE-LOCK 
            WHERE (ap-invl.company EQ "" OR ap-invl.company EQ ipcCompany) /* Delete all the child records if a parent is deleted */
              AND ap-invl.i-no     EQ ap-inv.i-no
              AND NOT CAN-FIND(FIRST ttAPInvl
                               WHERE ttAPInvl.company EQ ap-invl.company
                                 AND ttAPInvl.i-no    EQ ap-invl.i-no
                                 AND ttAPInvl.line    EQ ap-invl.line
                                 ):
            CREATE ttAPInvl.
            BUFFER-COPY ap-invl TO ttAPInvl.
            IF iplPurge THEN DO:
                EXPORT STREAM st-apinvl ap-invl.         
                DELETE ap-invl. 
            END.         
        END.
        CREATE ttAPInv.
        BUFFER-COPY ap-inv TO ttAPInv.
        IF iplPurge THEN DO:
            EXPORT STREAM st-apinv ap-inv.
            DELETE ap-inv. 
        END.                     
    END.        

    /* Delete ap-invl records with no header record */   
    FOR EACH ap-invl EXCLUSIVE-LOCK 
        WHERE ap-invl.company EQ ipcCompany
          AND ap-invl.posted  EQ YES
          AND NOT CAN-FIND(FIRST ap-inv
                           WHERE ap-inv.company  EQ ap-invl.company
                             AND ap-inv.posted   EQ YES
                             AND ap-inv.inv-date GE ipdtBeginigDate
                             AND ap-inv.inv-date LE ipdtEndingDate
                             AND ap-inv.i-no     EQ ap-invl.i-no
                           )
          AND NOT CAN-FIND(FIRST ttAPInvl
                           WHERE ttAPinvl.company EQ ap-invl.company
                             AND ttAPInvl.i-no    EQ ap-invl.i-no
                             AND ttAPInvl.line    EQ ap-invl.line
                           ):
        CREATE ttAPInvl.
        BUFFER-COPY ap-invl TO ttAPInvl.
        IF iplPurge THEN DO:
            EXPORT STREAM st-apinvl ap-invl.                      
            DELETE ap-invl. 
        END.                             
    END. 
         
    /*Delete records with blank company */
    FOR EACH ap-dis EXCLUSIVE-LOCK
        WHERE ap-dis.company    EQ "" 
          AND ap-dis.posted     EQ YES
          AND ap-dis.check-date GE ipdtBeginigDate
          AND ap-dis.check-date LE ipdtEndingDate
          AND NOT CAN-FIND(FIRST ttAPDis
                           WHERE ttAPDis.company EQ ap-dis.company
                             AND ttAPDis.d-no    EQ ap-dis.d-no
                           ):
        FOR EACH ap-disl EXCLUSIVE-LOCK 
            WHERE (ap-disl.company EQ ipcCompany OR ap-disl.company EQ "") /* Delete all the child records if a parent is deleted */
              AND ap-disl.d-no     EQ ap-dis.d-no
              AND NOT CAN-FIND(FIRST ttAPDisl
                               WHERE ttAPdisl.company EQ ap-disl.company
                                 AND ttAPDisl.d-no    EQ ap-disl.d-no
                                 AND ttAPDisl.line    EQ ap-disl.line
                               ):                 
            CREATE ttAPDisl.
            BUFFER-COPY ap-disl TO ttAPDisl.
            IF iplPurge THEN DO:
                EXPORT STREAM sAPDisl ap-disl.     
                DELETE ap-disl.
            END.          
        END.
        CREATE ttAPDis.
        BUFFER-COPY ap-dis TO ttAPDis.
        IF iplPurge THEN DO:
            EXPORT STREAM sAPDis ap-dis.
            DELETE ap-dis.
        END.                      
    END. 

    /* Delete ap-invl records with no header record */   
    FOR EACH ap-disl EXCLUSIVE-LOCK 
        WHERE ap-disl.company EQ ipcCompany
          AND ap-disl.posted  EQ YES
          AND NOT CAN-FIND(FIRST ap-dis
                           WHERE ap-dis.company    EQ ap-disl.company
                             AND ap-dis.posted     EQ YES
                             AND ap-dis.check-date GE ipdtBeginigDate
                             AND ap-dis.check-date LE ipdtEndingDate
                             AND ap-dis.d-no       EQ ap-disl.d-no
                           )
          AND NOT CAN-FIND(FIRST ttAPDisl
                           WHERE ttAPdisl.company EQ ap-disl.company
                             AND ttAPDisl.d-no    EQ ap-disl.d-no
                             AND ttAPDisl.line    EQ ap-disl.line
                           ):                              
        CREATE ttAPDisl.
        BUFFER-COPY ap-disl TO ttAPDisl.
        IF iplPurge THEN DO:
            EXPORT STREAM sAPDisl ap-disl.                      
            DELETE ap-disl. 
        END.                             
    END.             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ------------------------------------------------ util/apinvdel.p 03/98 JLF */
/* Purge Paid AP Invoices                                                     */
/* -------------------------------------------------------------------------- */

DEFINE BUFFER b-ap-payl FOR ap-payl.

DEFINE VARIABLE fdate    LIKE ap-inv.inv-date INIT 01/01/0001 FORMAT "99/99/9999".
DEFINE VARIABLE tdate    LIKE fdate           INIT 01/01/0001 FORMAT "99/99/9999".
DEFINE VARIABLE v-open   AS LOG               INIT NO         FORMAT "Yes/No".
DEFINE VARIABLE amt      AS DECIMAL.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.

IF v-process THEN DO:
    RUN FileSys_CreateDirectory(
         INPUT  cFileLocation + "\DataFiles",
         OUTPUT lSuccess,
         OUTPUT cMessage
         ).  
    OUTPUT STREAM st-apinv  TO VALUE(cFileLocation + "\DataFiles\ap-inv.d").
    OUTPUT STREAM st-apinvl TO VALUE(cFileLocation + "\DataFiles\ap-invl.d").
    OUTPUT STREAM st-appay  TO VALUE(cFileLocation + "\DataFiles\ap-pay.d").
    OUTPUT STREAM st-appayl TO VALUE(cFileLocation + "\DataFiles\ap-payl.d").
    OUTPUT STREAM sAPDis    TO VALUE(cFileLocation + "\DataFiles\ap-dis.d").
    OUTPUT STREAM sAPDisl   TO VALUE(cFileLocation + "\DataFiles\ap-disl.d").
    
END.

SESSION:SET-WAIT-STATE("General").

DO WITH FRAME {&frame-name}:
    ASSIGN
       begin_date
       end_date 
       tb_open  
       .
END.

ASSIGN
    fdate  = begin_date
    tdate  = end_date
    v-open = tb_open
    .


DISABLE TRIGGERS FOR LOAD OF ap-inv.
DISABLE TRIGGERS FOR LOAD OF ap-pay.
DISABLE TRIGGERS FOR LOAD OF ap-dis.

FOR EACH ap-inv
    WHERE ap-inv.company  EQ cocode
      AND ap-inv.posted   EQ YES
      AND ap-inv.inv-date GE fdate
      AND ap-inv.inv-date LE tdate
    USE-INDEX posted:

        amt       = 0 .

  FOR EACH ap-payl NO-LOCK
      WHERE ap-payl.inv-no   EQ ap-inv.inv-no
        AND ap-payl.vend-no  EQ ap-inv.vend-no
        AND ap-payl.posted   EQ YES
        AND ap-payl.due-date EQ ap-inv.due-date
        USE-INDEX inv-no,
      FIRST ap-pay NO-LOCK
      WHERE ap-pay.c-no EQ ap-payl.c-no
      USE-INDEX c-no:

    amt = amt - ap-payl.amt-paid +
          (ap-payl.amt-disc * if ap-payl.memo then 1 else -1) .

  END. /* for each ap-payl */

  amt = amt + ap-inv.net.

  IF v-open AND ( (ap-inv.due - ap-inv.paid ) > 0 OR 
                  (ap-inv.due - ap-inv.paid ) < 0 ) THEN amt = 0.

  IF amt EQ 0 THEN DO:
    FOR EACH ap-payl
        WHERE ap-payl.inv-no   EQ ap-inv.inv-no
          AND ap-payl.vend-no  EQ ap-inv.vend-no
          AND ap-payl.posted   EQ YES
          AND ap-payl.due-date EQ ap-inv.due-date
        USE-INDEX inv-no:
      FIND ap-pay
          WHERE ap-pay.c-no EQ ap-payl.c-no
          USE-INDEX c-no 
          NO-ERROR.
      IF AVAIL ap-pay THEN DO:
        FIND FIRST b-ap-payl NO-LOCK
            WHERE b-ap-payl.c-no   EQ ap-payl.c-no
              AND recid(b-ap-payl) NE recid(ap-payl)
            NO-ERROR.
        IF NOT AVAIL b-ap-payl THEN DO:
         CREATE ttAPPay.
         BUFFER-COPY ap-pay TO ttAPPay.
         IF v-process THEN DO:  
             EXPORT STREAM st-appay ap-pay.
             DELETE ap-pay.
         END.
        END.
      END.
      CREATE ttAPPayl.
      BUFFER-COPY ap-payl TO ttAPPayl.
      IF v-process THEN DO:
          EXPORT STREAM st-appayl ap-payl.
          DELETE ap-payl.
      END.
    END.

    FOR EACH ap-invl 
        WHERE ap-invl.company EQ cocode
          AND ap-invl.i-no    EQ ap-inv.i-no:
      CREATE ttAPInvl.
      BUFFER-COPY ap-invl TO ttAPInvl.
      IF v-process THEN DO:  
          EXPORT STREAM st-apinvl ap-invl.
          DELETE ap-invl.
      END.
    END.
    CREATE ttAPInv.
    BUFFER-COPY ap-inv TO ttAPInv.
    IF v-process THEN DO:
        EXPORT STREAM st-apinv ap-inv.
        DELETE ap-inv.
    END.
  END.
END.
FOR EACH ap-dis EXCLUSIVE-LOCK 
    WHERE ap-dis.company    EQ cocode
      AND ap-dis.posted     EQ YES
      AND ap-dis.check-date GE fdate
      AND ap-dis.check-date LE tDate:
    FOR EACH ap-disl EXCLUSIVE-LOCK 
        WHERE ap-disl.company EQ cocode
          AND ap-disl.d-no    EQ ap-dis.d-no:
        CREATE ttAPDisl.
        BUFFER-COPY ap-disl TO ttAPDisl.
        IF v-process THEN DO:
            EXPORT STREAM sAPDisl ap-disl.      
            DELETE ap-disl.
        END.          
    END. 
    CREATE ttAPDis.
    BUFFER-COPY ap-dis TO ttAPDis.
    IF v-process THEN DO:
        EXPORT STREAM sAPDis ap-dis.
        DELETE ap-dis. 
    END.              
END.          
RUN pPurgeOrphanRecords(
    INPUT cocode,
    INPUT fdate,
    INPUT tDate,
    INPUT v-Process
    ).
    
RUN FileSys_CreateDirectory(
     INPUT  cFileLocation + "\Csv",
     OUTPUT lSuccess,
     OUTPUT cMessage
     ).  
     MESSAGE cMessage
     VIEW-AS ALERT-BOX.  
RUN Output_TempTableToCSV IN hdOutputProcs (
    INPUT  TEMP-TABLE ttAPInv:HANDLE,
    INPUT  cFileLocation + "\Csv\ap-inv.csv",
    INPUT  TRUE,  /* Export Header */
    INPUT  FALSE, /* Auto increment File name */
    OUTPUT lSuccess,
    OUTPUT cMessage
    ). 
RUN Output_TempTableToCSV IN hdOutputProcs (
    INPUT  TEMP-TABLE ttAPInvl:HANDLE,
    INPUT  cFileLocation + "\Csv\ap-invl.csv",
    INPUT  TRUE,  /* Export Header */
    INPUT  FALSE, /* Auto increment File name */
    OUTPUT lSuccess,
    OUTPUT cMessage
    ). 
RUN Output_TempTableToCSV IN hdOutputProcs (
    INPUT  TEMP-TABLE ttAPPay:HANDLE,
    INPUT  cFileLocation + "\Csv\ap-pay.csv",
    INPUT  TRUE,  /* Export Header */
    INPUT  FALSE, /* Auto increment File name */
    OUTPUT lSuccess,
    OUTPUT cMessage
    ). 
RUN Output_TempTableToCSV IN hdOutputProcs (
    INPUT  TEMP-TABLE ttAPPayl:HANDLE,
    INPUT  cFileLocation + "\Csv\ap-payl.csv",
    INPUT  TRUE,  /* Export Header */
    INPUT  FALSE, /* Auto increment File name */
    OUTPUT lSuccess,
    OUTPUT cMessage
    ).   
RUN Output_TempTableToCSV IN hdOutputProcs (
    INPUT  TEMP-TABLE ttAPDis:HANDLE,
    INPUT  cFileLocation + "\Csv\ap-dis.csv",
    INPUT  TRUE,  /* Export Header */
    INPUT  FALSE, /* Auto increment File name */
    OUTPUT lSuccess,
    OUTPUT cMessage
    ).  
RUN Output_TempTableToCSV IN hdOutputProcs (
    INPUT  TEMP-TABLE ttAPDisl:HANDLE,
    INPUT  cFileLocation + "\Csv\ap-disl.csv",
    INPUT  TRUE,  /* Export Header */
    INPUT  FALSE, /* Auto increment File name */
    OUTPUT lSuccess,
    OUTPUT cMessage
    ).                        
OUTPUT STREAM st-apinv  CLOSE. 
OUTPUT STREAM st-apinvl CLOSE. 
OUTPUT STREAM st-appay  CLOSE. 
OUTPUT STREAM st-appayl CLOSE. 
OUTPUT STREAM sAPDis    CLOSE.
OUTPUT STREAM sAPDisl   CLOSE.

SESSION:SET-WAIT-STATE("").

MESSAGE (IF v-process THEN TRIM(c-win:TITLE) ELSE "Simulation")+ " Process Is Completed." VIEW-AS ALERT-BOX.
APPLY "CLOSE":U TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

