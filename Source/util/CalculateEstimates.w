&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

USING system.SharedConfig.

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

ASSIGN
    cocode = gcompany
    locode = gloc.
 
{est\ttInputEst.i NEW}

DEFINE VARIABLE hdEstimateCalcProcs AS HANDLE.
DEFINE VARIABLE hFreightProcs       AS HANDLE    NO-UNDO.
DEFINE VARIABLE tmp-dir             AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdOutputProcs       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdQuoteProcs        AS HANDLE.

DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xef  FOR ef.
DEFINE NEW SHARED BUFFER xeb  FOR eb.
DEFINE NEW SHARED BUFFER xqty FOR est-qty.
DEFINE NEW SHARED VARIABLE xcal   AS DECIMAL       NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-wid AS DECIMAL       NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-len AS DECIMAL       NO-UNDO.

DEFINE VARIABLE scInstance AS CLASS system.SharedConfig NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-18 begin_est-no end_est-no tb_cal-est ~
tb_create-quote btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_est-no end_est-no tb_cal-est ~
tb_create-quote 

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

DEFINE VARIABLE begin_est-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE end_est-no AS CHARACTER FORMAT "X(8)" INITIAL "99999999" 
     LABEL "Ending Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 6.86.

DEFINE VARIABLE tb_cal-est AS LOGICAL INITIAL no 
     LABEL "Calculate Estimate" 
     VIEW-AS TOGGLE-BOX
     SIZE 67.2 BY .81 NO-UNDO.

DEFINE VARIABLE tb_create-quote AS LOGICAL INITIAL no 
     LABEL "Create a quote" 
     VIEW-AS TOGGLE-BOX
     SIZE 67.2 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_est-no AT ROW 2.71 COL 31 COLON-ALIGNED HELP
          "Enter Begining Estimate# " WIDGET-ID 6
     end_est-no AT ROW 2.71 COL 78.8 COLON-ALIGNED HELP
          "Enter Ending Estimate# " WIDGET-ID 8
     tb_cal-est AT ROW 5.24 COL 33.8 WIDGET-ID 62
     tb_create-quote AT ROW 6.52 COL 33.8 WIDGET-ID 64
     btn-process AT ROW 9.67 COL 33.4
     btn-cancel AT ROW 9.67 COL 64.4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .95 AT ROW 1.24 COL 4
          FONT 4
     RECT-18 AT ROW 1.76 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.6 BY 11.33.


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
         TITLE              = "Estimate Processing"
         HEIGHT             = 11.33
         WIDTH              = 102.6
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 6
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
       begin_est-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_est-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Create Logistics Estimates */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Create Logistics Estimates */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est-no C-Win
ON HELP OF begin_est-no IN FRAME FRAME-A /* Beginning Estimate# */
DO:
   DEF VAR char-val AS cha NO-UNDO.

    RUN windows/l-esttyp.w (g_company,g_loc,"568","EST",FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN begin_est-no:SCREEN-VALUE = ENTRY(1,char-val).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est-no C-Win
ON LEAVE OF begin_est-no IN FRAME FRAME-A /* Beginning Estimate# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est-no C-Win
ON VALUE-CHANGED OF begin_est-no IN FRAME FRAME-A /* Beginning Estimate# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
        DEFINE VARIABLE v-process AS LOG INIT NO NO-UNDO.
    
        DO WITH FRAME {&FRAME-NAME}:  
            begin_est-no:SCREEN-VALUE = FILL(" ",8 - LENGTH(TRIM(INPUT begin_est-no))) + TRIM(INPUT begin_est-no).
            end_est-no:SCREEN-VALUE = FILL(" ",8 - LENGTH(TRIM(INPUT end_est-no))) + TRIM(INPUT end_est-no).
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.

        MESSAGE "Are you sure you want to proceed with Esimate processing?" 
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE v-process.

        IF v-process THEN RUN run-process.   
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est-no C-Win
ON HELP OF end_est-no IN FRAME FRAME-A /* Ending Estimate# */
DO:
   DEF VAR char-val AS cha NO-UNDO.

    RUN windows/l-esttyp.w (g_company,g_loc,"568","EST",FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN end_est-no:SCREEN-VALUE = ENTRY(1,char-val).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est-no C-Win
ON LEAVE OF end_est-no IN FRAME FRAME-A /* Ending Estimate# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cal-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cal-est C-Win
ON VALUE-CHANGED OF tb_cal-est IN FRAME FRAME-A /* Calculate Estimate */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_create-quote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_create-quote C-Win
ON VALUE-CHANGED OF tb_create-quote IN FRAME FRAME-A /* Create a quote */
DO:
        ASSIGN {&self-name}.
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

    {methods/nowait.i}

    DO WITH FRAME {&frame-name}:    
        {custom/usrprint.i}          
        APPLY "entry" TO begin_est-no.         
    END.
       
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
  DISPLAY begin_est-no end_est-no tb_cal-est tb_create-quote 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-18 begin_est-no end_est-no tb_cal-est tb_create-quote btn-process 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateInputEst C-Win 
PROCEDURE pCreateInputEst :
DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO .
    DEFINE BUFFER b-eb FOR eb.
    
    
    FIND FIRST b-eb WHERE ROWID(b-eb) EQ  ipriRowid NO-LOCK NO-ERROR .
    
    IF AVAILABLE b-eb THEN
    DO:
        FIND FIRST ef NO-LOCK
            WHERE ef.company EQ b-eb.company
            AND ef.est-no EQ b-eb.est-no
            AND ef.form-no EQ b-eb.form-no  
            NO-ERROR.
                
        CREATE ttInputEst.
        ASSIGN 
            ttInputEst.cCompany         = cocode
            ttInputEst.cPartID          = b-eb.part-no
            ttInputEst.cStockNo         = b-eb.stock-no
            ttInputEst.cPartName        = b-eb.part-dscr1
            ttInputEst.cPartDescription = b-eb.part-dscr2        
            ttInputEst.cBndlCode        = b-eb.cas-no
            ttInputEst.iUnitCount       = b-eb.cas-cnt
            ttInputEst.iPerPallet       = b-eb.cas-pal
            ttInputEst.iPartial         = b-eb.quantityPartial
            ttInputEst.cPallet          = b-eb.tr-no
            ttInputEst.iFormNo          = 1
            ttInputEst.iBlankNo         = 1 
            ttInputEst.cCustomer        = b-eb.cust-no 
            ttInputEst.cShipTo          = b-eb.ship-id 
            ttInputEst.cStyle           = b-eb.style 
            ttInputEst.dLength          = b-eb.len 
            ttInputEst.dWidth           = b-eb.wid 
            ttInputEst.dDepth           = b-eb.dep 
            ttInputEst.cBoard           = IF AVAILABLE ef THEN ef.board ELSE ""  
            ttInputEst.iQuantity        = b-eb.eqty 
            ttInputEst.cCategory        = b-eb.procat 
            ttInputEst.dWeightPerM      = b-eb.weight
            ttInputEst.iStackHeight     = b-eb.stackHeight
            ttInputEst.iStackCode       = b-eb.stack-code 
            ttInputEst.cEstType         = "MiscEstimate"
            ttInputEst.cSourceEst       = b-eb.est-no 
            ttInputEst.cTest            = b-eb.test
            ttInputEst.cFlute           = b-eb.flute
            .
        
        FIND FIRST est-qty NO-LOCK
            WHERE est-qty.company EQ b-eb.company
            AND est-qty.est-no EQ b-eb.est-no
            AND est-qty.eqty EQ b-eb.eqty NO-ERROR . 
    
        IF AVAILABLE est-qty THEN 
        DO:             
            ASSIGN 
                ttInputEst.copy-qty[2]  = est-qty.qty[2] 
                ttInputEst.copy-qty[3]  = est-qty.qty[3] 
                ttInputEst.copy-qty[4]  = est-qty.qty[4] 
                ttInputEst.copy-qty[5]  = est-qty.qty[5] 
                ttInputEst.copy-qty[6]  = est-qty.qty[6] 
                ttInputEst.copy-qty[7]  = est-qty.qty[7] 
                ttInputEst.copy-qty[8]  = est-qty.qty[8] 
                ttInputEst.copy-qty[9]  = est-qty.qty[9] 
                ttInputEst.copy-qty[10] = est-qty.qty[10]
         
                ttInputEst.copy-qty[11] = est-qty.qty[11] 
                ttInputEst.copy-qty[12] = est-qty.qty[12] 
                ttInputEst.copy-qty[13] = est-qty.qty[13] 
                ttInputEst.copy-qty[14] = est-qty.qty[14] 
                ttInputEst.copy-qty[15] = est-qty.qty[15] 
                ttInputEst.copy-qty[16] = est-qty.qty[16] 
                ttInputEst.copy-qty[17] = est-qty.qty[17] 
                ttInputEst.copy-qty[18] = est-qty.qty[18] 
                ttInputEst.copy-qty[19] = est-qty.qty[19] 
                ttInputEst.copy-qty[20] = est-qty.qty[20] 

                ttInputEst.copy-rel[1]  = est-qty.qty[21]
                ttInputEst.copy-rel[2]  = est-qty.qty[22] 
                ttInputEst.copy-rel[3]  = est-qty.qty[23] 
                ttInputEst.copy-rel[4]  = est-qty.qty[24] 
                ttInputEst.copy-rel[5]  = est-qty.qty[25] 
                ttInputEst.copy-rel[6]  = est-qty.qty[26] 
                ttInputEst.copy-rel[7]  = est-qty.qty[27] 
                ttInputEst.copy-rel[8]  = est-qty.qty[28] 
                ttInputEst.copy-rel[9]  = est-qty.qty[29] 
                ttInputEst.copy-rel[10] = est-qty.qty[30]
         
                ttInputEst.copy-rel[11] = est-qty.qty[31] 
                ttInputEst.copy-rel[12] = est-qty.qty[32] 
                ttInputEst.copy-rel[13] = est-qty.qty[33] 
                ttInputEst.copy-rel[14] = est-qty.qty[34] 
                ttInputEst.copy-rel[15] = est-qty.qty[35] 
                ttInputEst.copy-rel[16] = est-qty.qty[36] 
                ttInputEst.copy-rel[17] = est-qty.qty[37] 
                ttInputEst.copy-rel[18] = est-qty.qty[38] 
                ttInputEst.copy-rel[19] = est-qty.qty[39] 
                ttInputEst.copy-rel[20] = est-qty.qty[40] .    
     
     
            ASSIGN
                ttInputEst.copy-runship[1]  = STRING(est-qty.whsed[1])
                ttInputEst.copy-runship[2]  = STRING(est-qty.whsed[2])
                ttInputEst.copy-runship[3]  = STRING(est-qty.whsed[3])
                ttInputEst.copy-runship[4]  = STRING(est-qty.whsed[4])
                ttInputEst.copy-runship[5]  = STRING(est-qty.whsed[5])
                ttInputEst.copy-runship[6]  = STRING(est-qty.whsed[6])
                ttInputEst.copy-runship[7]  = STRING(est-qty.whsed[7])
                ttInputEst.copy-runship[8]  = STRING(est-qty.whsed[8])
                ttInputEst.copy-runship[9]  = STRING(est-qty.whsed[9])
                ttInputEst.copy-runship[10] = STRING(est-qty.whsed[10])
                ttInputEst.copy-runship[11] = STRING(est-qty.whsed[11])
                ttInputEst.copy-runship[12] = STRING(est-qty.whsed[12])
                ttInputEst.copy-runship[13] = STRING(est-qty.whsed[13])
                ttInputEst.copy-runship[14] = STRING(est-qty.whsed[14])
                ttInputEst.copy-runship[15] = STRING(est-qty.whsed[15])
                ttInputEst.copy-runship[16] = STRING(est-qty.whsed[16])
                ttInputEst.copy-runship[17] = STRING(est-qty.whsed[17])
                ttInputEst.copy-runship[18] = STRING(est-qty.whsed[18])
                ttInputEst.copy-runship[19] = STRING(est-qty.whsed[19])
                ttInputEst.copy-runship[20] = STRING(est-qty.whsed[20]).
        
        END.

    END.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEFINE VARIABLE riEb          AS ROWID     NO-UNDO .
    DEFINE VARIABLE iEstReleaseID AS INTEGER   NO-UNDO .
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO .
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO .
  
    DEFINE VARIABLE list-name     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE init-dir      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
  
    DEFINE VARIABLE lPurge        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cError        AS CHARACTER NO-UNDO.
    
  
    {sys/inc/print1.i}         
    
    RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs. 
      
    cFileName =  init-dir + "\" + "CreateMiscEstimate.log" .
    
    OUTPUT TO value(cFileName) . 
    
    IF tb_cal-est THEN
    DO:
        RUN util/CalcEstimateForRange.p(cocode, 5, begin_est-no, end_est-no, OUTPUT cError).
     
        IF cError EQ "" THEN
            PUT UNFORMATTED cError  SKIP.
    END.
    
    IF tb_create-quote THEN
    DO:
        RUN util/CreateEstQuoteForRange.p(cocode, 5, begin_est-no, end_est-no, OUTPUT  cError).
    
        IF cError EQ "" THEN
            PUT UNFORMATTED cError  SKIP.
    END.
    
    SESSION:SET-WAIT-STATE("").      
    
    MESSAGE TRIM(c-win:TITLE) + " Created [" STRING(iCount) "] Quotes from [" STRING(iCount)"] source estimates.  View log in " STRING(cFileName) " file." VIEW-AS ALERT-BOX.  

    OUTPUT CLOSE.
    
    STATUS DEFAULT "Processing Complete".
    
            
  scInstance = SharedConfig:instance.
  scinstance:DeleteValue(INPUT "UtilPrompt"). /* Delete stale data, if any */            
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

