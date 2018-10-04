&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\r-recreg.w

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
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

assign
 cocode = gcompany
 locode = gloc.

{ap/reconcil.i NEW}


DEF TEMP-TABLE tt-fileload NO-UNDO
    FIELDS tt-linefeed AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 btn-browse fi_loadfile fi_expFile ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_loadfile fi_expFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-browse 
     LABEL "Browse" 
     SIZE 14.6 BY 1.14.

DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_expFile AS CHARACTER FORMAT "X(50)":U 
     LABEL "Exception File" 
     VIEW-AS FILL-IN 
     SIZE 62.4 BY 1 NO-UNDO.

DEFINE VARIABLE fi_loadfile AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 62.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     btn-browse AT ROW 5.14 COL 79.4 WIDGET-ID 8
     fi_loadfile AT ROW 5.19 COL 13.4 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fi_expFile AT ROW 7.19 COL 13.4 COLON-ALIGNED WIDGET-ID 4
     btn-ok AT ROW 12.14 COL 21
     btn-cancel AT ROW 12.14 COL 56
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Load File:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 5.38 COL 3.4 WIDGET-ID 6
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 13.33.


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
         TITLE              = "A/P Load Bank Reconciliation File"
         HEIGHT             = 13.57
         WIDTH              = 96
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
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
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* A/P Load Bank Reconciliation File */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/P Load Bank Reconciliation File */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-browse C-Win
ON CHOOSE OF btn-browse IN FRAME FRAME-A /* Browse */
DO:
    DEF VAR v-okflg AS LOG NO-UNDO.

    DEF VAR v-loadFile AS CHAR NO-UNDO.
    DEF VAR v-path AS CHAR NO-UNDO INIT "c:/tmp".


    SYSTEM-DIALOG GET-FILE v-loadFile
        TITLE 'Select File to load'
        FILTERS    "Listing Files (*.txt)" "*.txt",
                    "All Files (*.*)" "*.*"
        INITIAL-DIR v-path
        MUST-EXIST USE-FILENAME 
        UPDATE v-okflg.

    IF NOT v-okflg THEN RETURN NO-APPLY.

    IF TRIM(v-loadFile) NE "" 
      THEN ASSIGN fi_loadfile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-loadFile.
      ELSE RETURN NO-APPLY.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.


  IF TRIM(fi_loadfile) EQ "" OR
       SEARCH(fi_loadFile) EQ ? THEN DO:

        MESSAGE 
             " Please enter a valid file to load "
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.

  END.


  RUN process-file.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_loadfile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_loadfile C-Win
ON HELP OF fi_loadfile IN FRAME FRAME-A
DO:
  APPLY "CHOOSE" TO btn-browse IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_loadfile C-Win
ON LEAVE OF fi_loadfile IN FRAME FRAME-A
DO:

    ASSIGN fi_loadfile = fi_loadfile:SCREEN-VALUE.


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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

 RUN enable_UI.

 fi_expFile:HIDDEN = TRUE.

  {methods/nowait.i}
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY fi_loadfile fi_expFile 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 btn-browse fi_loadfile fi_expFile btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-file C-Win 
PROCEDURE process-file :
DEF VAR v-acctnum AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-chcknum AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-statcd  AS CHAR FORMAT "x(1)"  NO-UNDO.
DEF VAR v-amount  AS CHAR FORMAT "x(12)" NO-UNDO.
DEF VAR v-pddate  AS CHAR FORMAT "x(6)"  NO-UNDO.
DEF VAR v-batch   AS CHAR FORMAT "x(8)"  NO-UNDO.
DEF VAR v-addtl   AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-fllr    AS CHAR FORMAT "x(18)" NO-UNDO.
DEF VAR v-msg     AS CHAR                NO-UNDO.

DEF VAR v-iflg    AS LOG INIT YES        NO-UNDO.

DEF VAR v-icnt    AS INT                 NO-UNDO.
DEF VAR v-zcnt    AS INT INIT 1          NO-UNDO.
DEF VAR v-int     AS INT                 NO-UNDO.

SESSION:SET-WAIT-STATE("general").

EMPTY TEMP-TABLE tt-fileload.

INPUT FROM VALUE(fi_loadFile).
REPEAT:
    CREATE tt-fileload.
    IMPORT tt-fileload.tt-linefeed.
END.

RUN ap/reconcil.p.

FOR EACH tt-fileload:

    IF TRIM(tt-fileload.tt-linefeed) EQ "" THEN  DO:
        DELETE tt-fileload.
        NEXT.
    END.

    ASSIGN
        v-acctnum = SUBSTR(tt-fileload.tt-linefeed,1,10) 
        v-chcknum = SUBSTR(tt-fileload.tt-linefeed,11,10) 
        v-statcd  = SUBSTR(tt-fileload.tt-linefeed,21,1) 
        v-amount  = SUBSTR(tt-fileload.tt-linefeed,22,12) 
        v-pddate  = SUBSTR(tt-fileload.tt-linefeed,34,6) 
        v-batch   = SUBSTR(tt-fileload.tt-linefeed,40,8) 
        v-addtl   = SUBSTR(tt-fileload.tt-linefeed,48,15) 
        v-fllr    = SUBSTR(tt-fileload.tt-linefeed,63,18).

    /* CLEANUP */ 
    /* CLEAN UP RECORDS  */
    v-zcnt = 0.
    DO v-icnt = 1 TO LENGTH(v-chcknum):

     ASSIGN v-int = INT(SUBSTR(v-chcknum,v-icnt,1)).

     IF v-int EQ 0 
      THEN v-zcnt = v-zcnt + 1.
      ELSE LEAVE.
    END.

    IF v-zcnt GT 0 
      THEN ASSIGN v-chcknum = SUBSTR(v-chcknum,v-zcnt + 1).

/*
    IF SUBSTR(v-chcknum, LENGTH(v-chcknum),1) = "0"
      THEN
        ASSIGN 
           v-chcknum = TRIM(v-chcknum,"0") + "0".
      ELSE
        ASSIGN 
           v-chcknum = TRIM(v-chcknum,"0").
*/
    ASSIGN
         v-amount = SUBSTR(v-amount,1,LENGTH(v-amount) - 2) + "." + 
                    SUBSTR(v-amount,LENGTH(v-amount) - 1).

    IF SUBSTR(v-amount, LENGTH(v-amount) - 1,2) = "00"
      THEN
        ASSIGN 
           v-amount = TRIM(v-amount,"0") + "00".
      ELSE
        ASSIGN 
           v-amount = TRIM(v-amount,"0").

    FIND FIRST reconcile NO-LOCK
        WHERE reconcile.tt-number = v-chcknum
          AND reconcile.tt-amt = DEC(v-amount) NO-ERROR.
    IF AVAIL reconcile THEN DO:

        IF v-statcd = "R" THEN reconcile.tt-cleared = YES.

        RUN reconcile-file.

        DELETE tt-fileload.

    END.           
END.

FIND FIRST tt-fileload NO-LOCK NO-ERROR.
IF AVAIL tt-fileload THEN DO:
     v-iflg = YES.
    OUTPUT TO VALUE(SUBSTR(fi_loadFile,1,LENGTH(fi_loadFile) - 4 ) +
                            "_exp" + SUBSTR(fi_loadFile,LENGTH(fi_loadFile) - 3)
                            ).

    FOR EACH tt-fileload.
       EXPORT tt-fileload.tt-linefeed.
    END.
    OUTPUT CLOSE.

    ASSIGN 
        fi_expFile:HIDDEN IN FRAME {&FRAME-NAME}      = FALSE
        fi_expFile:SCREEN-VALUE = SUBSTR(fi_loadFile,1,LENGTH(fi_loadFile) - 4 ) +
                            "_exp" + SUBSTR(fi_loadFile,LENGTH(fi_loadFile) - 3)
        fi_expFile:SENSITIVE    = FALSE.


END.

IF v-iflg 
  THEN v-msg = " Please check the exception file. ".
  ELSE v-msg = "".

MESSAGE "Load Complete !!" v-msg
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*
APPLY "CHOOSE" TO btn-cancel IN FRAME {&FRAME-NAME}.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reconcile-file C-Win 
PROCEDURE reconcile-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-ap-pay FOR ap-pay.

FIND CURRENT reconcile NO-LOCK NO-ERROR.

IF tt-type EQ 1 THEN DO /*TRANSACTION*/ :

    FIND ap-pay WHERE ROWID(ap-pay) EQ tt-rowid NO-LOCK NO-ERROR.
    IF AVAIL ap-pay 
      THEN
        IF ap-pay.d-no NE 0  AND
           NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) 
          THEN DO:
            FIND FIRST bf-ap-pay
                WHERE bf-ap-pay.company   EQ ap-pay.company
                  AND bf-ap-pay.check-act EQ ap-pay.check-act
                  AND bf-ap-pay.check-no  EQ ap-pay.d-no EXCLUSIVE-LOCK NO-ERROR.
        END.
        ELSE 
          FIND bf-ap-pay EXCLUSIVE-LOCK 
             WHERE ROWID(bf-ap-pay) EQ ROWID(ap-pay) NO-ERROR.


    IF AVAIL bf-ap-pay THEN DO:

        bf-ap-pay.cleared = reconcile.tt-cleared.

        FOR EACH ap-pay EXCLUSIVE-LOCK
            WHERE ap-pay.company EQ bf-ap-pay.company
              AND ap-pay.d-no    EQ bf-ap-pay.check-no
              AND NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)             
             USE-INDEX d-no:

            ASSIGN ap-pay.cleared = bf-ap-pay.cleared.

        END.
    END.

END. /*tt-type eq 1*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.
  def var parm-fld-list as cha no-undo.
  def var parm-lbl-list as cha no-undo.
  def var i as int no-undo.
  def var lv-label as cha.

  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
              lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name then do:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                  end.
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
              end.       
           end.                 
        end.            
     lv-field-hdl = lv-field-hdl:next-sibling.   
  end.

  put space(28)
      "< Selection Parameters >"
      skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".

      put lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.

  put fill("-",80) format "x(80)" skip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

