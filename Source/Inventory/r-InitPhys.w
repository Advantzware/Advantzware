&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: inventory\r-initPhys.w

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
DEFINE INPUT  PARAMETER ipcInitType AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha NO-UNDO.

DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.



DEFINE VARIABLE ll-secure AS LOG NO-UNDO.

DEFINE STREAM excel.
DEFINE STREAM excel2 .
DEFINE VARIABLE fi_file AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE cNk1Char AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 fiFromItem fiEndItem fiWhseList ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiFromItem fiEndItem fiWhseList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAddSpaceToList C-Win 
FUNCTION fAddSpaceToList RETURNS CHARACTER
  ( ipcList AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiEndItem AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzz" 
     LABEL "To Item" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Item" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fiWhseList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Whse LIst" 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 105 BY 8.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiFromItem AT ROW 3.86 COL 13 COLON-ALIGNED WIDGET-ID 26
     fiEndItem AT ROW 3.86 COL 53 COLON-ALIGNED WIDGET-ID 28
     fiWhseList AT ROW 4.81 COL 13 COLON-ALIGNED WIDGET-ID 14
     btn-ok AT ROW 11 COL 22
     btn-cancel AT ROW 11 COL 54
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-7 AT ROW 1.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.19
         SIZE 105.6 BY 13.14.


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
         TITLE              = "FG Physical Inventory Initialize"
         HEIGHT             = 13.33
         WIDTH              = 106.8
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
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* FG Physical Inventory Initialize */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FG Physical Inventory Initialize */
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
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEFINE VARIABLE lValidEntry AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cInvalidValues AS CHARACTER NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
 
  RUN validateWhseList (INPUT fiWhseList,  OUTPUT cInvalidValues, OUTPUT lValidEntry).
  IF NOT lValidEntry THEN DO:
    MESSAGE "Entry of warehouse list is not valid: " + cInvalidValues
    VIEW-AS ALERT-BOX.
    RETURN.
  END.
  RUN run-report. 
  MESSAGE "Initialize Complete"
  VIEW-AS ALERT-BOX.  
  APPLY 'Close' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndItem C-Win
ON HELP OF fiEndItem IN FRAME FRAME-A /* To Item */
DO:
        DEFINE VARIABLE recid-val AS RECID     NO-UNDO.
        DEFINE VARIABLE char-val  AS CHARACTER NO-UNDO. 
        IF ipcInitType EQ "FG" THEN DO:
            RUN windows/l-itemfg.w (cocode,"", {&SELF-NAME}:SCREEN-VALUE,OUTPUT char-val).
            IF char-val <> "" THEN 
            DO :
                ASSIGN 
                    {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                    .
            END. 
        END.
        ELSE DO:
            RUN windows/l-itmre.w (cocode,"","","R",FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT recid-val).
            FIND item WHERE RECID(item) EQ recid-val NO-LOCK NO-ERROR.
            IF AVAILABLE item AND item.i-no NE FOCUS:SCREEN-VALUE THEN 
            DO:
                FOCUS:SCREEN-VALUE = item.i-no.                        
            END.               
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFromItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFromItem C-Win
ON HELP OF fiFromItem IN FRAME FRAME-A /* From Item */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.
        DEFINE VARIABLE recid-val AS RECID     NO-UNDO.
        IF ipcInitType EQ "FG" THEN DO:
            RUN windows/l-itemfg.w (cocode,"", {&SELF-NAME}:SCREEN-VALUE,OUTPUT char-val).
            IF char-val <> "" THEN 
            DO :
                ASSIGN 
                    {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                    .
            END. 
        END.
        ELSE DO:
            RUN windows/l-itmre.w (cocode,"","","R",FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT recid-val).
            FIND item WHERE RECID(item) EQ recid-val NO-LOCK NO-ERROR.
            IF AVAILABLE item AND item.i-no NE FOCUS:SCREEN-VALUE THEN 
            DO:
                FOCUS:SCREEN-VALUE = item.i-no.                        
            END.                                      
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiWhseList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiWhseList C-Win
ON HELP OF fiWhseList IN FRAME FRAME-A /* Whse LIst */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN windows/l-locMulti.w (cocode,ENTRY(1, fiWhseList:SCREEN-VALUE), OUTPUT char-val).
        IF char-val <> "" THEN 
        DO :
            ASSIGN 
            {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val, "|")
                .
        END. 
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
  END.
    
            .
  IF ipcInitType EQ "FG" THEN
    c-win:TITLE              = "FG Physical Inventory Initialize". 
  ELSE
    c-win:TITLE              = "RM Physical Inventory Initialize".
  
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
  DISPLAY fiFromItem fiEndItem fiWhseList 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 fiFromItem fiEndItem fiWhseList btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDisplay AS cha NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS cha NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHA NO-UNDO.
DEFINE VARIABLE cVarValue AS cha NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS cha NO-UNDO.
DEFINE VARIABLE cSelectedList AS cha NO-UNDO.
DEFINE VARIABLE cFieldName AS cha NO-UNDO.
DEFINE VARIABLE fg-str-tit AS cha FORM "x(170)" NO-UNDO.
DEFINE VARIABLE fg-str-tit2 AS cha FORM "x(170)" NO-UNDO.
DEFINE VARIABLE fg-str-tit3 AS cha FORM "x(170)" NO-UNDO.
DEFINE VARIABLE fg-str-line AS cha FORM "x(170)" NO-UNDO.

DEFINE VARIABLE mach-str-tit AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE mach-str-tit2 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE mach-str-tit3 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE mach-str-line AS cha FORM "x(150)" NO-UNDO.

DEFINE VARIABLE item-str-tit AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE item-str-tit2 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE item-str-tit3 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE item-str-line AS cha FORM "x(150)" NO-UNDO.

DEFINE VARIABLE misc-str-tit AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE misc-str-tit2 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE misc-str-tit3 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE misc-str-line AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE exelHeader AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSnapshotID AS INTEGER NO-UNDO.
SESSION:SET-WAIT-STATE ("general").
    def var h as handle.  
    IF ipcInitType EQ "FG" THEN 
      RUN inventory/cyclecountcompare.p persistent set h.
    ELSE 
      RUN inventory/cyclecountcompareRM.p persistent set h.
    FIND LAST inventorySnapshot NO-LOCK 
        USE-INDEX inventorySnapshotID
        NO-ERROR.
    IF AVAIL inventorySnapshot THEN 
        iSnapshotID = inventorySnapshot.inventorySnapshotID + 1.
    ELSE 
        iSnapshotID = 1.
    
    run exportSnapshot in h     
     (input cocode,  
      input fiFromItem, 
      input fiEndItem,  
      input fAddSpaceToList(fiWhseList),
      INPUT iSnapshotID
      ).  
      
    delete object h.  
    

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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
  DEFINE VARIABLE lv-frame-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-group-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE parm-fld-list AS cha NO-UNDO.
  DEFINE VARIABLE parm-lbl-list AS cha NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateSnapshotFolder C-Win 
PROCEDURE validateSnapshotFolder :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cFolder AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.

FILE-INFO:FILE-NAME = cFolder.
IF FILE-INFO:FULL-PATHNAME EQ ? OR FILE-INFO:FULL-PATHNAME EQ "" THEN 
  OS-CREATE-DIR VALUE(cFolder).
IF FILE-INFO:FULL-PATHNAME EQ ? OR FILE-INFO:FULL-PATHNAME EQ "" THEN
  oplValid = NO.
ELSE
  oplValid = YES. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateWhseList C-Win 
PROCEDURE validateWhseList :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcWhseList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcInvalidList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.
DEFINE VARIABLE cInvalidList AS CHARACTER NO-UNDO.
lValid = YES.
DO iIndex = 1 TO NUM-ENTRIES(ipcWhseList):
    IF NOT CAN-FIND(FIRST loc NO-LOCK WHERE loc.company EQ cocode AND loc.loc EQ ENTRY(iIndex, ipcWhseList)) THEN 
       ASSIGN cInvalidList = cInvalidList + "," +  ENTRY(iIndex, ipcWhseList)
              lValid = NO
              .
END.
ASSIGN oplValid = lValid
       opcInvalidList = cInvalidList
       .



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAddSpaceToList C-Win 
FUNCTION fAddSpaceToList RETURNS CHARACTER
  ( ipcList AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:  Needed because some loc values contained spaces at the end
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cList2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cList3 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIter AS INTEGER NO-UNDO.
    
    DO iIter = 1 to NUM-ENTRIES(ipcList):
      cList2 = cList2 + TRIM(ENTRY(iIter, ipcList)) + ",".
      cList3 = cList3 + TRIM(ENTRY(iIter, ipcList)) + " " + ",".
    END.
    
    cList2 = trim(cList2, ",").
    cList3 = trim(cList3, ",").
    cResult = cList2 + "," + cList3.
    RETURN cResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

