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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}

DEF BUFFER b-period FOR period.

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO. 
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEFINE STREAM sExcel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiFromItem fiToItem fiFromDate btCalendar ~
fiToDate btCalendar-2 cbTransType tgOutputOnly tb_excel tb_runExcel fi_file ~
btn-process btn-cancel RECT-18 
&Scoped-Define DISPLAYED-OBJECTS fiFromItem fiToItem fiFromDate fiToDate ~
cbTransType tgOutputOnly tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCalendar 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     IMAGE-DOWN FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Popup Calendar".

DEFINE BUTTON btCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     IMAGE-DOWN FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Popup Calendar".

DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE cbTransType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transaction Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Receipts","R",
                     "Adjustments","A",
                     "Counts","C",
                     "Shipments","S",
                     "Transfers","T"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromDate AS DATE FORMAT "99/99/9999":U 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Item" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE fiToDate AS DATE FORMAT "99/99/9999":U 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiToItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "To Item" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\convUomRmHist.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 4.52.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tgOutputOnly AS LOGICAL INITIAL no 
     LABEL "Output Only - No Updates to Database" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiFromItem AT ROW 2.19 COL 28 COLON-ALIGNED WIDGET-ID 2
     fiToItem AT ROW 2.19 COL 75 COLON-ALIGNED WIDGET-ID 4
     fiFromDate AT ROW 3.86 COL 28.2 COLON-ALIGNED WIDGET-ID 6
     btCalendar AT ROW 3.81 COL 47.6 WIDGET-ID 16
     fiToDate AT ROW 3.86 COL 75 COLON-ALIGNED WIDGET-ID 8
     btCalendar-2 AT ROW 3.86 COL 93 WIDGET-ID 18
     cbTransType AT ROW 5.91 COL 28.8 COLON-ALIGNED WIDGET-ID 10
     tgOutputOnly AT ROW 7.67 COL 31 WIDGET-ID 26
     tb_excel AT ROW 11.33 COL 73 RIGHT-ALIGNED WIDGET-ID 22
     tb_runExcel AT ROW 11.24 COL 94 RIGHT-ALIGNED WIDGET-ID 24
     fi_file AT ROW 12.19 COL 51 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 20
     btn-process AT ROW 18.1 COL 21
     btn-cancel AT ROW 18.1 COL 53
     "Output Options" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 9.81 COL 30 WIDGET-ID 30
          FGCOLOR 9 
     RECT-18 AT ROW 9.95 COL 20 WIDGET-ID 28
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.6 BY 19.43.


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
         TITLE              = "Change RM UOM in History"
         HEIGHT             = 19.52
         WIDTH              = 120.4
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 131.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 131.2
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Change RM UOM in History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Change RM UOM in History */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCalendar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCalendar C-Win
ON CHOOSE OF btCalendar IN FRAME FRAME-A
DO:
  
        RUN nosweat/popupcal.w (OUTPUT calendarDate).
  
        IF calendarDate NE '' THEN
            fiFromDate:SCREEN-VALUE  = STRING(DATE(calendarDate),"99/99/9999").

        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCalendar-2 C-Win
ON CHOOSE OF btCalendar-2 IN FRAME FRAME-A
DO:
  
        RUN nosweat/popupcal.w (OUTPUT calendarDate).
  
        IF calendarDate NE '' THEN
            fiToDate:SCREEN-VALUE  = STRING(DATE(calendarDate),"99/99/9999").

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
  DEF VAR v-process AS LOG INIT NO NO-UNDO.
  ASSIGN {&DISPLAYED-OBJECTS} 
  .
  
  IF NOT v-process THEN
    MESSAGE "Are you sure you want to change RM history UOMs ?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN DO WITH FRAME {&FRAME-NAME}:
    SESSION:SET-WAIT-STATE("general").
    
    RUN run-process.
    
    IF tgOutputOnly THEN 
      MESSAGE "Report completed." VIEW-AS ALERT-BOX INFORMATION.
    ELSE 
      MESSAGE "Update completed." VIEW-AS ALERT-BOX INFORMATION.

    SESSION:SET-WAIT-STATE("").
        
    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
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
  DISPLAY fiFromItem fiToItem fiFromDate fiToDate cbTransType tgOutputOnly 
          tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE fiFromItem fiToItem fiFromDate btCalendar fiToDate btCalendar-2 
         cbTransType tgOutputOnly tb_excel tb_runExcel fi_file btn-process 
         btn-cancel RECT-18 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEFINE VARIABLE hCostProcs  AS HANDLE  NO-UNDO.
DEFINE VARIABLE dCostPerUOM AS DECIMAL NO-UNDO.
DEFINE VARIABLE dQtyPerUOM  AS DECIMAL NO-UNDO.
DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
RUN system/costProcs.p PERSISTENT SET hCostProcs.

IF tb_excel THEN DO:
  excelHeader = "Item#,Item Cost UOM,History Date,Trans Type,History Cost,History Cost UOM,History Qty,History Qty UOM,New Cost,New Qty".
  OUTPUT STREAM sExcel TO VALUE(fi_file).
  PUT STREAM sExcel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

FOR EACH rm-rcpth NO-LOCK
  WHERE company EQ g_company
    AND rm-rcpth.i-no GE fiFromItem
    AND rm-rcpth.i-no LE fiToItem
    AND rm-rcpth.post-date GE fiFromDate
    AND rm-rcpth.post-date LE fiToDate
    AND rm-rcpth.rita-code EQ cbTransType
    ,
    EACH rm-rdtlh NO-LOCK 
        WHERE rm-rdtlh.r-no EQ rm-rcpth.r-no
    :
  FIND FIRST ITEM NO-LOCK
        WHERE ITEM.company EQ rm-rcpth.company
          AND ITEM.i-no  EQ rm-rcpth.i-no
        NO-ERROR.

   IF ITEM.pur-uom EQ rm-rcpth.pur-uom THEN 
        NEXT.
        
   dCostPerUOM = DYNAMIC-FUNCTION("fConvert" IN hCostProcs, rm-rcpth.pur-uom, ITEM.pur-uom, 0, 0, 0, 0, 1, 1, rm-rdtlh.cost).
   RUN sys/ref/convquom.p (rm-rcpth.pur-uom, ITEM.pur-uom,
        item.basis-w, item.s-len, item.s-wid, item.s-dep,
        rm-rdtlh.qty, OUTPUT dQtyPerUOM).
  IF tb_excel THEN 
    EXPORT STREAM sExcel DELIMITER ","
                         rm-rcpth.i-no      COLUMN-LABEL "Item#" 
                         ITEM.pur-uom       COLUMN-LABEL "Item Cost UOM"
                         rm-rcpth.post-date COLUMN-LABEL "History Date"
                         rm-rcpth.rita-code COLUMN-LABEL "Trans Type"
                         rm-rdtlh.cost      COLUMN-LABEL "History Cost"
                         rm-rcpth.pur-uom   COLUMN-LABEL "History Cost UOM"
                         rm-rdtlh.qty       COLUMN-LABEL "History Qty"
                         rm-rcpth.pur-uom   COLUMN-LABEL "History Qty UOM"
                         dCostPerUOM        COLUMN-LABEL "New Cost"
                         dQtyPerUOM         COLUMN-LABEL "New Qty"
                         .
    
  IF NOT AVAIL ITEM THEN 
        NEXT.  
    
  IF NOT tgOutputOnly THEN DO:

        FIND bf-rm-rcpth EXCLUSIVE-LOCK 
            WHERE ROWID(bf-rm-rcpth) EQ ROWID(rm-rcpth)
            NO-ERROR.
        IF NOT AVAIL bf-rm-rcpth THEN 
            RETURN.
        FIND bf-rm-rdtlh EXCLUSIVE-LOCK 
            WHERE ROWID(bf-rm-rdtlh) EQ ROWID(rm-rdtlh)
            NO-ERROR.
        IF NOT AVAIL bf-rm-rdtlh THEN 
            RETURN.
        ASSIGN     
            bf-rm-rcpth.pur-uom = ITEM.pur-uom
            bf-rm-rdtlh.cost    = dCostPerUOM
            bf-rm-rdtlh.qty     = dQtyPerUOM
            .
  END.
END.

IF tb_excel THEN DO:
  OUTPUT STREAM sExcel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.
DELETE OBJECT hCostProcs.
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

