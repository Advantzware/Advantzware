&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES item

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define QUERY-STRING-FRAME-A FOR EACH item SHARE-LOCK
&Scoped-define OPEN-QUERY-FRAME-A OPEN QUERY FRAME-A FOR EACH item SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-A item
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-A item


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rd_item-code select-mat rd_alloc rd_stocked ~
rd_ord-policy fi_cc-code fi_pic-code fi_pur-rcode fi_cons-uom fi_pur-uom ~
fi_lead-days fi_loc fi_loc-bin btn-process btn-cancel RECT-17 RECT-18 
&Scoped-Define DISPLAYED-OBJECTS rd_item-code select-mat lbl-alloc rd_alloc ~
lbl-stocked rd_stocked lbl-ord-policy rd_ord-policy fi_cc-code fi_pic-code ~
fi_pur-rcode fi_cons-uom fi_pur-uom fi_lead-days fi_loc fi_loc-bin 

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

DEFINE VARIABLE fi_cc-code AS CHARACTER FORMAT "XX" 
     LABEL "Cycle Code" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cons-uom AS CHARACTER FORMAT "x(4)" 
     LABEL "Consumption UOM" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_lead-days AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Lead Time (Days)" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_loc AS CHARACTER FORMAT "x(5)" 
     LABEL "Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE fi_loc-bin AS CHARACTER FORMAT "x(12)" 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_pic-code AS CHARACTER FORMAT "x(3)" 
     LABEL "Prod. Code" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi_pur-rcode AS CHARACTER FORMAT "x(8)" 
     LABEL "Purch.Report Code" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi_pur-uom AS CHARACTER FORMAT "x(4)" 
     LABEL "Qty Purchased UOM" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE lbl-alloc AS CHARACTER FORMAT "X(256)":U INITIAL "Auto Allocate?" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE lbl-ord-policy AS CHARACTER FORMAT "X(256)":U INITIAL "Reorder Policy?" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lbl-stocked AS CHARACTER FORMAT "X(256)":U INITIAL "Stocked?" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE rd_alloc AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Yes", "Y",
"No", "N",
"No Update", ""
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE rd_item-code AS CHARACTER INITIAL "B" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Estimated", "E",
"Real", "R",
"Both", "B"
     SIZE 15 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_ord-policy AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Reorder Point", "Y",
"Lot Controlled", "N",
"No Update", ""
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE rd_stocked AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Yes", "Y",
"No", "N",
"No Update", ""
     SIZE 33 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 5.95.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 9.05.

DEFINE VARIABLE select-mat AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 56 BY 4.05 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FRAME-A FOR 
      item SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     rd_item-code AT ROW 6.24 COL 9 NO-LABEL
     select-mat AT ROW 6.24 COL 32 HELP
          "Enter description of this Material Type." NO-LABEL
     lbl-alloc AT ROW 12.19 COL 3 COLON-ALIGNED NO-LABEL
     rd_alloc AT ROW 12.19 COL 20 NO-LABEL
     lbl-stocked AT ROW 13.62 COL 7 COLON-ALIGNED NO-LABEL
     rd_stocked AT ROW 13.62 COL 20 NO-LABEL
     lbl-ord-policy AT ROW 15.05 COL 3 NO-LABEL
     rd_ord-policy AT ROW 15.05 COL 20 NO-LABEL
     fi_cc-code AT ROW 11.95 COL 74 COLON-ALIGNED HELP
          "Enter Code for Grouping Items for Physical Count Cycle Reports"
     fi_pic-code AT ROW 12.91 COL 74 COLON-ALIGNED HELP
          "Define Production & Inventory Control Code for Custom Reports."
     fi_pur-rcode AT ROW 13.86 COL 74 COLON-ALIGNED HELP
          "Define Code for use on Purchasing Reports for this Item."
     fi_cons-uom AT ROW 16.71 COL 26 COLON-ALIGNED HELP
          "Enter Consumption Unit Of Measure (Ea,M,TON,MSF,MSI,MLI,LBS)"
     fi_pur-uom AT ROW 17.67 COL 26 COLON-ALIGNED HELP
          "Enter Unit of Measure for Purchasing this Raw Material"
     fi_lead-days AT ROW 16.24 COL 66 COLON-ALIGNED HELP
          "Enter Number of Days from P.O. to delivery from Supplier."
     fi_loc AT ROW 17.19 COL 66 COLON-ALIGNED HELP
          "Enter primary warehouse/plant location where item is stocked"
     fi_loc-bin AT ROW 18.14 COL 66 COLON-ALIGNED HELP
          "Enter Primary Bin Location where Item is Stocked"
     btn-process AT ROW 20.29 COL 22
     btn-cancel AT ROW 20.29 COL 53
     RECT-17 AT ROW 4.81 COL 1
     RECT-18 AT ROW 10.76 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Select/Deselect Material Types" VIEW-AS TEXT
          SIZE 31 BY .76 AT ROW 5.52 COL 40
     "Default These Values" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 11.24 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 20.91.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 79 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 3
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
         TITLE              = "Default RM Item Fields"
         HEIGHT             = 20.91
         WIDTH              = 88.8
         MAX-HEIGHT         = 20.91
         MAX-WIDTH          = 111.2
         VIRTUAL-HEIGHT     = 20.91
         VIRTUAL-WIDTH      = 111.2
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   Custom                                                               */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".



DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-B:MOVE-BEFORE-TAB-ITEM (rd_item-code:HANDLE IN FRAME FRAME-A)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN lbl-alloc IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl-ord-policy IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lbl-stocked IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_item-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _TblList          = "asi.item"
     _Query            is OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Default RM Item Fields */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Default RM Item Fields */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR ll-process AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  ll-process  = NO.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
          " for the selected parameters?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-process.

  IF ll-process THEN RUN run-process.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_ord-policy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ord-policy C-Win
ON return OF rd_ord-policy IN FRAME FRAME-A
DO:
    apply "tab" to self.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
DEF VAR v-mat-list AS CHAR NO-UNDO.


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

  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH mat:                
      v-mat-list = v-mat-list + STRING(mat.mat,"x(5)") + " " + mat.dscr + ",".
    END.
    IF SUBSTR(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
      SUBSTR(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".

    select-mat:LIST-ITEMS = v-mat-list.
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

  {&OPEN-QUERY-FRAME-A}
  GET FIRST FRAME-A.
  DISPLAY rd_item-code select-mat lbl-alloc rd_alloc lbl-stocked rd_stocked 
          lbl-ord-policy rd_ord-policy fi_cc-code fi_pic-code fi_pur-rcode 
          fi_cons-uom fi_pur-uom fi_lead-days fi_loc fi_loc-bin 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE rd_item-code select-mat rd_alloc rd_stocked rd_ord-policy fi_cc-code 
         fi_pic-code fi_pur-rcode fi_cons-uom fi_pur-uom fi_lead-days fi_loc 
         fi_loc-bin btn-process btn-cancel RECT-17 RECT-18 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEF VAR lv-mat-type AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.


DO WITH FRAME {&FRAME-NAME}:
  DO li = 1 TO select-mat:NUM-ITEMS:
    IF select-mat:IS-SELECTED(li) THEN DO:
      li = select-mat:NUM-ITEMS + 2.
      LEAVE.
    END.
  END.

  IF li EQ select-mat:NUM-ITEMS + 2 THEN DO:
    DO li = 1 TO select-mat:NUM-ITEMS:
      IF select-mat:IS-SELECTED(li) THEN
        lv-mat-type = lv-mat-type + TRIM(SUBSTR(select-mat:ENTRY(li),1,5)) + ",".
    END.

    IF SUBSTR(lv-mat-type,LENGTH(TRIM(lv-mat-type)),1) EQ "," THEN
      SUBSTR(lv-mat-type,LENGTH(TRIM(lv-mat-type)),1) = "".
  END.
END.

SESSION:SET-WAIT-STATE("General").

IF lv-mat-type NE "" THEN
FOR EACH item
    WHERE item.company EQ cocode
      AND CAN-DO(lv-mat-type,item.mat-type)
      AND ((item.i-code EQ rd_item-code) OR rd_item-code EQ "B")
    BY item.i-no:

  DISPLAY "Processing Item: " + TRIM(item.i-no) FORMAT "x(40)"
      WITH FRAME f1.

  IF rd_alloc NE ""         THEN item.alloc = rd_alloc EQ "Y".
  IF rd_stocked NE ""       THEN item.stocked = rd_stocked EQ "Y".
  IF rd_ord-policy NE ""    THEN item.ord-policy = rd_ord-policy EQ "Y".
  IF fi_cons-uom NE ""      THEN item.cons-uom = fi_cons-uom.
  IF fi_pur-uom NE ""       THEN item.pur-uom = fi_pur-uom.
  IF fi_cc-code NE ""       THEN item.cc-code = fi_cc-code.
  IF fi_pic-code NE ""      THEN item.pic-code = fi_pic-code.
  IF fi_pur-rcode NE ""     THEN item.pur-rcode = fi_pur-rcode.
  IF fi_lead-days NE 0      THEN item.lead-days = fi_lead-days.
  IF fi_loc NE ""           THEN item.loc = fi_loc.
  IF fi_loc-bin NE ""       THEN item.loc-bin = fi_loc-bin.
END.

HIDE FRAME f1 NO-PAUSE.

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process is Completed..." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

