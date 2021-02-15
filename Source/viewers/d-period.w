&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: rm\d-trans.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-recid2  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

DEFINE VARIABLE char-val                 AS cha           NO-UNDO.
DEFINE VARIABLE ext-cost                 AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lv-recid                 AS RECID         NO-UNDO.
DEFINE VARIABLE ls-prev-po               AS cha           NO-UNDO.
DEFINE VARIABLE hd-post                  AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hd-post-child            AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE ll-help-run              AS LOG           NO-UNDO.  /* set on browse help, reset row-entry */
DEFINE VARIABLE lv-fgrecpt-val           AS INTEGER       NO-UNDO.
DEFINE VARIABLE trans-time               AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-new-tag-number-chosen AS LOG           NO-UNDO.


DEFINE VARIABLE lv-item-recid            AS RECID         NO-UNDO.
DEFINE VARIABLE ll-order-warned          AS LOGICAL       NO-UNDO.
DEFINE VARIABLE ll-new-record            AS LOGICAL       NO-UNDO.
DEFINE BUFFER bf-rell FOR oe-rell.
DEFINE BUFFER bf-ordl FOR oe-ordl.

DEFINE VARIABLE lActiveBin      AS LOGICAL NO-UNDO.
DEF VAR ll-secure AS LOG INIT NO NO-UNDO.
DEF VAR op-company AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES period

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame period.yr period.pnum ~
period.pst period.pend period.pstat period.subLedgerAP period.subLedgerPO ~
period.subLedgerOP period.subLedgerWIP period.subLedgerRM ~
period.subLedgerFG period.subLedgerBR period.subLedgerAR period.APClosedBy period.POClosedBy period.OPClosedBy ~
period.WIPClosedBy period.RMClosedBy period.FGClosedBy period.BRClosedBy period.ARClosedBy ~
period.APClosed period.POClosed period.OPClosed ~
period.WIPClosed period.RMClosed period.FGClosed period.BRClosed period.ARClosed
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame period.yr period.pnum ~
period.pst period.pend period.pstat period.subLedgerAP period.subLedgerPO ~
period.subLedgerOP period.subLedgerWIP period.subLedgerRM ~
period.subLedgerFG period.subLedgerBR period.subLedgerAR
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame period
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame period
&Scoped-define TABLES-IN-QUERY-Dialog-Frame period
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame period


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS period.yr period.pnum period.pst period.pend ~
period.pstat period.subLedgerAP period.subLedgerPO period.subLedgerOP ~
period.subLedgerWIP period.subLedgerRM period.subLedgerFG ~
period.subLedgerBR period.subLedgerAR
&Scoped-define ENABLED-TABLES period
&Scoped-define FIRST-ENABLED-TABLE period
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 Btn_OK Btn_Done Btn_Cancel ~
btnCalendar-2 RECT-21 RECT-38 RECT-39 RECT-40 RECT-41 
&Scoped-Define DISPLAYED-FIELDS period.yr period.pnum period.pst ~
period.pend period.pstat period.subLedgerAP period.subLedgerPO ~
period.subLedgerOP period.subLedgerWIP period.subLedgerRM ~
period.subLedgerFG period.subLedgerBR period.subLedgerAR period.APClosedBy period.POClosedBy period.OPClosedBy ~
period.WIPClosedBy period.RMClosedBy period.FGClosedBy period.BRClosedBy period.ARClosedBy ~
period.APClosed period.POClosed period.OPClosed ~
period.WIPClosed period.RMClosed period.FGClosed period.BRClosed period.ARClosed 
&Scoped-define DISPLAYED-TABLES period
&Scoped-define FIRST-DISPLAYED-TABLE period
&Scoped-Define DISPLAYED-OBJECTS fi_tr-time cCangedby1 cCangedby-2 ~
cCangedby-3 cCangedby-4 cCangedby-5 cCangedby-6 cCangedby-7 cCangedby-8 ~
cDate1 cDate-2 cDate-3 cDate-4 cDate-5 cDate-6 cDate-7 cDate-8 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetLastDate Dialog-Frame
FUNCTION fGetLastDate RETURNS DATE PRIVATE
  (ipdDate AS DATE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .
     
DEFINE VARIABLE fi_tr-time AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tr Time" 
     VIEW-AS FILL-IN 
     SIZE 17.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 133.8 BY 15
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 13.33
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 94.2 BY 11.67
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 133.8 BY 1.67
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      period SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     period.yr AT ROW 1.24 COL 12.2 COLON-ALIGNED
          LABEL "Year" FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 1
     period.pnum AT ROW 1.24 COL 37 COLON-ALIGNED
          LABEL "Period" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 17.2 BY 1
          BGCOLOR 15 FONT 1
     
     fi_tr-time AT ROW 16.71 COL 93 COLON-ALIGNED
     period.pst AT ROW 1.29 COL 69.8 COLON-ALIGNED
          LABEL "Start Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 14.8 BY 1
          BGCOLOR 15 FONT 1
     btnCalendar-1 AT ROW 1.24 COL 88     
     period.pend AT ROW 1.24 COL 103.6 COLON-ALIGNED
          LABEL "End Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 1
     btnCalendar-2 AT ROW 1.24 COL 122.6 WIDGET-ID 6     
     period.subLedgerAP AT ROW 4.62 COL 43.6 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS 
                     "Open","O",
                     "Closed","C",
                     "Auto Close","A",
                     "NA","XX"
          DROP-DOWN-LIST
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     period.subLedgerPO AT ROW 5.91 COL 43.6 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS 
                     "Open","O",
                     "Closed","C",
                     "Auto Close","A",
                     "NA","XX"
          DROP-DOWN-LIST
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     period.subLedgerOP AT ROW 7.14 COL 43.6 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS 
                     "Open","O",
                     "Closed","C",
                     "Auto Close","A",
                     "NA","XX"
          DROP-DOWN-LIST
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     period.subLedgerWIP AT ROW 8.33 COL 43.6 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS 
                     "Open","O",
                     "Closed","C",
                     "Auto Close","A",
                     "NA","XX"
          DROP-DOWN-LIST
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     period.subLedgerRM AT ROW 9.57 COL 43.6 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS 
                     "Open","O",
                     "Closed","C",
                     "Auto Close","A",
                     "NA","XX"
          DROP-DOWN-LIST
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     period.subLedgerFG AT ROW 10.81 COL 43.6 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS 
                     "Open","O",
                     "Closed","C",
                     "Auto Close","A",
                     "NA","XX"
          DROP-DOWN-LIST
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     period.subLedgerBR AT ROW 12.1 COL 43.6 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS
                     "Open","O",
                     "Closed","C",
                     "Auto Close","A",
                     "NA","XX"
          DROP-DOWN-LIST
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     period.subLedgerAR AT ROW 13.38 COL 43.6 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS 
                     "Open","O",
                     "Closed","C",
                     "Auto Close","A",
                     "NA","XX"
          DROP-DOWN-LIST
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     Btn_OK AT ROW 16.38 COL 116
     Btn_Done AT ROW 16.67 COL 117
     Btn_Cancel AT ROW 16.38 COL 125        
     period.APClosedBy AT ROW 4.62 COL 70 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 1 
     period.POClosedBy AT ROW 5.91 COL 70 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 1 
     period.OPClosedBy AT ROW 7.14 COL 70 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 1 
     period.WIPClosedBy AT ROW 8.33 COL 70 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 1 
     period.RMClosedBy AT ROW 9.57 COL 70 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 1 
     period.FGClosedBy AT ROW 10.81 COL 70 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 1 
     period.BRClosedBy AT ROW 12.1 COL 70 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 1 
     period.ARClosedBy AT ROW 13.38 COL 70 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 1 
     period.APClosed AT ROW 4.62 COL 94.6 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 34.4 BY 1
     BGCOLOR 15 FONT 1 
     period.POClosed AT ROW 5.91 COL 94.6 COLON-ALIGNED NO-LABEL
     VIEW-AS FILL-IN 
     SIZE 34.4 BY 1
     BGCOLOR 15 FONT 1 
     period.OPClosed AT ROW 7.14 COL 94.6 COLON-ALIGNED NO-LABEL
     VIEW-AS FILL-IN 
     SIZE 34.4 BY 1
     BGCOLOR 15 FONT 1 
     period.WIPClosed AT ROW 8.33 COL 94.6 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 34.4 BY 1
     BGCOLOR 15 FONT 1 
     period.RMClosed AT ROW 9.57 COL 94.6 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 34.4 BY 1
     BGCOLOR 15 FONT 1 
     period.FGClosed AT ROW 10.81 COL 94.6 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 34.4 BY 1
     BGCOLOR 15 FONT 1 
     period.BRClosed AT ROW 12.1 COL 94.6 COLON-ALIGNED NO-LABEL
     VIEW-AS FILL-IN 
     SIZE 34.4 BY 1
     BGCOLOR 15 FONT 1 
     period.ARClosed AT ROW 13.38 COL 94.6 COLON-ALIGNED NO-LABEL 
     VIEW-AS FILL-IN 
     SIZE 34.4 BY 1
     BGCOLOR 15 FONT 1 
     period.pstat AT ROW 14.76 COL 46 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Open", yes,
                    "Closed", no
          SIZE 23 BY 1
          BGCOLOR 15 FONT 1
          
     "WIP - Work In Process" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 8.14 COL 4 WIDGET-ID 220
     "AS Of" VIEW-AS TEXT
          SIZE 12.6 BY 1.19 AT ROW 2.91 COL 110.6 WIDGET-ID 212
     "A/P - Payables" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 4.52 COL 4 WIDGET-ID 214
     "P/O - Purchasing" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 5.76 COL 4 WIDGET-ID 216
     "O/P - Order Processing" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 6.95 COL 4 WIDGET-ID 218
     "R/M - Close Inventory" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 9.48 COL 4 WIDGET-ID 222
     "F/G - Close Inventory" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 10.76 COL 4 WIDGET-ID 224
     "B/R - Bank Reconciliation" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 12.05 COL 4 WIDGET-ID 226
     "A/R - Receivables" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 13.33 COL 4 WIDGET-ID 228
     "G/L - General Ledger" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 14.62 COL 4 WIDGET-ID 230
     "Module - Description" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 2.91 COL 4 WIDGET-ID 206
     "Status" VIEW-AS TEXT
          SIZE 12 BY 1.19 AT ROW 2.91 COL 44 WIDGET-ID 208
     "Status Changed By" VIEW-AS TEXT
          SIZE 25.6 BY 1.19 AT ROW 2.91 COL 72.2 WIDGET-ID 210
     RECT-21 AT ROW 16.14 COL 115
     RECT-38 AT ROW 1 COL 1
     RECT-39 AT ROW 2.67 COL 1 WIDGET-ID 2
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     RECT-40 AT ROW 4.33 COL 40.8 WIDGET-ID 4
     RECT-41 AT ROW 2.67 COL 1 WIDGET-ID 8
     SPACE(0.99) SKIP(14.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Open Periods".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR FILL-IN fi_tr-time IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fi_tr-time:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN period.pend IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN period.pnum IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN period.pst IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN period.yr IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.period "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.period.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Open Periods */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Open Periods */
DO:
        DISABLE TRIGGERS FOR LOAD OF period .
    
        IF AVAILABLE period THEN
            op-rowid = ROWID(period) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST period EXCLUSIVE-LOCK
                WHERE RECID(period) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE period THEN DELETE period .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.

    /*APPLY "END-ERROR":U TO SELF.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 Dialog-Frame
ON CHOOSE OF btnCalendar-1 IN FRAME Dialog-Frame
DO:
    {methods/btnCalendar.i period.pst }
        APPLY "entry" TO period.pst .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period.pst Dialog-Frame
ON HELP OF period.pst IN FRAME Dialog-Frame  
DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 Dialog-Frame
ON CHOOSE OF btnCalendar-2 IN FRAME Dialog-Frame
DO:
    {methods/btnCalendar.i period.pend }
        APPLY "entry" TO period.pend .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period.pend Dialog-Frame
ON HELP OF period.pend IN FRAME Dialog-Frame  
DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
        DISABLE TRIGGERS FOR LOAD OF period .
    
        IF AVAILABLE period THEN
            op-rowid = ROWID(period) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST period EXCLUSIVE-LOCK
                WHERE RECID(period) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE period THEN DELETE period .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
        IF AVAILABLE period THEN
            ASSIGN op-rowid = ROWID(period) .
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
DO:
        
        DEFINE VARIABLE ld      AS DECIMAL NO-UNDO.
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE cAPStatus AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cPOStatus AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOPStatus AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cWIPStatus AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cRMStatus AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFGStatus AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cBRStatus AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cARStatus AS CHARACTER NO-UNDO.

        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
        
        IF ip-type EQ "New" OR ip-type EQ "Copy" THEN 
        DO:   
            CREATE period.
            op-rowid = ROWID(period).
            {methods/viewers/create/period.i}
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
   
        /*RUN valid-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-job-loc-bin-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-loc2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-loc-bin2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        
        period.tag2:SCREEN-VALUE = period.tag:SCREEN-VALUE.
        RUN valid-tag2 (OUTPUT lValidateResult) NO-ERROR.
        IF NOT lValidateResult THEN 
            RETURN NO-APPLY.  */
  
        DO TRANSACTION:             
          ASSIGN  
             cAPStatus = period.subLedgerAP 
             cPOStatus = period.subLedgerPO
             cOPStatus = period.subLedgerOP
             cWIPStatus = period.subLedgerWIP
             cRMStatus = period.subLedgerRM
             cFGStatus = period.subLedgerFG
             cBRStatus = period.subLedgerBR
             cARStatus = period.subLedgerAR.
            FIND CURRENT period EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
            
            IF cAPStatus NE period.subLedgerAP  THEN
            ASSIGN
               period.APClosedBy = USERID(LDBNAME(1))
               period.APClosed   = NOW.
            IF cPoStatus NE period.subLedgerPO THEN
            ASSIGN
               period.POClosedBy = USERID(LDBNAME(1))
               period.POClosed   = NOW.
            IF cOPStatus NE period.subLedgerOP THEN
            ASSIGN
               period.OPClosedBy = USERID(LDBNAME(1))
               period.OPClosed   = NOW.
            IF cWIPStatus NE period.subLedgerWIP THEN
            ASSIGN
               period.WIPClosedBy = USERID(LDBNAME(1))
               period.WIPClosed   = NOW.
            IF cRMStatus NE period.subLedgerRM THEN
            ASSIGN
               period.RMClosedBy = USERID(LDBNAME(1))
               period.RMClosed   = NOW.
            IF cFGStatus NE period.subLedgerFG THEN
            ASSIGN
               period.FGClosedBy = USERID(LDBNAME(1))
               period.FGClosed   = NOW.
            IF cBRStatus NE period.subLedgerBR THEN
            ASSIGN
               period.BRClosedBy = USERID(LDBNAME(1))
               period.BRClosed   = NOW.
            IF cARStatus NE period.subLedgerAR THEN
            ASSIGN
               period.ARClosedBy = USERID(LDBNAME(1))
               period.ARClosed   = NOW.
            cocode = period.company.   
        END.
        
        
        IF ip-type EQ "update" THEN
         RUN gl/reopenpr.p (RECID(period)).
         
        DO TRANSACTION:
          FIND CURRENT company.    
          IF period.pnum EQ company.num-per THEN company.yend-per = YES.
          FIND CURRENT company NO-LOCK.
        END. 

        FIND CURRENT period NO-LOCK NO-ERROR .
        op-rowid = ROWID(period).

        APPLY "go" TO FRAME {&FRAME-NAME}.  

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME period.pstat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period.pstat Dialog-Frame
ON VALUE-CHANGED OF period.pstat IN FRAME Dialog-Frame /* Period Status */
DO: 
  IF {&self-name}:SCREEN-VALUE EQ "yes" AND NOT ll-secure THEN
    RUN sys/ref/d-passwd.w (1, OUTPUT ll-secure).

  IF NOT ll-secure THEN {&self-name}:SCREEN-VALUE = "no".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME period.yr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period.yr Dialog-Frame
ON ENTRY OF period.yr IN FRAME Dialog-Frame /* Year */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME period.pend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period.pend Dialog-Frame
ON LEAVE OF period.pend IN FRAME Dialog-Frame /*  */
DO:
    DEFINE VARIABLE dDate  AS DATE NO-UNDO .
    dDate = fGetLastDate(DATE(period.pend:SCREEN-VALUE)) .

   IF DATE(period.pend:SCREEN-VALUE) LT dDate THEN
       MESSAGE "Not all days for the year are accounted for with ALL periods in the Year"
         VIEW-AS ALERT-BOX warning  .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        
     FIND FIRST company NO-LOCK
          WHERE RECID(company) EQ ip-recid2 NO-ERROR.
      op-company = IF AVAIL company THEN company.company ELSE "". 
    
    IF ip-type EQ "copy" THEN lv-item-recid = ip-recid.

    IF ip-recid EQ ? THEN 
    DO:
        RUN create-item.
    END.
    ELSE FIND period NO-LOCK WHERE RECID(period) EQ ip-recid NO-ERROR.

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

        ASSIGN 
            ll-order-warned = NO.
        btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN 
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
        btn_done:SENSITIVE                        = YES.
        btn_ok:HIDDEN                             = YES.
        btn_cancel:HIDDEN                         = YES.
    END.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Dialog-Frame 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/ 
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    IF AVAILABLE period  THEN 
    DO: 
        /*ASSIGN 
            fi_tr-time = STRING(period.trans-time,'HH:MM') .*/
        
        DISPLAY  period.yr period.pnum 
            period.pst period.pend 
            period.pstat period.SubLedgerAP period.SubLedgerPO period.SubLedgerOP period.SubLedgerWIP 
            period.SubLedgerRM period.SubLedgerFG period.SubLedgerBR period.SubLedgerAR 
            period.APClosedBy period.POClosedBy period.OPClosedBy
            period.WIPClosedBy period.RMClosedBy period.FGClosedBy period.BRClosedBy period.ARClosedBy
            period.APClosed period.POClosed period.OPClosed
            period.WIPClosed period.RMClosed period.FGClosed period.BRClosed period.ARClosed fi_tr-time
            WITH FRAME Dialog-Frame.
   END.


    IF ip-type NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.
    IF ip-type NE "New" THEN 
    DO:
        DISABLE  period.yr  period.pnum period.pstat WITH FRAME Dialog-Frame.
        IF period.SubLedgerAP EQ "" THEN        
          period.SubLedgerAP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "O" .
        IF period.SubLedgerPO EQ "" THEN   
          period.SubLedgerPO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "O" .
        IF period.SubLedgerOP EQ "" THEN   
          period.SubLedgerOP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "O" .
        IF period.SubLedgerWIP EQ "" THEN      
          period.SubLedgerWIP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "O" .
          IF period.SubLedgerRM EQ "" THEN   
          period.SubLedgerRM:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "O" .
          IF period.SubLedgerFG EQ "" THEN   
          period.SubLedgerFG:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "O" .
          IF period.SubLedgerBR EQ "" THEN   
          period.SubLedgerBR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "O" .
          IF period.SubLedgerAR EQ "" THEN   
          period.SubLedgerAR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "O" .          
        
    END.
     IF ip-type EQ "New" OR ip-type EQ "Copy" THEN 
    DO:
        ENABLE  period.yr WITH FRAME Dialog-Frame.
       
       DISABLE period.pnum 
            period.pst period.pend 
            period.pstat period.SubLedgerAP period.SubLedgerPO period.SubLedgerOP period.SubLedgerWIP 
            period.SubLedgerRM period.SubLedgerFG period.SubLedgerBR period.SubLedgerAR WITH FRAME Dialog-Frame.
    END.
    fi_tr-time:HIDDEN IN FRAME Dialog-Frame           = TRUE.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY fi_tr-time 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE period THEN 
    DISPLAY period.yr period.pnum period.pst period.pend period.pstat 
          period.subLedgerAP period.subLedgerPO period.subLedgerOP 
          period.subLedgerWIP period.subLedgerRM period.subLedgerFG 
          period.subLedgerBR period.subLedgerAR period.APClosedBy period.POClosedBy period.OPClosedBy
          period.WIPClosedBy period.RMClosedBy period.FGClosedBy period.BRClosedBy period.ARClosedBy
          period.APClosed period.POClosed period.OPClosed
          period.WIPClosed period.RMClosed period.FGClosed period.BRClosed period.ARClosed
      WITH FRAME Dialog-Frame.
  ENABLE period.yr period.pnum btnCalendar-1 period.pst period.pend 
         period.pstat period.subLedgerAP period.subLedgerPO period.subLedgerOP 
         period.subLedgerWIP period.subLedgerRM period.subLedgerFG 
         period.subLedgerBR period.subLedgerAR 
          Btn_OK Btn_Done Btn_Cancel 
         btnCalendar-2 RECT-21 RECT-38 RECT-39 RECT-40 RECT-41 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit Dialog-Frame 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */   


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetLastDate Dialog-Frame
FUNCTION fGetLastDate RETURNS DATE PRIVATE
  ( ipdDate AS DATE  ):
/*------------------------------------------------------------------------------
 Purpose: Gets the Taxable flag based on inputs
 Notes:
------------------------------------------------------------------------------*/
 return add-interval( date( month( ipdDate ), 1, year( ipdDate )), 1, "month" ) - 1.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
