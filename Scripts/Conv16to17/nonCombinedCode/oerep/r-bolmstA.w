&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-bolmst.w

  Description: Master BOL Printing

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: JLF

  Created: 02/21/05

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

{oerep/r-bolmst.i NEW}

DEF VAR lv-cust-no LIKE oe-bolh.cust-no NO-UNDO.

def var v-print-fmt as char no-undo format 'x'.
def var v-headers   as log no-undo.
def var v-print-coc as log no-undo.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.

{custom/xprint.i}

DEF VAR lv-prt-bypass AS LOG NO-UNDO.  /* bypass window's printer driver */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES soldto

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define FIELDS-IN-QUERY-FRAME-A soldto.sold-name soldto.sold-addr[1] ~
soldto.sold-addr[2] soldto.sold-city soldto.sold-state soldto.sold-zip 
&Scoped-define QUERY-STRING-FRAME-A FOR EACH soldto SHARE-LOCK
&Scoped-define OPEN-QUERY-FRAME-A OPEN QUERY FRAME-A FOR EACH soldto SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-A soldto
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-A soldto


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 RECT-9 begin_trailer ~
begin_sold-id rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-FIELDS soldto.sold-name soldto.sold-addr[1] ~
soldto.sold-addr[2] soldto.sold-city soldto.sold-state soldto.sold-zip 
&Scoped-define DISPLAYED-TABLES soldto
&Scoped-define FIRST-DISPLAYED-TABLE soldto
&Scoped-Define DISPLAYED-OBJECTS begin_trailer begin_sold-id rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_sold-id AS CHARACTER FORMAT "X(8)" 
     LABEL "Dist Ctr #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_trailer AS CHARACTER FORMAT "X(8)" 
     LABEL "Trailer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 23 BY 7.86 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.67.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 3.1.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 6.91.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FRAME-A FOR 
      soldto SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_trailer AT ROW 3.86 COL 40 COLON-ALIGNED HELP
          "Enter Trailer Number"
     begin_cust AT ROW 6.95 COL 65 COLON-ALIGNED HELP
          "Enter Trailer Number" NO-LABEL
     begin_sold-id AT ROW 7.19 COL 17 COLON-ALIGNED HELP
          "Enter Trailer Number"
     soldto.sold-name AT ROW 8.14 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     soldto.sold-addr[1] AT ROW 9.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     soldto.sold-addr[2] AT ROW 10.05 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     soldto.sold-city AT ROW 11 COL 17 COLON-ALIGNED
          LABEL "City State Zip"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     soldto.sold-state AT ROW 11 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     soldto.sold-zip AT ROW 11 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     rd-dest AT ROW 13.86 COL 5 NO-LABEL
     lv-ornt AT ROW 15.76 COL 30 NO-LABEL
     lines-per-page AT ROW 15.76 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 17.19 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 18.14 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 20.52 COL 29
     btn-ok AT ROW 23.14 COL 21
     btn-cancel AT ROW 23.14 COL 61
     "DISTRIBUTION CENTER ID AND ADDRESS" VIEW-AS TEXT
          SIZE 45 BY 1 AT ROW 6 COL 24
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.91 COL 5
     "Master Bill of Lading/Shipping Manifest Trailer" VIEW-AS TEXT
          SIZE 44 BY 1 AT ROW 2.43 COL 25
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-6 AT ROW 12.67 COL 1
     RECT-7 AT ROW 1 COL 1
     RECT-8 AT ROW 2.19 COL 24
     RECT-9 AT ROW 5.52 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 95.8 BY 24.33.


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
         TITLE              = "Print Master BOL"
         HEIGHT             = 24.38
         WIDTH              = 95.8
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
                                                                        */
/* SETTINGS FOR FILL-IN begin_cust IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       begin_cust:HIDDEN IN FRAME FRAME-A           = TRUE
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_sold-id:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_trailer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN soldto.sold-addr[1] IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN soldto.sold-addr[2] IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN soldto.sold-city IN FRAME FRAME-A
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN soldto.sold-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN soldto.sold-state IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN soldto.sold-zip IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _TblList          = "asi.soldto"
     _Query            is OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Master BOL */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Master BOL */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR look-recid AS RECID NO-UNDO.


  CASE FOCUS:NAME:
    WHEN "begin_trailer" THEN DO:
      RUN windows/l-trailr.w (g_company, begin_trailer:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
      IF char-val NE "" THEN DO:
        begin_trailer:SCREEN-VALUE = ENTRY(1,char-val).
        RUN get-cust-no.
      END.
    END.
    WHEN "begin_sold-id" THEN DO:
      RUN windows/l-soldto.w (g_company, lv-cust-no, begin_sold-id:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" THEN DO:
        begin_sold-id:SCREEN-VALUE = ENTRY(2,char-val).
        RUN new-sold-id.
      END.
    END.
  END.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-trailer NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_sold-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_sold-id C-Win
ON LEAVE OF begin_sold-id IN FRAME FRAME-A /* Dist Ctr # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sold-id NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_sold-id C-Win
ON VALUE-CHANGED OF begin_sold-id IN FRAME FRAME-A /* Dist Ctr # */
DO:
  RUN new-sold-id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_trailer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_trailer C-Win
ON LEAVE OF begin_trailer IN FRAME FRAME-A /* Trailer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-trailer NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:09 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  RUN valid-trailer NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-sold-id NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    begin_cust:SCREEN-VALUE = lv-cust-no.
    ASSIGN {&displayed-objects}.
  END.

  RUN run-report NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &TYPE="MASTER BOL"
                            &begin_cust=begin_cust
                            &end_cust=begin_cust
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
              {custom/asimail.i &TYPE = "MASTER BOL"
                             &begin_cust=begin_cust
                             &end_cust=begin_cust
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "MASTER BOL"
                                  &begin_cust=begin_cust
                                  &end_cust=begin_cust
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN output-to-port.
  end case. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:09 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
   ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
DO:
  {custom/chgfont.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
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
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:09 am */
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

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO begin_trailer.
    lines-per-page:SCREEN-VALUE = STRING(62).
    DISABLE lines-per-page.
    RELEASE soldto.
    ASSIGN soldto.sold-name:SCREEN-VALUE = ""
           soldto.sold-addr[1]:SCREEN-VALUE = ""
           soldto.sold-addr[2]:SCREEN-VALUE = ""
           soldto.sold-city:SCREEN-VALUE = ""
           soldto.sold-state:SCREEN-VALUE = ""
           soldto.sold-zip:SCREEN-VALUE = "".
      {methods/setButton.i btn-cancel "Cancel"}
      {methods/setButton.i btn-ok "OK"}
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:09 am */
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

  {&OPEN-QUERY-FRAME-A}
  GET FIRST FRAME-A.
  DISPLAY begin_trailer begin_sold-id rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  IF AVAILABLE soldto THEN 
    DISPLAY soldto.sold-name soldto.sold-addr[1] soldto.sold-addr[2] 
          soldto.sold-city soldto.sold-state soldto.sold-zip 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-8 RECT-9 begin_trailer begin_sold-id rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-cust-no C-Win 
PROCEDURE get-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  lv-cust-no = "".

  DO WITH FRAME {&FRAME-NAME}:
    IF begin_trailer:SCREEN-VALUE NE "" THEN
    FOR EACH oe-bolh
        WHERE oe-bolh.company EQ cocode
          AND oe-bolh.posted  EQ NO
          AND oe-bolh.printed EQ YES
          AND oe-bolh.trailer EQ begin_trailer:SCREEN-VALUE
          AND CAN-FIND(FIRST oe-boll
                       WHERE oe-boll.company EQ oe-bolh.company
                         AND oe-boll.b-no    EQ oe-bolh.b-no
                         AND oe-boll.ord-no  NE 0)
        USE-INDEX post NO-LOCK:
      lv-cust-no = oe-bolh.cust-no.
      LEAVE.
    END.

    IF lv-cust-no NE ""                 AND
       begin_sold-id:SCREEN-VALUE EQ "" THEN DO:
      FIND FIRST soldto
          WHERE soldto.company EQ cocode
            AND soldto.cust-no EQ lv-cust-no
            AND soldto.sold-id EQ lv-cust-no
          NO-LOCK NO-ERROR.
      IF AVAIL soldto THEN DO:
        begin_sold-id:SCREEN-VALUE = soldto.sold-id.
        RUN new-sold-id.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-sold-id C-Win 
PROCEDURE new-sold-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF lv-cust-no NE "" THEN
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST soldto
        WHERE soldto.company EQ cocode
          AND soldto.cust-no EQ lv-cust-no
          AND soldto.sold-id EQ begin_sold-id:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL soldto THEN
      DISPLAY soldto.sold-name
              soldto.sold-addr
              soldto.sold-city
              soldto.sold-state
              soldto.sold-zip.
  END.

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
 /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.
   */
   {custom/out2file.i}



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN custom/d-print.w(list-name).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
*/
 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE IF lv-prt-bypass THEN RUN custom/d-print.w (list-name).
  ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE /*run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ */
       run custom/scr-rpt2.w (list-name,c-win:title,int(lv-font-no),lv-ornt,lv-prt-bypass).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------- oe/rep/oe-mast.p 05/97 FWK  */
/* Master BOL Print                                                           */
/* __________________________________________________________________________ */

{sys/form/r-top.i}

def var unit-pkg like oe-boll.cases format ">>>,>>>" initial 0 no-undo.
def var unit-weight like oe-boll.weight initial 0 no-undo.
def var tot-pkg like oe-boll.cases format ">>>,>>>" initial 0 no-undo.
def var tot-weight like oe-boll.weight initial 0 no-undo.
def var v-num-cases like oe-boll.cases no-undo.
def var v-weight like oe-boll.weight no-undo.
def var v-cust-div like edpotran.cust-div no-undo.
def var v-ship-id like oe-bolh.ship-id no-undo.

FORM
  v-num-cases format ">>>,>>9" TO 15
/*
  fgcat.dscr AT 23
*/
  "NMFC29280_GIFT_BOX" AT 23
  oe-bolh.po-no format "x(8)"
  v-cust-div TO 55 format "x(3)"
/*
  fgcat.procat TO 55 format "x(3)"
  oe-boll.weight TO 63
*/
  v-weight TO 63
  WITH FRAME mast-bol-line DOWN NO-LABELS NO-BOX STREAM-IO WIDTH 85.

FORM
/*
  oe-bolh.ship-id TO 18
*/
  v-ship-id TO 18
  shipto.ship-city AT 23
  shipto.ship-state
  v-cust-div TO 55 format "x(3)"
/*
  fgcat.procat TO 55
*/
  oe-bolh.po-no format "x(8)" AT 57
  v-num-cases format ">>>,>>9" TO 72
  v-weight TO 80
  WITH FRAME ship-man-line DOWN NO-LABELS NO-BOX STREAM-IO WIDTH 85.

FORM
  "UNIT TOTAL" AT 23
  unit-pkg TO 72
  unit-weight TO 80
  WITH FRAME unit-tot DOWN NO-LABELS NO-BOX STREAM-IO WIDTH 85.

FORM
  skip(1)
  "-------" TO 15
  "-------" TO 63
  tot-pkg TO 15
  tot-weight TO 63
  WITH FRAME totals-man DOWN NO-LABELS NO-BOX STREAM-IO WIDTH 85.

FORM
  skip(1)
  "TOTALS" AT 30
  tot-pkg TO 72
  tot-weight TO 80
  WITH FRAME totals-ship DOWN NO-LABELS NO-BOX STREAM-IO WIDTH 85.

FIND FIRST soldto
    WHERE soldto.company EQ cocode
      AND soldto.cust-no EQ lv-cust-no
      AND soldto.sold-id EQ begin_sold-id
    NO-LOCK NO-ERROR.
IF NOT AVAIL soldto THEN RETURN ERROR.

FORM HEADER
        skip(4)
        soldto.sold-name AT 51 skip
        soldto.sold-addr AT 51 skip
        soldto.sold-city AT 51 space(0) "," soldto.sold-state soldto.sold-zip
        skip(3)
        "MASTER BILL OF LADING" AT 26
        "BILL OF LADING #" AT 58 v-mast-bol-no TO 80 skip(2)
        "# PACKAGES" AT 8
        "DESCRIPTION" AT 23
        "DIV" TO 55
        "WEIGHT" AT 57
        "CLASS" AT 65
        "CHECK" AT 76 skip(1)
        WITH FRAME mast-bol-hdr DOWN PAGE-TOP NO-LABELS NO-BOX STREAM-IO WIDTH 85.

  FORM HEADER
        skip(4)
        soldto.sold-name AT 51 skip
        soldto.sold-addr AT 51 skip
        soldto.sold-city AT 51 space(0) "," soldto.sold-state soldto.sold-zip
        skip(3)
        "SHIPPING MANIFEST" AT 26
        "BILL OF LADING #" AT 58 v-mast-bol-no TO 80 skip(2)
        "FOR UNIT #" AT 8
        "CITY" AT 23
        "ST" AT 39
        "DIV" AT 50
        "P.O. #" AT 57
        "# PKGS" AT 66
        "WEIGHT" AT 75 skip(1)
        WITH FRAME ship-man-hdr DOWN PAGE-TOP NO-LABELS NO-BOX STREAM-IO WIDTH 85.


assign
 v-trailer = begin_trailer
 v-emp-id  = begin_sold-id.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

view frame mast-bol-hdr.

  assign tot-pkg = 0
         tot-weight = 0.

  FOR EACH oe-bolh WHERE oe-bolh.company = cocode AND
      oe-bolh.printed eq yes       AND
      oe-bolh.posted  eq no        AND
      oe-bolh.trailer eq v-trailer AND
      CAN-FIND(FIRST oe-boll
               WHERE oe-boll.company EQ oe-bolh.company
                 AND oe-boll.b-no    EQ oe-bolh.b-no
                 AND oe-boll.ord-no  NE 0)
      no-lock USE-INDEX post
      BREAK BY oe-bolh.bol-date BY oe-bolh.trailer BY oe-bolh.bol-no:

     /* it is not possible to check for reprint, so just delete them
        if they are already present */
    if first-of (oe-bolh.trailer) then do:
        if connected("edi") then do:
            run edi/ed/asi/o856del.p (input recid(oe-bolh)).
        end.
    end.

    assign v-cust-div = "".
    /* Use EDI files to print Customer Division */
    find first edmast where edmast.cust eq oe-bolh.cust-no 
                            use-index customer no-lock no-error.
    if avail edmast then
    do:
      RELEASE eddoc.
      FIND FIRST oe-boll
          WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no
            AND oe-boll.ord-no  NE 0
          NO-LOCK NO-ERROR.
      IF AVAIL oe-boll THEN
      find first eddoc where eddoc.partner eq edmast.partner 
                         and eddoc.unique-order-no eq oe-boll.ord-no
                       no-lock no-error.
      if avail eddoc then
        find edpotran where edpotran.partner eq eddoc.partner 
                        and edpotran.seq eq edmast.seq
                                no-lock no-error.

      if not avail edpotran then
        find last edpotran where edpotran.partner eq edmast.partner 
                           and edpotran.cust-po eq oe-bolh.po-no
                                no-lock no-error.
      if avail edpotran then
        assign v-cust-div = edpotran.cust-div.
    end.

    for each oe-boll where oe-boll.company = cocode and
                           oe-boll.bol-no = oe-bolh.bol-no
                           no-lock:
      find first itemfg where itemfg.company = cocode and
                              itemfg.i-no = oe-boll.i-no no-lock no-error.
      if avail itemfg then
        find first fgcat where fgcat.company = cocode and
                               fgcat.procat = itemfg.procat no-lock no-error.

      assign v-num-cases = v-num-cases + oe-boll.cases
             v-weight = v-weight + oe-boll.weight.

      if oe-boll.partial > 0 then
        assign v-num-cases = v-num-cases + 1.

    end.

    assign tot-pkg = tot-pkg + v-num-cases
           tot-weight = tot-weight + v-weight.

    display v-num-cases
            oe-bolh.po-no
            v-cust-div
/*
            fgcat.procat when avail fgcat
*/
            v-weight with down frame mast-bol-line.
    down with frame mast-bol-line.
    assign v-num-cases = 0
           v-weight = 0.
  end.

  display tot-pkg tot-weight with frame totals-man.

  hide frame mast-bol-hdr no-pause.

  page.

  view frame ship-man-hdr.

  assign tot-pkg = 0
         tot-weight = 0.

  FOR EACH oe-bolh WHERE oe-bolh.company = cocode AND
      oe-bolh.printed eq yes       AND
      oe-bolh.posted  eq no        AND
      oe-bolh.trailer eq v-trailer AND
      CAN-FIND(FIRST oe-boll
               WHERE oe-boll.company EQ oe-bolh.company
                 AND oe-boll.b-no    EQ oe-bolh.b-no
                 AND oe-boll.ord-no  NE 0)
      no-lock USE-INDEX post BREAK BY oe-bolh.ship-id:

    if first-of(oe-bolh.ship-id) then
      assign unit-pkg = 0
             unit-weight = 0.

    assign v-cust-div = "".
    /* Use EDI files to print Customer Division */
    find first edmast where edmast.cust eq oe-bolh.cust-no 
                            use-index customer no-lock no-error.
    if avail edmast then
    do:
      RELEASE eddoc.
      FIND FIRST oe-boll
          WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no
            AND oe-boll.ord-no  NE 0
          NO-LOCK NO-ERROR.
      IF AVAIL oe-boll THEN
      find first eddoc where eddoc.partner eq edmast.partner 
                         and eddoc.unique-order-no eq oe-boll.ord-no
                       no-lock no-error.
      if avail eddoc then
        find edpotran where edpotran.partner eq eddoc.partner 
                        and edpotran.seq eq edmast.seq
                                no-lock no-error.

      if not avail edpotran then
        find last edpotran where edpotran.partner eq edmast.partner 
                           and edpotran.cust-po eq oe-bolh.po-no
                                no-lock no-error.
      if avail edpotran then
        assign v-cust-div = edpotran.cust-div.
    end.

    find first shipto where shipto.company = cocode and
                            shipto.ship-id = oe-bolh.ship-id and
                            shipto.cust-no = oe-bolh.cust-no
                            use-index ship-id no-lock no-error.

    for each oe-boll where oe-boll.company = cocode and
                           oe-boll.bol-no = oe-bolh.bol-no
                           no-lock:

      assign v-num-cases = v-num-cases + oe-boll.cases
             v-weight = v-weight + oe-boll.weight
             unit-pkg = unit-pkg + oe-boll.cases.

      if oe-boll.partial > 0 then
        assign v-num-cases = v-num-cases + 1
               unit-pkg = unit-pkg + 1.

/*
      assign unit-pkg = unit-pkg + v-num-cases
*/
      assign unit-weight = unit-weight + oe-boll.weight
             tot-weight = tot-weight + oe-boll.weight.


      find first itemfg where itemfg.company = cocode and
                              itemfg.i-no = oe-boll.i-no no-lock no-error.
      if avail itemfg then
        find first fgcat where fgcat.company = cocode and
                               fgcat.procat = itemfg.procat no-lock no-error.

    end.

    if substring(oe-bolh.ship-id,1,2) eq "73" then
      assign v-ship-id = 
                      substring(oe-bolh.ship-id,3,length(oe-bolh.ship-id) - 2).
    else
      assign v-ship-id = oe-bolh.ship-id.

    assign tot-pkg = tot-pkg + v-num-cases.

      display v-ship-id
/*
              oe-bolh.ship-id
*/
              shipto.ship-city
              shipto.ship-state
              v-cust-div
/*
              fgcat.procat when avail fgcat
*/
              oe-bolh.po-no
              v-num-cases
              v-weight with down frame ship-man-line.
      down with frame ship-man-line.
    assign v-num-cases = 0
           v-weight = 0.

      if last-of(oe-bolh.ship-id) and NOT first-of(oe-bolh.ship-id) then
        display unit-pkg unit-weight with frame unit-tot.
/* WFK - Feb 7, 2016 
   remove edi references as edi was not converted from character */  

/*    {edi/ed/asi/o856hook.i &reprint=true &force_asn=false}*/

  end. /* oe-bolh */

  display tot-pkg tot-weight with frame totals-ship.
  assign tot-pkg = 0
         tot-weight = 0.

  FOR EACH oe-bolh WHERE oe-bolh.company = cocode AND
      oe-bolh.printed eq yes       AND
      oe-bolh.posted  eq no        AND
      oe-bolh.trailer eq v-trailer AND
      CAN-FIND(FIRST oe-boll
               WHERE oe-boll.company EQ oe-bolh.company
                 AND oe-boll.b-no    EQ oe-bolh.b-no
                 AND oe-boll.ord-no  NE 0)
      USE-INDEX post BREAK BY oe-bolh.ship-id:
    assign oe-bolh.master-bol-no = v-mast-bol-no
           oe-bolh.master-bol-printed = yes.
  END.

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

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
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sold-id C-Win 
PROCEDURE valid-sold-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN get-cust-no.

    FIND FIRST soldto
        WHERE soldto.company EQ cocode
          AND soldto.cust-no EQ lv-cust-no
          AND soldto.sold-id EQ begin_sold-id:SCREEN-VALUE
        NO-LOCK NO-ERROR.

    IF NOT AVAIL soldto THEN DO:
      APPLY "entry" TO begin_sold-id.
      MESSAGE TRIM(begin_sold-id:LABEL) +
              " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-trailer C-Win 
PROCEDURE valid-trailer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN get-cust-no.

    IF lv-cust-no EQ "" THEN DO:
      APPLY "entry" TO begin_trailer.
      MESSAGE TRIM(begin_trailer:LABEL) +
              " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

