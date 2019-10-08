&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-rele&p.w

  Description: Order Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 04/11/2002

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
&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN
DEF VAR ip-post AS LOG NO-UNDO.
&ELSE
DEF INPUT PARAMETER ip-post AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR lv-last-assigned AS CHAR NO-UNDO.
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

{oe/oe-relp1.i NEW}

DEF TEMP-TABLE tt-user-print LIKE user-print.
DEF TEMP-TABLE tt-date FIELD tt-name AS cha
                       FIELD tt-value AS cha.
def buffer xfg-bin for fg-bin.

def var v-ship-lu as ch initial ["I,S,B"].
def var v-ship-no as int.
def var v-s-code as char.
def var v-no-post as int format ">>>9".
def var v-tot-post as int format ">>>9".
def var v-first-release as log.
def var v-r-no like inv-head.r-no.
def var v-ext-price like inv-line.t-price.
def var v-nxt-r-no as int.
def var v-po-no like oe-rel.po-no.
def var v-royal as log.
def var v-n-bol like oe-ctrl.n-bol.
def var v-bol-qty like oe-boll.qty.
def var temp-tax as dec init 0 no-undo.
def var v-hold-list as CHAR NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.

DEF STREAM s-temp.

def TEMP-TABLE w-nopost no-undo
  field ord-no like oe-relh.ord-no
  field rel-date like oe-relh.rel-date
  field rel-no like oe-rell.rel-no
  field b-ord-no like oe-relh.b-ord-no
  field cust-no like oe-relh.cust-no
  field ship-id like oe-relh.ship-id
  field i-no like oe-rell.i-no
  field part-no like itemfg.part-no
  field i-name as char format "x(25)"
  field loc like oe-rell.loc
  field loc-bin like oe-rell.loc-bin
  field tag like oe-rell.tag
  field qty like oe-rell.qty
  field code as char format "x(5)"
  FIELD link-no LIKE oe-rell.link-no.

DEF VAR  v-chkflg AS LOG NO-UNDO.

{sys/ref/relpost.i}

{sys/inc/relcrhold.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_relnum ~
end_relnum begin_date end_date begin_cust end_cust begin_ord end_ord ~
tgMultipleReleases rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_text-1 tran-date tran-period ~
begin_relnum end_relnum begin_date end_date begin_cust end_cust begin_ord ~
end_ord tgMultipleReleases rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Release Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_relnum AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Release#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Release Date#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_relnum AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Release#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-relep.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE fi_text-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Selection Parameters" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY .95
     BGCOLOR 2  NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

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
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.67.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tgMultipleReleases AS LOGICAL INITIAL yes 
     LABEL "Multiple Releases" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_text-1 AT ROW 1.24 COL 3 COLON-ALIGNED NO-LABEL
     tran-date AT ROW 2.43 COL 36 COLON-ALIGNED
     tran-period AT ROW 2.43 COL 64 COLON-ALIGNED
     begin_relnum AT ROW 3.86 COL 28 COLON-ALIGNED HELP
          "Enter the beginning release number"
     end_relnum AT ROW 3.86 COL 73 COLON-ALIGNED HELP
          "Enter the ending release number"
     begin_date AT ROW 4.81 COL 28 COLON-ALIGNED HELP
          "Enter the beginning release date"
     end_date AT ROW 4.81 COL 73 COLON-ALIGNED HELP
          "Enter the ending release date"
     begin_cust AT ROW 5.76 COL 28 COLON-ALIGNED HELP
          "Enter the beginning customer number"
     end_cust AT ROW 5.76 COL 73 COLON-ALIGNED HELP
          "Enter the ending customer number"
     begin_ord AT ROW 6.71 COL 28 COLON-ALIGNED HELP
          "Enter the beginning order number"
     end_ord AT ROW 6.71 COL 73 COLON-ALIGNED HELP
          "Enter the ending order number"
     tgMultipleReleases AT ROW 7.67 COL 30 WIDGET-ID 8
     rd-dest AT ROW 14.57 COL 4 NO-LABEL
     lv-ornt AT ROW 14.81 COL 28 NO-LABEL
     lines-per-page AT ROW 14.81 COL 81 COLON-ALIGNED
     lv-font-no AT ROW 16.24 COL 31 COLON-ALIGNED
     lv-font-name AT ROW 17.19 COL 25 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.43 COL 27
     tb_excel AT ROW 19.48 COL 27 WIDGET-ID 2
     tb_runExcel AT ROW 19.48 COL 49.4 WIDGET-ID 4
     fi_file AT ROW 20.67 COL 25 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 6
     btn-ok AT ROW 22.67 COL 23
     btn-cancel AT ROW 22.67 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.14 COL 5
     RECT-6 AT ROW 13.38 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 25.

DEFINE FRAME FRAME-E
     "Release tickets MUST BE Printed prior to posting!" VIEW-AS TEXT
          SIZE 61 BY 1.19 AT ROW 4.1 COL 19
          BGCOLOR 11 FONT 5
     "The Edit List will show all available releases to be" VIEW-AS TEXT
          SIZE 61 BY 1.19 AT ROW 1.71 COL 19
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "posted to all orders." VIEW-AS TEXT
          SIZE 26 BY 1.19 AT ROW 2.91 COL 19
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 8.62
         SIZE 94 BY 4.52
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
         TITLE              = "Release Edit List & Posting"
         HEIGHT             = 23.33
         WIDTH              = 96.6
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-E:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_relnum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_relnum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fi_text-1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tgMultipleReleases:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-E
                                                                        */
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
ON END-ERROR OF C-Win /* Release Edit List  Posting */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Release Edit List  Posting */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Release Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord C-Win
ON LEAVE OF begin_ord IN FRAME FRAME-A /* Beginning Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_relnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_relnum C-Win
ON LEAVE OF begin_relnum IN FRAME FRAME-A /* Beginning Release# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:08 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.


  run check-date.
  if v-invalid then return no-apply.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  ASSIGN
   v-frel     = begin_relnum
   v-trel     = end_relnum
   v-fdat     = begin_date
   v-tdat     = end_date
   v-fcus     = begin_cust
   v-tcus     = end_cust
   v-ford     = begin_ord
   v-tord     = end_ord
   v-no-post  = 0
   v-tot-post = 0.

IF tgMultipleReleases:SCREEN-VALUE NE "YES" THEN
    ASSIGN END_relnum = begin_relnum v-trel = begin_relnum.


  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.
  SESSION:SET-WAIT-STATE ("").

  IF ip-post THEN DO:
    IF v-tot-post GT 0 THEN DO:
      lv-post = NO.

      MESSAGE "Post Releases?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE lv-post.

      IF lv-post THEN do:
        RUN post-releases.

        MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
      END.
    END.

    ELSE MESSAGE "No releases available for posting..." VIEW-AS ALERT-BOX ERROR.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:08 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Release Date# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord C-Win
ON LEAVE OF end_ord IN FRAME FRAME-A /* Ending Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_relnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_relnum C-Win
ON LEAVE OF end_relnum IN FRAME FRAME-A /* Ending Release# */
DO:
  ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tgMultipleReleases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgMultipleReleases C-Win
ON VALUE-CHANGED OF tgMultipleReleases IN FRAME FRAME-A /* Multiple Releases */
DO:
  ASSIGN {&SELF}.
  IF tgMultipleReleases:SCREEN-VALUE EQ "YES" THEN DO:
      ASSIGN END_relnum:VISIBLE = TRUE begin_relnum:LABEL = "Beginning Release#".
      ENABLE END_relnum.
      END_relnum:SENSITIVE = YES.
  END.
  ELSE DO:
      ASSIGN END_relnum:VISIBLE = FALSE
           begin_relnum:LABEL = "Release#"
           END_relnum:SCREEN-VALUE =  string(begin_relnum)  .
      DISABLE END_relnum.
      END_relnum:SENSITIVE = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
DO:
  assign {&self-name}.

  if lastkey ne -1 then do:
    run check-date.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
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
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:08 am */
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

  assign
   tran-date   = today
   begin_date  = TODAY
   end_date    = TODAY
   c-win:TITLE = IF ip-post THEN "Release Posting/Create BOL"
                            ELSE "Release Edit List".


  v-hold-list = "Royal,Superior,ContSrvc,BlueRidg,Danbury".

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLFMT"
      no-lock no-error.
  v-royal = avail sys-ctrl and lookup(sys-ctrl.char-fld,v-hold-list) ne 0.

  RUN enable_UI.

  ASSIGN tgMultipleReleases.
  IF tgMultipleReleases:SCREEN-VALUE NE "YES" THEN DO:
      ASSIGN END_relnum:VISIBLE = FALSE begin_relnum:LABEL = "Release#".
      DISABLE END_relnum.
      END_relnum:SENSITIVE = NO.
  END.
  RUN check-date.

/*  IF ip-post THEN DO:     */
/*    fi_text-1:BGCOLOR = ?.*/
/*    DISPLAY fi_text-1.    */
/*  END.                    */

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
      IF NOT ip-post THEN DO:
          {custom/usrprint.i}
          /*ASSIGN tgMultipleReleases.*/  
          APPLY "entry" TO begin_relnum.  
          DISABLE tran-date tran-period.
          /* In case this changes after usrprint */
          IF tgMultipleReleases:SCREEN-VALUE NE "YES" THEN DO WITH FRAME {&FRAME-NAME}:
               ASSIGN END_relnum:VISIBLE = FALSE begin_relnum:LABEL = "Release#".
               DISABLE END_relnum.
               END_relnum:SENSITIVE = NO.
          END.    
      END.
      ELSE DO:
          fi_text-1:BGCOLOR = ?.
          DISPLAY fi_text-1.
          {custom/usrprintv.i "tgMultipleReleases"}
          /* ASSIGN tgMultipleReleases. */
          /* In case this changes after usrprint */
          IF lv-last-assigned NE "YES" THEN DO:
             RUN disable-multi-release.
          END.
      END.
      {methods/setButton.i btn-cancel "Cancel"}
      {methods/setButton.i btn-ok "OK"}
  END.
  {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:08 am */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO with frame {&frame-name}:
    v-invalid = no.

    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period then tran-period:SCREEN-VALUE = string(period.pnum).

    else
    IF ip-post THEN DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      v-invalid = yes.
    end.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-multi-release C-Win 
PROCEDURE disable-multi-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   ASSIGN END_relnum:VISIBLE = FALSE begin_relnum:LABEL = "Release#".
   DISABLE END_relnum.
   END_relnum:SENSITIVE = NO.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY fi_text-1 tran-date tran-period begin_relnum end_relnum begin_date 
          end_date begin_cust end_cust begin_ord end_ord tgMultipleReleases 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
          tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_relnum end_relnum begin_date end_date 
         begin_cust end_cust begin_ord end_ord tgMultipleReleases rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-E IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-E}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exception-rpt C-Win 
PROCEDURE exception-rpt :
{oe/relexcpt.i}

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
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.


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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no), lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-releases C-Win 
PROCEDURE post-releases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ll-exception AS LOG NO-UNDO.

DEF BUFFER upd-oe-relh FOR oe-relh.
DEF BUFFER upd-oe-rell FOR oe-rell.

{sa/sa-sls01.i}

FOR EACH tt-except:
    DELETE tt-except.
END.

FOR EACH tt-fg-bin:
    DELETE tt-fg-bin.
END.

DISABLE TRIGGERS FOR LOAD OF itemfg.

if relpost-chr eq "Nothing" then
headblok:
for each oe-relh FIELDS(company r-no deleted) no-lock
    where oe-relh.company  eq cocode
      and oe-relh.posted   eq no
      and oe-relh.printed  eq yes
      and oe-relh.release# ge v-frel
      and oe-relh.release# le v-trel
      and oe-relh.rel-date ge v-fdat
      and oe-relh.rel-date le v-tdat
      and oe-relh.cust-no  ge v-fcus
      and oe-relh.cust-no  le v-tcus
     /* gdm - 03110907 */
      AND (NOT v-chkflg OR
           (v-chkflg AND oe-relh.w-ord = NO))
      /* gdm - 03110907 end */
      and not can-find(first oe-rell
                       where oe-rell.company eq oe-relh.company
                         and oe-rell.r-no    eq oe-relh.r-no
                         and (oe-rell.ord-no lt v-ford or
                              oe-rell.ord-no gt v-tord or
                              oe-rell.s-code eq "I")
                       USE-INDEX r-no)
    use-index post.

  RUN oe/relcheck.p (ROWID(oe-relh), OUTPUT ll-exception).
  IF ll-exception THEN NEXT headblok.


  {oe/oe-relp.i}
END. /* each oe-relh */

RUN oe/oe-relp2.p (v-term, v-royal).

FOR EACH report WHERE report.term-id EQ v-term EXCLUSIVE-LOCK, 
    FIRST oe-boll
    WHERE RECID(oe-boll) EQ report.rec-id
      AND CAN-FIND(FIRST oe-bolh
                   WHERE oe-bolh.b-no    EQ oe-boll.b-no
                     AND oe-bolh.printed EQ NO)
    NO-LOCK:
  DELETE report.
END.

RUN oe/oe-bolp3.p (v-term).

delete-blok:
FOR EACH oe-relh NO-LOCK
    WHERE oe-relh.company EQ cocode
      AND oe-relh.deleted EQ YES
    USE-INDEX deleted:

  FIND upd-oe-relh WHERE ROWID(upd-oe-relh) EQ ROWID(oe-relh) EXCLUSIVE NO-WAIT NO-ERROR.

  IF AVAIL upd-oe-relh THEN DO:
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no:
      FIND upd-oe-rell WHERE ROWID(upd-oe-rell) EQ ROWID(oe-rell) EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL upd-oe-rell THEN DELETE upd-oe-rell.
      ELSE UNDO delete-blok, NEXT delete-blok.
    END. /* each oe-rell */
    DELETE upd-oe-relh.
  END.
END.

find first tt-except no-error.
if avail tt-except then do:
  ll-exception = YES.
  MESSAGE "  Releases Tickets have been found that are in use  "    skip
          "  or have insufficient inventory for posting to be  "    skip
          "  completed.  Do you wish to print the exceptions?  "
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll-exception.

  IF ll-exception THEN do:
    run exception-rpt.

    case rd-dest:
         when 1 then run output-to-printer.
         when 2 then run output-to-screen.
         when 3 then run output-to-file.
    end case.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------- oe/rep/neword.p 8/93 rd  */
/* Order Entry - Edit Register                                                */
/*  FOR: Order Status of - (N)ew, (A)pproved Credit, (U)pdated, (D)eleted,    */
/*                          & (C)losed                                        */
/* -------------------------------------------------------------------------- */

DEF VAR v-export    AS LOG                 NO-UNDO.
DEF VAR v-excel-hdr AS CHAR                NO-UNDO.
DEF VAR v-exp-name  AS CHAR FORMAT "x(40)" NO-UNDO 
    INIT "c:\tmp\r-relep.csv".    
DEF VAR v-part-no   LIKE itemfg.part-no    NO-UNDO.
DEF VAR v-i-name    LIKE itemfg.i-name     NO-UNDO.
{sys/form/r-topw.f}

form oe-rell.ord-no label "Ord #"
     oe-relh.rel-date label "  Date" FORMAT "99/99/99"
     oe-rell.rel-no COLUMN-LABEL "Rel" space(0) "-" space(0)
     oe-rell.b-ord-no COLUMN-LABEL "# " format "99"
     oe-relh.cust-no label "Cust #"
     oe-relh.ship-id label "Ship #"
     oe-rell.i-no label "FG Item #"
     itemfg.part-no label "Customer Part #"
     itemfg.i-name format "x(20)" label "Item Name"
     oe-rell.loc label "Whse"
     oe-rell.loc-bin label "Bin Loc"
     oe-rell.tag label "Tag"
     oe-rell.qty label "Tot Qty" FORMAT "->,>>>,>>>"
     v-s-code format "x" label "C"
     oe-rell.link-no LABEL "Seq. #" FORMAT ">>>>>>>>>"
    with STREAM-IO width 145 down frame rell.


  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.

  assign
   str-tit2 = c-win:title 
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 145}.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  ASSIGN
      v-export    = tb_excel
      v-excel-hdr = "Ord #,Date,Rel,#,Cust #,Ship #,FG Item #,Customer Part #,Item Name,Whse,Bin Loc,Tag,Tot Qty,C,Seq.#"
      v-exp-name  = fi_file.

  if td-show-parm then run show-param.

  SESSION:SET-WAIT-STATE ("general").

  IF v-export THEN DO:
    OUTPUT STREAM s-temp TO VALUE(v-exp-name).
    PUT STREAM s-temp UNFORMATTED 
        v-excel-hdr                 
    SKIP.
  END. 

  display with frame r-top.

  for each oe-relh
      where oe-relh.company  eq cocode
        and oe-relh.posted   eq no
        and (oe-relh.printed eq YES OR NOT ip-post)
        and oe-relh.release# ge v-frel
        and oe-relh.release# le v-trel
        and oe-relh.rel-date ge v-fdat
        and oe-relh.rel-date le v-tdat
        and oe-relh.cust-no  ge v-fcus
        and oe-relh.cust-no  le v-tcus
      /* gdm - 03110907 */
        AND (NOT v-chkflg OR
                         (v-chkflg AND oe-relh.w-ord EQ NO))
      /* gdm - 03110907 end */
        and not can-find(first oe-rell
                         where oe-rell.company eq oe-relh.company
                           and oe-rell.r-no    eq oe-relh.r-no
                           and (oe-rell.ord-no lt v-ford or
                                oe-rell.ord-no gt v-tord)
                         USE-INDEX r-no)        

      use-index post no-lock:  

    for each oe-rell
        where oe-rell.company eq oe-relh.company
          and oe-rell.r-no    eq oe-relh.r-no
        USE-INDEX r-no no-lock:
      v-ship-no = lookup(oe-rell.s-code,v-ship-lu).

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-rell.i-no
          no-lock no-error.

      ASSIGN 
          v-part-no = itemfg.part-no
          v-i-name  = itemfg.i-name.

      ASSIGN
         v-s-code = if oe-relh.posted and oe-relh.deleted then "D"
                    else oe-rell.s-code
         v-tot-post = v-tot-post + 1.

      display oe-rell.ord-no oe-relh.rel-date oe-rell.rel-no
              oe-rell.b-ord-no oe-relh.cust-no oe-relh.ship-id
              oe-rell.i-no itemfg.part-no WHEN AVAIL itemfg itemfg.i-name WHEN AVAIL itemfg
              oe-rell.loc oe-rell.loc-bin oe-rell.tag oe-rell.qty
              v-s-code oe-rell.link-no
          with frame rell.
      down with frame rell.

      IF v-export THEN
          PUT STREAM s-temp UNFORMATTED                 
              '"' oe-rell.ord-no                    '",'
              '"' oe-relh.rel-date                  '",'
              '"' oe-rell.rel-no                    '",'
              '"' oe-rell.b-ord-no                  '",'
              '"' oe-relh.cust-no                   '",'
              '"' oe-relh.ship-id                   '",'
              '"' oe-rell.i-no                      '",'
              '"' v-part-no                         '",'
              '"' v-i-name                          '",'
              '"' oe-rell.loc                       '",'
              '"' oe-rell.loc-bin                   '",'
              '"' oe-rell.tag                       '",'
              '"' oe-rell.qty                       '",'
              '"' v-s-code                          '",'
              '"' oe-rell.link-no                   '"'
              SKIP.                                     


    end. /* each oe-rell */
  end. /* each oe-relh */

  v-no-post = 0.
  for each w-nopost break by w-nopost.ord-no:
    if first(w-nopost.ord-no) then
      put "** Release Unable To Post To Orders. **" skip.

    IF v-export AND first(w-nopost.ord-no)
        THEN
          PUT STREAM s-temp UNFORMATTED
            "** Release Unable To Post To Orders. **" SKIP 
            "Ord #,Date,Rel,#,Cust #,Ship #,FG Item #,Customer Part #," + 
            "Item Name,Whse,Bin Loc,Tag,Tot Qty,C,Seq.#"
          SKIP.

    display w-nopost.ord-no @ oe-rell.ord-no
            w-nopost.rel-date @ oe-relh.rel-date
            w-nopost.rel-no @ oe-rell.rel-no
            w-nopost.b-ord-no @ oe-rell.b-ord-no
            w-nopost.cust-no @ oe-relh.cust-no
            w-nopost.ship-id @ oe-relh.ship-id
            w-nopost.i-no @ oe-rell.i-no
            w-nopost.part-no @ itemfg.part-no
            w-nopost.i-name @ itemfg.i-name
            w-nopost.loc @ oe-rell.loc
            w-nopost.loc-bin @ oe-rell.loc-bin
            w-nopost.tag @ oe-rell.tag
            w-nopost.qty @ oe-rell.qty
            w-nopost.code @ v-s-code 
            w-nopost.link-no @ oe-rell.link-no
            with frame rell.
    down with frame rell.

    IF v-export THEN
        PUT STREAM s-temp UNFORMATTED
            '"' w-nopost.ord-no   '",'      
            '"' w-nopost.rel-date '",'
            '"' w-nopost.rel-no   '",'
            '"' w-nopost.b-ord-no '",'
            '"' w-nopost.cust-no  '",'
            '"' w-nopost.ship-id  '",'
            '"' w-nopost.i-no     '",'
            '"' w-nopost.part-no  '",'
            '"' w-nopost.i-name   '",'
            '"' w-nopost.loc      '",'
            '"' w-nopost.loc-bin  '",'
            '"' w-nopost.tag      '",'
            '"' w-nopost.qty      '",'
            '"' w-nopost.code     '",'
            '"' w-nopost.link-no  '"' 
            SKIP.

    v-no-post = v-no-post + 1.
    delete w-nopost.
  end. /* each w-no-post */

     /* gdm 10/20/2008 */
    IF v-export THEN DO:
        OUTPUT STREAM s-temp close.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(v-exp-name)).
    END.

IF NOT ip-post THEN RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
ELSE
    RUN usr-print-one-field (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE usr-print-one-field C-Win 
PROCEDURE usr-print-one-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-program-id AS cha  NO-UNDO.
DEF INPUT PARAM ip-frame-hdl AS HANDLE NO-UNDO.
/*
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}
  */

ASSIGN
 cocode = g_company
 locode = g_loc.  

DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR li AS INT NO-UNDO.

&SCOPED-DEFINE where-phrase                      ~
    WHERE user-print.company    EQ cocode        ~
      AND user-print.program-id EQ ip-program-id ~
      AND user-print.batch      EQ ""


DEF BUFFER bf-user-print FOR user-print.
DEF VAR lv-update-all-batch AS LOG NO-UNDO.

FOR EACH tt-date:
    DELETE tt-date.
END.

IF g_batch THEN
FIND FIRST user-print NO-LOCK WHERE ROWID(user-print) EQ g_batch-rowid NO-ERROR.
ELSE 
FIND FIRST user-print NO-LOCK
     {&where-phrase} AND user-print.user-id EQ USERID("nosweat") NO-ERROR.

IF NOT AVAIL user-print THEN
FIND FIRST user-print NO-LOCK {&where-phrase} AND user-print.user-id EQ "" NO-ERROR.

CREATE tt-user-print.

IF AVAIL user-print THEN BUFFER-COPY user-print TO tt-user-print.

 ASSIGN
 lv-group-hdl = ip-frame-hdl:FIRST-CHILD
 lv-field-hdl = lv-group-hdl:FIRST-CHILD.

 li = 0.

DO WHILE TRUE:
  li = li + 1.

  IF li GT EXTENT(tt-user-print.field-name) OR
     NOT VALID-HANDLE(lv-field-hdl)         THEN LEAVE.
  IF lv-field-hdl:NAME = "tgMultipleReleases" THEN DO:

  ASSIGN
   tt-user-print.field-label[li] = lv-field-hdl:LABEL
   tt-user-print.field-name[li]  = lv-field-hdl:NAME
   tt-user-print.field-value[li] = IF lv-field-hdl:NAME BEGINS "sl" THEN lv-field-hdl:LIST-ITEMS
                                   ELSE lv-field-hdl:SCREEN-VALUE 
   NO-ERROR.     

  END.
  lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
END.
DO:
  FIND CURRENT user-print EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAIL user-print THEN CREATE user-print.

  BUFFER-COPY tt-user-print EXCEPT rec_key TO user-print.

  RELEASE user-print.

  FOR EACH user-print NO-LOCK
      {&where-phrase}
        AND user-print.user-id EQ "":
    FIND bf-user-print WHERE ROWID(bf-user-print) EQ ROWID(user-print)
        EXCLUSIVE NO-ERROR NO-WAIT.
    IF AVAIL bf-user-print THEN DELETE bf-user-print.
  END.

  CREATE user-print.

  BUFFER-COPY tt-user-print EXCEPT rec_key TO user-print
  ASSIGN user-print.user-id = "".  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

