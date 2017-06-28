&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-bole&p.w

  Description: BOL Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 04/12/2002

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

DEF VAR ip-post AS LOG INIT YES NO-UNDO.

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

{oe/oe-bolp1.i NEW}

def var v-back like itemfg.q-back.
def var v-prt  as log init yes.
def var v-bol-bal like oe-boll.qty.
def var v-ref-no as int.
def var v-rcpth-no as int.
def var v-frst as log init no.
def var v-ext-price like inv-line.t-price.
def var olinecnt as int init 0.
def var frtcnt as log init no.
def var v-create as log.
def var v-bo-ch as log no-undo.
def var v-close-qty like oe-ordl.qty.

def var f as int no-undo.
def var v-assign-comm as log init no no-undo.
def var exist-amt as dec no-undo.
def var exist-flag as log init no no-undo.
def var exist-comm as dec extent 3 init 0 no-undo.
def var temp-tax as dec init 0 no-undo.
def var v-fg-qty like oe-boll.qty.
def var v-po-no like oe-rel.po-no.
def var v-ship-inst as char extent 2.
def var v-check-qty as log no-undo.
def var dis-tag as CHAR   no-undo.
DEFINE VAR vtag AS INT NO-UNDO.
DEFINE VAR vtag2 AS INT NO-UNDO.
DEFINE VAR  td-full-tag AS LOG NO-UNDO.

DEF VAR v-rtn-char      AS CHAR NO-UNDO.
DEF VAR v-rec-found     AS LOG  NO-UNDO.
DEF VAR invstatus-char  AS CHAR NO-UNDO.
DEF VAR invstatus-log   AS LOG  NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.


FORMAT
  oe-bolh.bol-date
  space(2)
  oe-bolh.bol-no   format ">>>>>>>>"
  space(2)
  oe-bolh.carrier
  space(4)
  oe-bolh.trailer
  space(2)
  oe-bolh.freight  format "->>>,>>9.99"
  space(2)
  oe-bolh.cwt
  space(3)
  oe-bolh.tot-wt   format "->>>,>>9"
  space(2)
  oe-bolh.cust-no
  space(4)
  oe-bolh.ship-id
  space(2)
  oe-bolh.deleted AT 106 format "*DELETED*/ "
  skip(1)

  header "Date           BOL.#  Carrier  Trailer                   Freight    Rate     Tot WT  Cust#       Ship#   "
         "----------  --------  -------  --------------------  -----------  ------  ---------  --------    --------"

  with stream-io width 132 no-labels no-box no-underline frame bolh.

format
  space(5)
  oe-boll.i-no
  itemfg.i-name    format "x(20)"
  oe-boll.po-no
  oe-boll.ord-no
  oe-boll.rel-no   format ">>9" space(0) "-" space(0)
  oe-boll.b-ord-no format "99"
  oe-boll.loc
  oe-boll.loc-bin
  oe-boll.tag
  oe-boll.cases    format "->>>,>>9"
  oe-boll.qty-case format "->>>,>>9"
  oe-boll.partial  format "->>>,>>9"
  oe-boll.weight   format "->>>,>>9"

  header
  space(5) "Item#           Item Name            P.O. #            Ord#  Rel.# Whse. Bin Loc  Tag         Cases Qty/Case  Partial   Weight" skip
  space(5) "--------------- -------------------- --------------- ------ ------ ----- -------- -------- -------- -------- -------- --------"
  with stream-io width 132 DOWN no-labels no-box no-underline frame boll.

{oe/closchk.i NEW}

DEF TEMP-TABLE tt-email NO-UNDO
      FIELD tt-recid AS RECID
      FIELD bol-no LIKE oe-boll.bol-no
      FIELD ord-no LIKE oe-boll.ord-no
      FIELD i-no LIKE itemfg.i-no
      FIELD qty AS INT
      FIELD cust-no AS cha
      INDEX tt-cust IS PRIMARY cust-no DESCENDING .


DEF STREAM st-email.

DEFINE STREAM ediBOL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 tran-date begin_bolnum begin_date ~
end_date begin_cust end_cust lv-ornt lines-per-page rd-dest lv-font-no ~
td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period tran-time ~
begin_bolnum begin_date end_date begin_cust end_cust lv-ornt lines-per-page ~
rd-dest lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_bolnum AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning BOL#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning BOL Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_bolnum AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending BOL#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending BOL Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

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

DEFINE VARIABLE tran-time AS CHARACTER FORMAT "x(20)":U 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

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
     SIZE 21 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 17.62.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 29 COLON-ALIGNED
     tran-period AT ROW 2.43 COL 53 COLON-ALIGNED
     tran-time AT ROW 2.43 COL 74 COLON-ALIGNED WIDGET-ID 2
     begin_bolnum AT ROW 4.1 COL 29 COLON-ALIGNED HELP
          "Enter the beginning BOL number"
     end_bolnum AT ROW 4.1 COL 74 COLON-ALIGNED HELP
          "Enter the ending BOL number"
     begin_date AT ROW 5.05 COL 29 COLON-ALIGNED HELP
          "Enter the beginning BOL date"
     end_date AT ROW 5.05 COL 74 COLON-ALIGNED HELP
          "Enter the ending BOL date"
     begin_cust AT ROW 6 COL 29 COLON-ALIGNED HELP
          "Enter the beginning customer number"
     end_cust AT ROW 6 COL 74 COLON-ALIGNED HELP
          "Enter the ending customer number"
     lv-ornt AT ROW 13.14 COL 31 NO-LABEL
     lines-per-page AT ROW 13.14 COL 84 COLON-ALIGNED
     rd-dest AT ROW 13.38 COL 7 NO-LABEL
     lv-font-no AT ROW 15.52 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 16.48 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.43 COL 7
     btn-ok AT ROW 19.57 COL 24
     btn-cancel AT ROW 19.57 COL 59
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.95 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 6
          BGCOLOR 2 
     RECT-7 AT ROW 1.24 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.4 BY 21.86.

DEFINE FRAME FRAME-E
     "posted to all orders." VIEW-AS TEXT
          SIZE 26 BY 1.19 AT ROW 2.43 COL 15
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "Bills of Lading MUST BE printed prior to posting!" VIEW-AS TEXT
          SIZE 61 BY .95 AT ROW 3.86 COL 15
          BGCOLOR 11 FONT 5
     "The Edit List will show all available bills of lading to be" VIEW-AS TEXT
          SIZE 65 BY 1.19 AT ROW 1.24 COL 15
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.43
         SIZE 92 BY 4.29
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
         TITLE              = "BOL Edit List & Posting"
         HEIGHT             = 21.86
         WIDTH              = 96.4
         MAX-HEIGHT         = 21.86
         MAX-WIDTH          = 96.4
         VIRTUAL-HEIGHT     = 21.86
         VIRTUAL-WIDTH      = 96.4
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
       begin_bolnum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_bolnum IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       end_bolnum:HIDDEN IN FRAME FRAME-A           = TRUE
       end_bolnum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tran-time IN FRAME FRAME-A
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
ON END-ERROR OF C-Win /* BOL Edit List  Posting */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* BOL Edit List  Posting */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bolnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bolnum C-Win
ON LEAVE OF begin_bolnum IN FRAME FRAME-A /* Beginning BOL# */
DO:
  ASSIGN {&self-name}.
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
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning BOL Date */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  run check-date.
  if v-invalid then return no-apply.

  IF invstatus-char EQ "One Bol Only" THEN
    ASSIGN END_bolnum = begin_bolnum
           END_bolnum:SCREEN-VALUE = begin_bolnum:SCREEN-VALUE.  
  ELSE
    ASSIGN END_bolnum.

  assign
   rd-dest
   tran-period
   v-s-bol    = begin_bolnum
   v-e-bol    = end_bolnum
   v-s-date   = begin_date
   v-e-date   = end_date
   v-s-cust   = begin_cust
   v-e-cust   = end_cust
   v-no-post  = 0
   v-tot-post = 0
   v-tried    = no.

  FOR EACH w-ord.
    DELETE w-ord.
  END.

  EMPTY TEMP-TABLE tt-email.
  RUN run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  IF ip-post THEN DO:
    IF CAN-FIND(FIRST w-bolh) THEN DO:
      lv-post = NO.

      MESSAGE "Post BOLs?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE lv-post.

      IF lv-post THEN do:
        RUN post-bols.

        /* close transfer order here */
        RUN oe/closchk.p (0).


        /* WFk- 5/4/12- This is here to make sure it is the last thing in */
        /* the posting process.  Posting relies on a cleanup routine */
        /* for releases instead of fixing the real problem.          */
        FOR EACH w-bolh,
          FIRST oe-bolh WHERE RECID(oe-bolh) eq w-bolh.w-recid NO-LOCK:

          FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no:

               FIND FIRST oe-ordl NO-LOCK
                    WHERE oe-ordl.company EQ oe-boll.company
                      AND oe-ordl.ord-no EQ oe-boll.ord-no
                      AND oe-ordl.line EQ oe-boll.line NO-ERROR.
            RUN oe/cleanrel.p (INPUT ROWID(oe-ordl)).    
          end.
        end.

        FOR EACH w-ord:
          RUN oe/close.p (w-ord.rec-id, YES).  
        END.

        FIND FIRST tt-email NO-LOCK NO-ERROR.
        IF AVAIL tt-email THEN RUN email-reorderitems.

        MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
      END.
    END.

    ELSE MESSAGE "No BOLs available for posting..." VIEW-AS ALERT-BOX ERROR.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bolnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bolnum C-Win
ON LEAVE OF end_bolnum IN FRAME FRAME-A /* Ending BOL# */
DO:
  ASSIGN {&self-name}.
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
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending BOL Date */
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


&Scoped-define SELF-NAME tran-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-time C-Win
ON LEAVE OF tran-time IN FRAME FRAME-A /* Time */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */    
PROCEDURE mail EXTERNAL "xpMail.dll" :
      DEF INPUT PARAM mailTo AS CHAR.
      DEF INPUT PARAM mailsubject AS CHAR.
      DEF INPUT PARAM mailText AS CHAR.
      DEF INPUT PARAM mailFiles AS CHAR.
      DEF INPUT PARAM mailDialog AS LONG.
      DEF OUTPUT PARAM retCode AS LONG.
END.

{sys/inc/f3helpw.i}
DEF VAR choice AS LOG NO-UNDO.

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

  ASSIGN
   tran-date   = TODAY
   begin_date  = TODAY
   end_date    = TODAY
   c-win:TITLE = IF ip-post THEN "BOL Posting/Create Invoice"
                            ELSE "BOL Edit List".

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
  v-u-inv = oe-ctrl.u-inv.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BolPostTime"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BolPostTime"
     sys-ctrl.descrip = "Parameter to post Time of Invoice Creation"
     sys-ctrl.log-fld  = yes
     sys-ctrl.char-fld = "BolCreation" .
  END.
   IF AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Fixed Time"  THEN
       ASSIGN  tran-time = STRING(int(SUBSTRING(string(sys-ctrl.dec-fld),1,2)) * 60 * 60 + int(SUBSTRING(string(sys-ctrl.dec-fld),3,4)) * 60 , "hh:mm:ss").
       ELSE ASSIGN tran-time = string(TIME,"hh:mm:ss") .

  RELEASE sys-ctrl.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLPOST"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BOLPOST"
     sys-ctrl.descrip = "Post BOL if BOL Qty > Bin Qty"
     choice           = yes.

    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE choice.

    if not choice then sys-ctrl.char-fld eq "Bin>Qty".
  end.

  /* Invstatus to determine invoice status when created  */
  RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "L", no, no, "", "", 
                         Output v-rtn-char, output v-rec-found).
  invstatus-log = LOGICAL(v-rtn-char).
  /* Invstatus to determine invoice status when created  */
  RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "C", no, no, "", "", 
                         Output invstatus-char, output v-rec-found).

  v-check-qty = sys-ctrl.char-fld eq "Bin>Qty".
  DO TRANSACTION:
     {sys/inc/fgreorder.i}
  END.

 IF InvStatus-char NE "One BOL Only" THEN
      ASSIGN END_bolnum:HIDDEN = NO
             END_bolnum:SENSITIVE = YES.

  RUN enable_UI.

  RUN check-date.

  IF NOT ip-post THEN 
    ASSIGN tran-date:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           tran-period:SENSITIVE IN FRAME {&FRAME-NAME} = NO.


  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_bolnum .
  END.    


    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-nopost C-Win 
PROCEDURE create-nopost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-reason LIKE w-nopost.reason NO-UNDO.


  FIND FIRST itemfg
      WHERE itemfg.company EQ oe-boll.company
        AND itemfg.i-no    EQ oe-boll.i-no
      NO-LOCK NO-ERROR.

  CREATE w-nopost.
  ASSIGN
   w-nopost.ord-no   = oe-boll.ord-no
   w-nopost.i-no     = oe-boll.i-no
   w-nopost.i-name   = IF AVAIL itemfg THEN itemfg.i-name ELSE "Not on File"
   w-nopost.bol-date = oe-bolh.BOL-date
   w-nopost.bol-no   = oe-bolh.BOL-no
   w-nopost.rel-no   = oe-boll.REL-no
   w-nopost.b-ord-no = oe-boll.b-ord-no
   w-nopost.cust-no  = oe-bolh.cust-no
   w-nopost.po-no    = oe-boll.PO-NO
   w-nopost.reason   = ip-reason.

  DELETE w-bolh.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-reorder C-Win 
PROCEDURE create-reorder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAM ip-oeboll-rowid AS ROWID NO-UNDO.
 DEF BUFFER bf-oeboll FOR oe-boll.

 DEF VAR v-qty-onh AS INT NO-UNDO.
 DEF VAR v-qty-avail AS INT NO-UNDO.
 DEF VAR v-reord-qty AS INT NO-UNDO.

 FIND bf-oeboll WHERE ROWID(bf-oeboll) = ip-oeboll-rowid NO-LOCK.
 FIND itemfg WHERE itemfg.company = cocode AND
                   itemfg.i-no = bf-oeboll.i-no NO-LOCK.

 v-qty-onh = 0.
 FOR EACH fg-bin
         WHERE fg-bin.company EQ itemfg.company
           AND fg-bin.i-no    EQ itemfg.i-no
           /*AND fg-bin.loc     GE begin_whse
           AND fg-bin.loc     LE end_whse*/ NO-LOCK:
      v-qty-onh = v-qty-onh + fg-bin.qty.
 END.

 ASSIGN
     v-qty-avail = v-qty-onh /*+ (if v-inconh then itemfg.q-ono else 0)*/
                    -  itemfg.q-alloc.

 if itemfg.ord-level gt v-qty-avail then do:
       v-reord-qty = itemfg.ord-level - v-qty-avail.

       if v-reord-qty lt itemfg.ord-min and
          itemfg.ord-min ne 0 then 
         v-reord-qty = itemfg.ord-min.

       if v-reord-qty gt itemfg.ord-max and
          itemfg.ord-max ne 0 then 
         v-reord-qty = itemfg.ord-max.
 end.
 else v-reord-qty = 0.


 IF v-reord-qty > 0 THEN DO:

    CREATE tt-email.
    ASSIGN tt-email.bol-no = bf-oeboll.bol-no
           tt-email.ord-no = bf-oeboll.ord-no
           tt-email.i-no = bf-oeboll.i-no
           tt-email.qty = v-reord-qty
           tt-email.cust-no = oe-bolh.cust-no
           .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE email-reorderitems C-Win 
PROCEDURE email-reorderitems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*DEF INPUT PARAM ip-fgemail-file AS cha .*/

  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS cha NO-UNDO.
  DEF VAR lv-mailto AS cha NO-UNDO.
  DEF VAR lv-mailsubject AS cha NO-UNDO.
  DEF VAR lv-mailbody AS cha NO-UNDO.
  DEF VAR lv-mailattach AS cha NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.
  DEF VAR v-qty-onh AS INT NO-UNDO.
  DEF VAR v-qty-avail AS INT NO-UNDO.
  DEF VAR v-qty-alloc AS INT NO-UNDO.
  DEF VAR v-qty-onOrder AS INT NO-UNDO.

  FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.
  IF AVAIL users AND users.user_program[2] NE "" THEN v-dir = users.user_program[2] + "\".
  ELSE v-dir = "c:\tmp\".

  FOR EACH tt-email,
       FIRST cust NO-LOCK WHERE cust.company = cocode
                           AND cust.cust-no = tt-email.cust-no
                           AND cust.active = "E" BREAK BY tt-email.cust-no BY tt-email.i-no:
       IF FIRST-OF(tt-email.cust-no) THEN DO:
          v-fgemail-file = v-dir + trim(tt-email.cust-no) + ".txt".
          OUTPUT STREAM st-email TO VALUE(v-fgemail-file).
          PUT STREAM st-email "***** Reorder Point Item from BOL Posting *****" SKIP
                              "BOL#     Order#       FG Item#      ReOrder Qty     Avail Qty  On Hand Qty On Order Qty" SKIP
                              "======== ========== =============== ============ ============ ============ ============" SKIP.
       END.
       IF FIRST-OF(tt-email.i-no) THEN DO:
          /*v-qty-onh = 0.
          FOR EACH fg-bin WHERE fg-bin.company EQ cocode
                         AND fg-bin.i-no    EQ tt-email.i-no
                 /*AND fg-bin.loc     GE begin_whse
                 AND fg-bin.loc     LE end_whse*/  NO-LOCK:
              v-qty-onh = v-qty-onh + fg-bin.qty.
         END.
         */
         FIND itemfg WHERE itemfg.company = cocode
                       AND itemfg.i-no = tt-email.i-no NO-LOCK NO-ERROR.
         IF AVAIL itemfg THEN ASSIGN v-qty-onh = itemfg.q-onh
                                     v-qty-onOrder = itemfg.q-ono
                                     v-qty-alloc = itemfg.q-alloc.
         ELSE ASSIGN v-qty-onh = 0
                     v-qty-onOrder = 0
                     v-qty-alloc = 0.
         {sys/inc/oereordr.i}

         v-qty-avail = v-qty-onh +
                       (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE v-qty-onOrder) -
                       v-qty-alloc.

       END.

       PUT STREAM st-email UNFORMATTED
                 STRING(tt-email.bol-no) FORM "x(9)"
                 string(tt-email.ord-no) FORM "x(10)"
                 " " tt-email.i-no " " tt-email.qty FORM "->>>,>>>,>>9" 
                 " " v-qty-avail  FORM "->>>,>>>,>>9"
                 " " v-qty-onh FORM "->>>,>>>,>>9"
                 " " v-qty-onOrder FORM "->>>,>>>,>>9"
                 SKIP.
       IF LAST-OF(tt-email.cust-no) THEN do:
           OUTPUT STREAM st-email CLOSE.
           {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}

           IF ls-to-list NE '' THEN DO:
             ASSIGN lv-mailto = "To:" + ls-to-list
                    lv-mailsubject = "Finished Goods Reorder Point item from BOL Post"
                    lv-mailbody = "Finished Goods Reorder Point item from BOL Post"
                    lv-mailattach = v-fgemail-file.
             RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,lv-mailattach,1,OUTPUT retcode).
           END.
       END. /* last-of(tt-email.cust-no) */
   END.

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
  DISPLAY tran-date tran-period tran-time begin_bolnum begin_date end_date 
          begin_cust end_cust lv-ornt lines-per-page rd-dest lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 tran-date begin_bolnum begin_date end_date begin_cust end_cust 
         lv-ornt lines-per-page rd-dest lv-font-no td-show-parm btn-ok 
         btn-cancel 
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
/* -------------------------------------------------- oe/oe-bolp7.p 11/01 JLF */
/* BOL posting Exception Report                                               */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

     ASSIGN td-full-tag = YES.
    MESSAGE " Do you wish to print full tag value?  "  
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO 
        UPDATE td-full-tag .

  FORM HEADER SKIP(1) WITH FRAME r-top.

  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.

  assign
   str-tit2 = "BOL - Insufficient Inventory Report"
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  display with frame r-top.

  for each w-except,

      first oe-bolh
      where oe-bolh.company eq cocode
        and oe-bolh.bol-no  eq w-except.bol-no
      no-lock

     break by w-except.bol-no
           by w-except.ord-no
           by w-except.rel-no
           by w-except.b-ord-no:

    if first-of(w-except.bol-no) then do:
      display oe-bolh.bol-date FORMAT "99/99/9999" COLUMN-LABEL "Date"
              oe-bolh.bol-no format ">>>>>>>>" COLUMN-LABEL "   BOL #"
              oe-bolh.carrier FORMAT "X(5)" COLUMN-LABEL "Carrier"
              oe-bolh.trailer FORMAT "X(20)" COLUMN-LABEL "Trailer"
              oe-bolh.freight format "->>>,>>9.99" COLUMN-LABEL "    Freight"
              oe-bolh.cwt     COLUMN-LABEL "  Rate"
              oe-bolh.tot-wt  format "->>>,>>9" COLUMN-LABEL "   Tot WT"
              oe-bolh.cust-no COLUMN-LABEL "Cust#"
              oe-bolh.ship-id COLUMN-LABEL "Ship#"
              oe-bolh.deleted format "*DELETED*/ " COLUMN-LABEL "Deleted"
              SKIP(1)
          with frame bolh2 DOWN NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 150.
      down with frame bolh2.
    end.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-except.i-no
        no-lock no-error.

       vtag = 0.
       vtag2 = 0.
       vtag = LENGTH(w-except.tag).
       vtag2 = vtag - 5 .

      IF NOT td-full-tag AND vtag <> 0 THEN ASSIGN  dis-tag =  SUBSTR(w-except.tag,vtag2,6) .
      ELSE ASSIGN dis-tag  = w-except.tag .

    display SPACE(5)
            w-except.i-no  COLUMN-LABEL "Item #"   
            dis-tag COLUMN-LABEL "Tag" FORMAT "X(22)"
            itemfg.i-name  FORMAT "X(20)" when avail itemfg COLUMN-LABEL "Item Name"
            w-except.po-no COLUMN-LABEL "P.O. #"    
            w-except.ord-no COLUMN-LABEL "  Ord#"   
            STRING(w-except.rel-no,">>9") + "-" + STRING(w-except.b-ord-no,"99") COLUMN-LABEL "Rel.#"    
            w-except.loc COLUMN-LABEL "Whse."
            w-except.loc-bin COLUMN-LABEL "Bin Loc"   

            w-except.cases format "->>>,>>9"   COLUMN-LABEL "   Cases"
            w-except.qty-case format "->>>,>>9" COLUMN-LABEL "Qty/Case" 
            w-except.partial format "->>>,>>9"  COLUMN-LABEL " Partial"
            w-except.weight format "->>>,>>9"   COLUMN-LABEL "  Weight"
        with frame boll2 DOWN NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 165.
    down with frame boll2.

    put skip(1).
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
/*   DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.

   /* Use Progress Print. Always use Font#9 in Registry (set above) */
   RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                             INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
   /* use-dialog(1) and landscape(2) */ */

  RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

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
run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-bols C-Win 
PROCEDURE post-bols :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-exception AS LOG NO-UNDO.
DEF VAR d-out AS DECIMAL NO-UNDO.
DEF VAR lr-rel-lib AS HANDLE NO-UNDO.

{sa/sa-sls01.i}

DISABLE TRIGGERS FOR LOAD OF itemfg.

FOR EACH w-bolh,
    FIRST oe-bolh WHERE RECID(oe-bolh) eq w-bolh.w-recid
    BREAK BY oe-bolh.b-no
          BY oe-bolh.bol-no:
  IF NOT FIRST-OF(oe-bolh.b-no) THEN DELETE w-bolh.
END.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ 'EDIBOLPost' NO-ERROR.

/**********************  POSTING BLOCK  ****************************/
post-blok:
DO TRANSACTION.
  bolh:
  /*for each oe-bolh
      where oe-bolh.company  eq cocode
        and oe-bolh.posted   eq no
        and oe-bolh.printed  eq YES
        AND oe-bolh.deleted  EQ NO
        and oe-bolh.bol-no   ge v-s-bol
        and oe-bolh.bol-no   le v-e-bol
        and oe-bolh.bol-date ge v-s-date
        and oe-bolh.bol-date le v-e-date
        AND oe-bolh.cust-no  GE v-s-cust
        AND oe-bolh.cust-no  LE v-e-cust
        and oe-bolh.trailer  ne "HOLD"
        and oe-bolh.stat     eq "R"
        and not can-find(first oe-boll
                         where oe-boll.company  eq oe-bolh.company
                           and oe-boll.b-no     eq oe-bolh.b-no
                           and (oe-boll.loc     eq "" or
                                oe-boll.loc-bin eq "" or
                                oe-boll.qty     eq 0))
      use-index post,*/
  for each w-bolh,
      first oe-bolh where recid(oe-bolh) eq w-bolh.w-recid,

      first cust
      where cust.company eq cocode
        and cust.cust-no eq oe-bolh.cust-no
      no-lock

      break by oe-bolh.bol-no
            by oe-bolh.ord-no
            by oe-bolh.rel-no.

    IF FIRST-OF(oe-bolh.bol-no) AND v-u-inv AND v-check-qty THEN
      RUN oe/bolcheck.p (ROWID(oe-bolh)).

    find first w-except where w-except.bol-no eq oe-bolh.bol-no no-error.
    if avail w-except then next bolh.

    olinecnt = olinecnt + 1.

    IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
      FIND FIRST sys-ctrl-shipto NO-LOCK
           WHERE sys-ctrl-shipto.company EQ sys-ctrl.company
             AND sys-ctrl-shipto.name EQ sys-ctrl.name
             AND sys-ctrl-shipto.cust-vend EQ YES
             AND sys-ctrl-shipto.cust-vend-no EQ w-bolh.cust-no
             AND sys-ctrl-shipto.log-fld EQ YES NO-ERROR.
      IF AVAILABLE sys-ctrl-shipto THEN
      OUTPUT STREAM ediBOL TO VALUE(sys-ctrl.char-fld + '/' +
                                    sys-ctrl-shipto.cust-vend-no + '/' +
                                    'bol' + STRING(TODAY,'99999999')) APPEND.
    END. /* avail sys-ctrl */

    RUN-PROC = "sbo/oerel-recalc-act.p".
    RUN VALUE(run-proc) PERSISTENT SET phandle NO-ERROR.
    lr-rel-lib = phandle.

    FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no,
      EACH oe-ordl NO-LOCK
         WHERE oe-ordl.company EQ oe-boll.company
           AND oe-ordl.ord-no EQ oe-boll.ord-no
            AND oe-ordl.line EQ oe-boll.LINE:

      FOR EACH oe-rel 
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND oe-rel.stat = "P"
              AND oe-rel.link-no GT 0 
              AND oe-rel.rel-no GT 0:

            /* Set actual quantity */
            IF AVAIL oe-rel AND VALID-HANDLE(lr-rel-lib) THEN 
               RUN recalc-act-qty IN lr-rel-lib (INPUT ROWID(oe-rel), OUTPUT d-out).

      END.
    END.

    IF VALID-HANDLE(lr-rel-lib) THEN
     DELETE OBJECT lr-rel-lib.

    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no:
      RUN oe/bol-pre-post.p (ROWID(oe-boll), v-term).

      IF fgreorder-log AND cust.ACTIVE EQ "E" THEN
      RUN create-reorder (ROWID(oe-boll)).


      IF AVAILABLE sys-ctrl-shipto THEN DO:
        FIND FIRST oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ oe-boll.company
               AND oe-ordl.ord-no EQ oe-boll.ord-no
               AND oe-ordl.line EQ oe-boll.line NO-ERROR.
        EXPORT STREAM ediBOL DELIMITER '~t'
          oe-bolh.bol-date
          oe-bolh.bol-no
          oe-bolh.carrier
          oe-bolh.trailer
          oe-bolh.freight
          oe-bolh.cwt
          oe-bolh.tot-wt
          oe-bolh.cust-no
          oe-bolh.ship-id
          oe-boll.i-no
          oe-boll.po-no
          oe-boll.ord-no
          oe-boll.rel-no
          oe-boll.loc
          oe-boll.loc-bin
          oe-boll.tag
          oe-boll.cases
          oe-boll.qty-case
          oe-boll.partial
          oe-boll.weight
          oe-ordl.part-no WHEN AVAIL(oe-ordl).
      END. /* avail sys-ctrl-shipto */

    END. /* each oe-boll */

    IF AVAILABLE sys-ctrl-shipto THEN
    OUTPUT STREAM ediBOL CLOSE.

  END. /* for each oe-bolh */

  /*/* check all items shipped to complete for order type "T" */
  DEF BUFFER bf-word FOR w-ord.
  DEF VAR v-close AS LOG NO-UNDO.
  FOR EACH w-ord BREAK BY w-ord.ord-no:
      IF FIRST-OF(w-ord.ord-no) THEN DO:
         FIND FIRST oe-ord WHERE oe-ord.company = cocode AND
                                 oe-ord.ord-no = w-ord.ord-no NO-LOCK NO-ERROR.
         v-close = YES.
         FOR EACH oe-ordl OF oe-ord NO-LOCK:
             IF NOT CAN-FIND(FIRST bf-word WHERE bf-word.rec-id = RECID(oe-ordl))
                THEN v-close = NO.
         END.
         IF NOT v-close THEN
            FOR EACH bf-word WHERE bf-word.ord-no = w-ord.ord-no:
                DELETE bf-word.
            END.
      END.
  END.*/

  for each tt-fg-bin:
    delete tt-fg-bin.
  end.

  run oe/oe-bolp3.p (v-term).


end. /* post-blok*/

delete-blok:
for each oe-bolh
    where oe-bolh.company  eq cocode
      and oe-bolh.deleted  eq YES
      and oe-bolh.bol-no   ge v-s-bol
      and oe-bolh.bol-no  le v-e-bol
      and oe-bolh.bol-date ge v-s-date
      and oe-bolh.bol-date le v-e-date
      AND oe-bolh.cust-no  GE v-s-cust
      AND oe-bolh.cust-no  LE v-e-cust
      and oe-bolh.trailer  ne "HOLD"
      and oe-bolh.stat     eq "R"
    use-index deleted:

  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no:
    DELETE oe-boll.
  END. /* each oe-boll */

  delete oe-bolh.
end. /* each oe-bolh */



find first w-except no-error.
if avail w-except then do:
  lv-exception = YES.
  MESSAGE "  Bill(s) of Lading have been found that do not have  "     skip
          "  sufficient inventory for posting to be completed.   "     skip
          "  Do you wish to print the exception report?          "
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE lv-exception.
  IF lv-exception THEN do:
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
/* -------------------------------------------------- oe/oe-bolp2.p 07/97 FWK */
/* BILL OF LADING POSTING REPORT MODULE 2 - O/E Module                        */
/* -------------------------------------------------------------------------- */

DEF BUFFER b-oe-boll FOR oe-boll.

{sys/form/r-top3w.f}

FORM HEADER SKIP(1) WITH FRAME r-top.


  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.

  assign
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  if td-show-parm then run show-param.

  display with frame r-top.

  for each w-bolh:
    delete w-bolh.
  end.

  for each w-nopost:
    delete w-nopost.
  end.

  for each oe-bolh
      where oe-bolh.company  eq cocode
        and oe-bolh.posted   eq no
        and (oe-bolh.printed eq YES OR NOT ip-post)
        and oe-bolh.bol-no   ge v-s-bol
        and oe-bolh.bol-no   le v-e-bol
        and oe-bolh.bol-date ge v-s-date
        and oe-bolh.bol-date le v-e-date
        and oe-bolh.cust-no  ge v-s-cust
        and oe-bolh.cust-no  le v-e-cust
        and oe-bolh.trailer  ne "HOLD"
        and oe-bolh.stat     eq "R"
      use-index post no-lock:

    create w-bolh.
    assign
     w-bolh.bol-no   = oe-bolh.bol-no
     w-bolh.ord-no   = oe-bolh.ord-no
     w-bolh.w-recid  = recid(oe-bolh)
     w-bolh.rel-no   = oe-bolh.rel-no
     w-bolh.b-ord-no = oe-bolh.b-ord-no
     w-bolh.cust-no  = oe-bolh.cust-no.
  end.

  for each oe-bolh
      where oe-bolh.company  eq cocode
        and oe-bolh.deleted  eq yes
        and oe-bolh.posted   eq yes
        and oe-bolh.bol-no   ge v-s-bol
        and oe-bolh.bol-no   le v-e-bol
        and oe-bolh.bol-date ge v-s-date
        and oe-bolh.bol-date le v-e-date
        and oe-bolh.trailer  ne "HOLD"
        and oe-bolh.stat     eq "R"
      use-index deleted no-lock:

    create w-bolh.
    assign
     w-bolh.bol-no   = oe-bolh.bol-no
     w-bolh.ord-no   = oe-bolh.ord-no
     w-bolh.w-recid  = recid(oe-bolh)
     w-bolh.rel-no   = oe-bolh.rel-no
     w-bolh.b-ord-no = oe-bolh.b-ord-no
     w-bolh.cust-no  = oe-bolh.cust-no.
  end.

  FOR EACH w-bolh,
      FIRST oe-bolh WHERE RECID(oe-bolh) eq w-bolh.w-recid
      BREAK BY oe-bolh.b-no
            BY oe-bolh.bol-no:
    IF NOT FIRST-OF(oe-bolh.b-no) THEN DELETE w-bolh.
  END.

  MAINBLOK:
  for each w-bolh by w-bolh.bol-no by w-bolh.ord-no
                  by w-bolh.rel-no by w-bolh.b-ord-no:
    find oe-bolh where recid(oe-bolh) eq w-bolh.w-recid no-lock.

    v-tot-post = v-tot-post + 1.

    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no
        NO-LOCK
        break by oe-boll.company
              by oe-boll.b-no
              by oe-boll.ord-no
              by oe-boll.rel-no
              by oe-boll.b-ord-no:

      RELEASE oe-ord.
      RELEASE oe-ordl.

      if not oe-bolh.deleted then do:
        find first oe-ord where oe-ord.company = oe-bolh.company and
             oe-ord.ord-no = oe-boll.ord-no no-lock no-error.
        if not avail oe-ord then do:
          RUN create-nopost ("Order Was Not Found").
          next mainblok.
        end.

        /* 04301302 - If customer 'x' and shipto = shipfrom, don't post */
        FIND cust 
          WHERE cust.company EQ oe-bolh.company
            AND cust.cust-no EQ oe-bolh.cust-no 
          NO-LOCK NO-ERROR.

        IF AVAIL(cust) AND cust.ACTIVE EQ "X"
            AND oe-bolh.ship-id = oe-boll.loc THEN DO:
            run create-nopost ("Cannot transfer to the same location").
            next mainblok.
        END.
        find first oe-ordl where oe-ordl.company = oe-boll.company  and
              oe-ordl.ord-no = oe-boll.ord-no  and
              oe-ordl.line   = oe-boll.line no-lock no-error.
        if not avail oe-ordl then do:
          run create-nopost ("Order Lines Were Not Found").
          next mainblok.
        end.

        find first oe-rell where oe-rell.company = oe-boll.company and
               oe-rell.r-no = oe-boll.r-no AND
               oe-rell.i-no = oe-boll.i-no and
               oe-rell.line = oe-boll.line
              USE-INDEX r-no no-lock no-error.
        if not avail oe-rell then do:
          run create-nopost ("Release Lines Were Not Found").
          next mainblok.
        end.

        find first itemfg where itemfg.company = oe-boll.company and
                itemfg.i-no = oe-boll.i-no no-lock no-error.
        if not avail itemfg then do:
          run create-nopost ("Finish Good Item Was Not Found").
          NEXT mainblok.
        end.

        if oe-boll.loc eq "" or oe-boll.loc-bin eq "" THEN do:
          run create-nopost ("Warehouse or Bin is Blank").
          NEXT mainblok.
        end.

        IF NOT CAN-FIND(FIRST b-oe-boll
                        WHERE b-oe-boll.company EQ oe-bolh.company
                          AND b-oe-boll.b-no    EQ oe-bolh.b-no
                          AND b-oe-boll.qty     NE 0)
        THEN DO:
          RUN create-nopost ("BOL Qty is Zero").
          NEXT mainblok.
        END.
      end.

      if first-of(oe-boll.b-no) then do:
        DISPLAY oe-bolh.BOL-date
                oe-bolh.BOL-no 
                oe-bolh.CARRIER
                oe-bolh.TRAILER
                oe-bolh.FREIGHT
                oe-bolh.CWT
                oe-bolh.TOT-WT
                oe-bolh.cust-no
                oe-bolh.ship-id
                oe-bolh.deleted
            with frame bolh.
        down with frame bolh.
      end.

     DISPLAY oe-boll.i-no
              itemfg.i-name when avail itemfg
              oe-boll.po-no
              oe-boll.ord-no
              oe-boll.rel-no
              oe-boll.b-ord-no
              oe-boll.loc
              oe-boll.loc-bin
              oe-boll.tag
                SUBSTR(oe-boll.tag,16,8) WHEN SUBSTR(oe-boll.tag,1,15) EQ oe-boll.i-no
                @ oe-boll.tag
              oe-boll.CASES
              oe-boll.qty-CASE
              oe-boll.PARTIAL
              oe-boll.WEIGHT
          with frame boll.
      down with frame boll.

      IF AVAIL oe-ord                                 AND
         AVAIL oe-ordl                                AND
         oe-ordl.ship-qty + oe-boll.qty GT
         oe-ordl.qty * (1 + (oe-ordl.over-pct / 100)) THEN
        PUT SPACE(10)
            "*** Qty Shipped will exceed Qty Ordered + Allowable Overrun"
            SKIP.
    end. /* each oe-boll */

    put skip(1).
  end. /* each oe-bolh */

  v-no-post = 0.

  for each w-nopost break by w-nopost.bol-no:
    if first(w-nopost.bol-no) then
      put skip(1)
          "** Bills Of Lading Unable To Be Posted. **" skip.

    DISPLAY w-nopost.bol-no     COLUMN-LABEL "BOL.#"
            w-nopost.bol-date   COLUMN-LABEL "Date"
            w-nopost.ord-no     COLUMN-LABEL "Order#"
            string(w-nopost.rel-no,">>>9") + "-" +
            string(w-nopost.b-ord-no,"99")
                                COLUMN-LABEL "Rel#-BO#"    FORMAT "x(7)"
            w-nopost.cust-no    COLUMN-LABEL "Cust.#"
            w-nopost.po-no      COLUMN-LABEL "PO#"
            w-nopost.i-no       COLUMN-LABEL "Item"
            w-nopost.i-name     COLUMN-LABEL "Name"         format "x(20)"
            w-nopost.reason     COLUMN-LABEL "Reason"       skip
        with down STREAM-IO width 132 frame nopost2.
    down with frame nopost2.

    v-no-post = v-no-post + 1.

    delete w-nopost.
  end.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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

  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.

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
                     lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name THEN
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
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

