&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-select AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM io-qty LIKE rm-bin.qty NO-UNDO.

/* Local Variable Definitions ---                                       */
{sys/inc/var.i "new shared"}
{custom/globdefs.i}

{rm/d-selbin.i}

DEF VAR ll-change-qty AS LOG NO-UNDO.

/* 02/06/07 rdb */
DEF VAR lv-sort-by     AS CHAR INIT "loc"           NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Whs/Bin/Tag"   NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.

ASSIGN
 cocode = g_company
 locode = g_loc.

ll-sort-asc = NO.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

&SCOPED-DEFINE sortby-log                                                             ~
    IF lv-sort-by EQ "loc"       THEN tt-bin.loc                                  ELSE ~
    IF lv-sort-by EQ "loc-bin"   THEN tt-bin.loc-bin                              ELSE ~
    IF lv-sort-by EQ "tag"       THEN tt-bin.tag                                  ELSE ~
                                      STRING(9999999999.9999999999 + tt-bin.qty,"9999999999.9999999999")

&SCOPED-DEFINE sortby BY tt-bin.loc BY tt-bin.loc-bin BY tt-bin.tag

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc  ~
    BY ({&sortby-log}) DESC        ~
    {&sortby}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME br-bin

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-bin

/* Definitions for BROWSE br-bin                                        */
&Scoped-define FIELDS-IN-QUERY-br-bin tt-bin.loc tt-bin.loc-bin tt-bin.tag tt-bin.qty tt-bin.cons-uom tt-bin.qty-sht   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-bin tt-bin.loc ~
 tt-bin.loc-bin   tt-bin.tag   tt-bin.qty   tt-bin.qty-sht   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-bin tt-bin
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-bin tt-bin
&Scoped-define SELF-NAME br-bin
&Scoped-define QUERY-STRING-br-bin FOR EACH tt-bin
&Scoped-define OPEN-QUERY-br-bin OPEN QUERY {&SELF-NAME} FOR EACH tt-bin.
&Scoped-define TABLES-IN-QUERY-br-bin tt-bin
&Scoped-define FIRST-TABLE-IN-QUERY-br-bin tt-bin


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-br-bin}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-bin Btn_OK Btn_Select Btn_Deselect ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_seq fi_sel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Deselect 
     LABEL "Unselect All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Select 
     LABEL "Select All" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_sel AS INTEGER FORMAT ">,>>>,>>>,>>>":U INITIAL 0 
     LABEL "Sheets Selected" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi_seq AS INTEGER FORMAT "->,>>>,>>>":U INITIAL 0 
     LABEL "Seq#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-bin FOR 
      tt-bin SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-bin Dialog-Frame _FREEFORM
  QUERY br-bin DISPLAY
      tt-bin.loc         LABEL "Whs"            LABEL-BGCOLOR  14 WIDTH 12
    tt-bin.loc-bin       LABEL "Bin"            LABEL-BGCOLOR  14 WIDTH 12
    tt-bin.tag           LABEL "Tag"            FORMAT "x(30)"
                                                LABEL-BGCOLOR  14 WIDTH 35
    tt-bin.qty           LABEL "Qty On-Hand"    LABEL-BGCOLOR  14 WIDTH 20
    tt-bin.cons-uom      LABEL "UOM"
    tt-bin.qty-sht       LABEL "Shts On-Hand"   LABEL-BGCOLOR  14 WIDTH 20

    ENABLE
    tt-bin.loc    
    tt-bin.loc-bin
    tt-bin.tag
    tt-bin.qty
    tt-bin.qty-sht
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 122 BY 15
         BGCOLOR 8 FONT 0 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     br-bin AT ROW 1 COL 1
     fi_seq AT ROW 16.24 COL 7 COLON-ALIGNED
     fi_sel AT ROW 16.24 COL 38 COLON-ALIGNED
     Btn_OK AT ROW 16.24 COL 60
     Btn_Select AT ROW 16.24 COL 76
     Btn_Deselect AT ROW 16.24 COL 92
     Btn_Cancel AT ROW 16.24 COL 108
     SPACE(0.59) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bins/Tags for"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB br-bin 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_sel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_seq IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-bin
/* Query rebuild information for BROWSE br-bin
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-bin
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-bin */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bins/Tags for */
DO:
  io-qty = 0.
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-bin
&Scoped-define SELF-NAME br-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON MOUSE-EXTEND-CLICK OF br-bin IN FRAME Dialog-Frame
DO:
  RUN renumber-seq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON MOUSE-MENU-CLICK OF br-bin IN FRAME Dialog-Frame
DO:
  RUN renumber-seq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON MOUSE-MOVE-CLICK OF br-bin IN FRAME Dialog-Frame
DO:
  RUN renumber-seq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON MOUSE-SELECT-CLICK OF br-bin IN FRAME Dialog-Frame
DO:
  RUN renumber-seq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON START-SEARCH OF br-bin IN FRAME Dialog-Frame
DO:
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.

  lh-column = {&BROWSE-NAME}:CURRENT-COLUMN.
  IF lh-column:LABEL-BGCOLOR NE 14 THEN RETURN NO-APPLY.

  ASSIGN
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-column-nam BEGINS "job-no" THEN
    ASSIGN
     lv-column-nam = "job-no"
     lv-column-lab = "Job#".

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  IF ll-sort-asc THEN OPEN QUERY br-bin {&QUERY-STRING-br-bin} {&sortby-phrase-asc}.
                 ELSE OPEN QUERY br-bin {&QUERY-STRING-br-bin} {&sortby-phrase-desc}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  io-qty = 0.
  {&browse-name}:DESELECT-ROWS () NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deselect Dialog-Frame
ON CHOOSE OF Btn_Deselect IN FRAME Dialog-Frame /* Unselect All */
DO:
  {&browse-name}:DESELECT-ROWS ().
  RUN renumber-seq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR ld-selected-qty LIKE io-qty NO-UNDO.


  RUN set-select.

  ll = NO.
  IF CAN-FIND(FIRST tt-bin WHERE tt-bin.selekt-log) THEN
  MESSAGE "This will select all bins/tags selected from browser." SKIP
          "Do you want to continue?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll.
  IF NOT ll THEN RETURN NO-APPLY.

  SESSION:SET-WAIT-STATE("general").   

  FOR EACH tt-bin:
    IF tt-bin.selekt-log THEN
      ASSIGN
       tt-date         = tt-date + tt-bin.seq
       ld-selected-qty = ld-selected-qty + tt-bin.qty.

    ELSE
      DELETE tt-bin.
  END.

  ll = YES.
  IF ld-selected-qty GT io-qty THEN DO:
    ll = NO.
    MESSAGE "Bin/Tag Qty Exceeds Qty Required for job..." SKIP(1)
            "YES to Issue ALL Bin/Tags Selected"       SKIP
            "NO to Issue ONLY Qty Required for Job"

        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.
  END.
  IF ll THEN io-qty = ld-selected-qty.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select Dialog-Frame
ON CHOOSE OF Btn_Select IN FRAME Dialog-Frame /* Select All */
DO:
  {&browse-name}:SELECT-ALL ().
  RUN renumber-seq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
DEF VAR ll AS LOG NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FIND job-mat WHERE ROWID(job-mat) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL job-mat THEN DO:
    FOR EACH rm-rctd NO-LOCK
        WHERE rm-rctd.company   EQ job-mat.company
          AND rm-rctd.i-no      EQ job-mat.i-no
          AND rm-rctd.job-no    EQ job-mat.job-no
          AND rm-rctd.job-no2   EQ job-mat.job-no2
          AND rm-rctd.s-num     EQ job-mat.frm
          AND rm-rctd.rita-code EQ "I":
      io-qty = io-qty - rm-rctd.qty.
    END.

    FOR EACH rm-rcpth NO-LOCK
        WHERE rm-rcpth.company   EQ job-mat.company
          AND rm-rcpth.i-no      EQ job-mat.i-no
          AND rm-rcpth.job-no    EQ job-mat.job-no
          AND rm-rcpth.job-no2   EQ job-mat.job-no2
          AND rm-rcpth.rita-code EQ "I"
        USE-INDEX job,
        EACH rm-rdtlh NO-LOCK
        WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
          AND rm-rdtlh.s-num     EQ job-mat.frm
        USE-INDEX rm-rdtl:
      io-qty = io-qty - rm-rdtlh.qty.
    END.

    IF io-qty GT 0 THEN
    FIND FIRST item NO-LOCK
        WHERE item.company EQ job-mat.company
          AND item.i-no    EQ job-mat.i-no
        NO-ERROR.
  END.

  ll = NO.
  IF AVAIL item THEN  ll = YES.
     /*MESSAGE "Would you like to issue RM?"
       VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
     */
  IF ll THEN DO:
    ld = io-qty.

    IF item.cons-uom NE "EA" THEN
      RUN sys/ref/convquom.p (item.cons-uom,
                              "EA",
                              job-mat.basis-w,
                              job-mat.len,
                              job-mat.wid,
                              job-mat.dep,
                              ld,
                              OUTPUT ld).

    {sys/inc/roundup.i ld}

    FRAME {&FRAME-NAME}:TITLE = "Select Bins/Tags for "           +
                                TRIM(STRING(ld,">>,>>>,>>>")) +
                                " sheets needed of RM: "          +
                                TRIM(item.i-no).

    RUN initialize.

    IF NOT CAN-FIND(FIRST tt-bin) THEN
      MESSAGE "Sorry, no inventory in warehouse..."
          VIEW-AS ALERT-BOX.
 
    ELSE DO:
      RUN enable_UI.

      ASSIGN
       tt-bin.loc:READ-ONLY IN BROWSE {&browse-name} = YES
       tt-bin.loc-bin:READ-ONLY IN BROWSE {&browse-name} = YES
       tt-bin.tag:READ-ONLY IN BROWSE {&browse-name} = YES
       tt-bin.qty:READ-ONLY IN BROWSE {&browse-name} = YES
       tt-bin.qty-sht:READ-ONLY IN BROWSE {&browse-name} = YES.

      WAIT-FOR GO OF FRAME {&FRAME-NAME}.
    END.
  END.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY fi_seq fi_sel 
      WITH FRAME Dialog-Frame.
  ENABLE br-bin Btn_OK Btn_Select Btn_Deselect Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize Dialog-Frame 
PROCEDURE initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE tt-bin.

FOR EACH rm-bin
    WHERE rm-bin.company EQ item.company
      AND rm-bin.i-no    EQ item.i-no
    NO-LOCK
    BY rm-bin.tag:

  CREATE tt-bin.
  BUFFER-COPY rm-bin TO tt-bin
  ASSIGN
   tt-bin.rec-id     = RECID(rm-bin)
   tt-bin.cons-uom   = item.cons-uom
   tt-bin.tt-date    = TODAY
   tt-bin.selekt     = ""
   tt-bin.selekt-log = NO.

  FOR EACH rm-rctd NO-LOCK
      WHERE rm-rctd.company   EQ rm-bin.company
        AND rm-rctd.i-no      EQ rm-bin.i-no
        AND rm-rctd.loc       EQ rm-bin.loc
        AND rm-rctd.loc-bin   EQ rm-bin.loc-bin
        AND rm-rctd.tag       EQ rm-bin.tag
        AND rm-rctd.rita-code EQ "I":
    tt-bin.qty = tt-bin.qty - rm-rctd.qty.
  END.
END.

FOR EACH tt-bin:
  IF tt-bin.qty LE 0 THEN DELETE tt-bin.

  ELSE DO:
    tt-bin.qty-sht = tt-bin.qty.

    IF item.cons-uom NE "EA" THEN
      RUN sys/ref/convquom.p (item.cons-uom,
                              "EA",
                              job-mat.basis-w,
                              job-mat.len,
                              job-mat.wid,
                              job-mat.dep,
                              tt-bin.qty-sht,
                              OUTPUT tt-bin.qty-sht).

    {sys/inc/roundup.i tt-bin.qty-sht}

    tt-bin.qty-sel = tt-bin.qty-sht.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renumber-seq Dialog-Frame 
PROCEDURE renumber-seq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-tt-bin FOR tt-bin.

DEF VAR li AS INT NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR ld-sel AS DEC NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-rowid = IF AVAIL tt-bin THEN ROWID(tt-bin) ELSE ?.

    RUN set-select.

    FOR EACH b-tt-bin WHERE b-tt-bin.selekt-log EQ NO:
      b-tt-bin.seq = 0.
    END.

    li = 0.
    FOR EACH b-tt-bin
        WHERE b-tt-bin.selekt-log
          AND b-tt-bin.seq GT 0
          AND b-tt-bin.seq LT 1000
        BY b-tt-bin.seq:
      ASSIGN
       li          = li + 1
       b-tt-bin.seq = (li * 1000) + (IF ROWID(b-tt-bin) EQ lv-rowid THEN 2 ELSE 1).
    END.
    FOR EACH b-tt-bin
        WHERE b-tt-bin.selekt-log
          AND b-tt-bin.seq EQ 0:
      ASSIGN
       li          = li + 1
       b-tt-bin.seq = (li * 1000) + (IF ROWID(b-tt-bin) EQ lv-rowid THEN 2 ELSE 1).
    END.
    li = 0.
    FOR EACH b-tt-bin
        WHERE b-tt-bin.selekt-log
          AND b-tt-bin.seq GE 1000
        BY b-tt-bin.seq:
      ASSIGN
       li          = li + 1
       b-tt-bin.seq = li.
    END.

    ASSIGN
     fi_seq:SCREEN-VALUE = ""
     fi_sel:SCREEN-VALUE = ""
     ld-sel              = 0.

    FOR EACH b-tt-bin WHERE b-tt-bin.selekt-log BY b-tt-bin.seq:
      ASSIGN
       fi_seq:SCREEN-VALUE = STRING(b-tt-bin.seq)
       ld-sel              = ld-sel + b-tt-bin.qty-sht.
    END.

    fi_sel:SCREEN-VALUE = STRING(ld-sel).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-select Dialog-Frame 
PROCEDURE set-select :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER b-tt-bin FOR tt-bin.


  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH b-tt-bin:
      b-tt-bin.selekt-log = NO.
    END.
    IF {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN
    DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
      {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.
      IF AVAIL tt-bin THEN tt-bin.selekt-log = YES.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

