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
&SCOPED-DEFINE autoFind
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{sys/inc/VAR.i "new shared"}
{custom/globdefs.i}

DEF INPUT PARAM ip-recid AS RECID NO-UNDO.

def temp-table w-bin field tag    like fg-bin.tag
                     field selekt as   char format "x"
                     field rec-id as   recid
                     FIELD job-no LIKE fg-bin.job-no
                     FIELD job-no2 LIKE fg-bin.job-no2
                     FIELD loc LIKE fg-bin.loc
                     FIELD loc-bin LIKE fg-bin.loc-bin
                     FIELD cust-no LIKE fg-bin.cust-no
                     FIELD qty LIKE fg-bin.qty
                     FIELD rec_key LIKE fg-bin.rec_key .
def var v-i-no  like oe-ordl.i-no NO-UNDO.
def var v-ord   like oe-ordl.ord-no NO-UNDO.
def var v-line  like oe-ordl.line NO-UNDO.
def var v-qty   like oe-rell.qty NO-UNDO.

DEF VAR nufile AS LOG NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEF VAR bolh_id AS RECID NO-UNDO.
DEF VAR boll_id AS RECID NO-UNDO.

ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME br-bin

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-bin

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-bin                                        */
&Scoped-define FIELDS-IN-QUERY-br-bin w-bin.job-no w-bin.job-no2 NO-LABEL w-bin.loc w-bin.loc-bin w-bin.tag w-bin.cust-no w-bin.qty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-bin   
&Scoped-define SELF-NAME br-bin
&Scoped-define QUERY-STRING-br-bin FOR EACH w-bin WHERE ~{&KEY-PHRASE} NO-LOCK ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-bin OPEN QUERY {&SELF-NAME} FOR EACH w-bin WHERE ~{&KEY-PHRASE} NO-LOCK ~{&SORTBY-PHRASE} .
&Scoped-define TABLES-IN-QUERY-br-bin w-bin
&Scoped-define FIRST-TABLE-IN-QUERY-br-bin w-bin


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-br-bin}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-bin RECT-4 browse-order Btn_Clear_Find ~
auto_find Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find fi_sortby 

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

DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear" 
     SIZE 10 BY 1
     FONT 4.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Find" 
     VIEW-AS FILL-IN 
     SIZE 30.6 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27.2 BY 1
     /*BGCOLOR 14*/ FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 39 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116.8 BY 1.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-bin FOR 
      w-bin SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-bin Dialog-Frame _FREEFORM
  QUERY br-bin DISPLAY
      w-bin.job-no   LABEL "Job#"
   w-bin.job-no2  NO-LABEL
   w-bin.loc      LABEL "Whs" WIDTH 5 FORMAT "X(5)"
   w-bin.loc-bin  LABEL "Bin" WIDTH 8 FORMAT "X(8)"
   w-bin.tag      LABEL "Tag#" FORMAT "X(30)"
   w-bin.cust-no  LABEL "Customer#"
   w-bin.qty      LABEL "Quantity" FORMAT "->>,>>>,>>9.99"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 117 BY 15.24
         BGCOLOR 8  ROW-HEIGHT-CHARS .71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     br-bin AT ROW 1 COL 1
     browse-order AT ROW 16.62 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.71 COL 53 COLON-ALIGNED NO-LABEL
     Btn_Clear_Find AT ROW 16.62 COL 96.2 HELP
          "CLEAR AUTO FIND Value"
     auto_find AT ROW 16.67 COL 60.4 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_OK AT ROW 18.91 COL 34
     Btn_Cancel AT ROW 18.91 COL 74
     RECT-4 AT ROW 16.38 COL 1.2
     SPACE(0.39) SKIP(2.61)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB br-bin TEXT-1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       fi_sortby:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-bin
/* Query rebuild information for BROWSE br-bin
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH w-bin
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-bin */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bins/Tags for */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-bin
&Scoped-define SELF-NAME br-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON ROW-ENTRY OF br-bin IN FRAME Dialog-Frame
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON ROW-LEAVE OF br-bin IN FRAME Dialog-Frame
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF br-bin IN FRAME Dialog-Frame
DO:
   DO WITH FRAME {&FRAME-NAME}:
    APPLY "choose" TO Btn_OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON START-SEARCH OF br-bin IN FRAME Dialog-Frame
DO:
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR li-cnt AS INT NO-UNDO.
  DEF VAR li-num-selected AS INT NO-UNDO.
   
  MESSAGE "This will select all bins/tags selected from Browser." SKIP
          "Do you want to continue?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
  IF NOT ll-ans THEN RETURN NO-APPLY.

  SESSION:SET-WAIT-STATE("general").   

  li-num-selected = BROWSE {&browse-name}:NUM-SELECTED-ROWS. 

  IF li-num-selected GT 0 THEN
     FOR EACH oe-boll WHERE
         oe-boll.company = oe-bolh.company AND
         oe-boll.b-no = oe-bolh.b-no AND
         oe-boll.i-no = inv-line.i-no AND
         oe-boll.line = inv-line.LINE:

         DELETE oe-boll.
     END.

  DO li-cnt = 1 TO li-num-selected:
    br-bin:FETCH-SELECTED-ROW(li-cnt).
    w-bin.selekt = "X".
  END.

  for each w-bin where w-bin.selekt eq "X", 
      first fg-bin
      where recid(fg-bin) eq w-bin.rec-id
      no-lock,
      first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq fg-bin.i-no
      no-lock
      break by w-bin.job-no
            by w-bin.job-no2
            by w-bin.loc
            by w-bin.loc-bin
            by w-bin.tag:

    bolh_id = recid(oe-bolh).

    FIND FIRST oe-boll
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no
          AND oe-boll.i-no    EQ inv-line.i-no
          AND oe-boll.line    EQ inv-line.line
          AND oe-boll.job-no  EQ fg-bin.job-no
          AND oe-boll.job-no2 EQ fg-bin.job-no2
          AND oe-boll.loc     EQ fg-bin.loc
          AND oe-boll.loc-bin EQ fg-bin.loc-bin
          AND oe-boll.tag     EQ fg-bin.tag
          AND oe-boll.cust-no EQ fg-bin.cust-no
        NO-ERROR.

    IF NOT AVAIL oe-boll THEN DO:
      {oe/oe-boll.a}
    END.

    ASSIGN
     oe-boll.job-no   = fg-bin.job-no
     oe-boll.job-no2  = fg-bin.job-no2
     oe-boll.loc      = fg-bin.loc
     oe-boll.loc-bin  = fg-bin.loc-bin
     oe-boll.tag      = fg-bin.tag
     oe-boll.cust-no  = fg-bin.cust-no
     oe-boll.i-no     = inv-line.i-no
     oe-boll.line     = inv-line.line
     oe-boll.s-code   = "B"
     oe-boll.po-no    = inv-line.po-no
     oe-boll.ord-no   = inv-line.ord-no
     oe-boll.deleted  = NO
     oe-boll.posted   = YES
     oe-boll.printed  = YES
     oe-boll.qty-case = IF fg-bin.case-count GT 0 THEN fg-bin.case-count
                        ELSE itemfg.case-count
     oe-boll.qty      = IF inv-line.ship-qty LT 0 THEN inv-line.ship-qty
                        ELSE MIN(oe-boll.qty + MIN(v-qty,fg-bin.qty),fg-bin.qty)
     oe-boll.cases    = TRUNC((oe-boll.qty - oe-boll.partial) / oe-boll.qty-case,0)
     oe-boll.partial  = oe-boll.qty - (oe-boll.qty-case * oe-boll.cases)
     oe-boll.bol-no   = oe-bolh.bol-no
     oe-boll.bol-date = oe-bolh.bol-date
     oe-boll.inv-no   = inv-line.inv-no
     oe-boll.weight   = oe-boll.qty / 100 * itemfg.weight-100
     v-qty            = v-qty - oe-boll.qty.
     
    if oe-boll.ord-no NE 0 then
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-boll.ord-no
          and oe-ordl.i-no    eq oe-boll.i-no 
        no-lock no-error.
   

    IF v-qty LE 0 THEN LEAVE.
  END.

  /* recalculate oe-rell.p-c field */
  FOR EACH oe-boll 
    WHERE oe-boll.company = oe-bolh.company 
      AND oe-boll.b-no = oe-bolh.b-no 
    EXCLUSIVE-LOCK
    BREAK BY oe-boll.ord-no BY oe-boll.i-no:
    IF FIRST-OF(oe-boll.i-no) THEN DO:
      if oe-boll.ord-no NE 0 then
        find first oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq oe-boll.ord-no
              and oe-ordl.i-no    eq oe-boll.i-no 
            no-lock no-error.
      {oe/oe-bolpc.i ALL}
    END. /* if last of */
  END. /* each oe-boll*/

  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
        AND oe-boll.i-no    EQ inv-line.i-no
        AND oe-boll.line    EQ inv-line.line
        AND oe-boll.qty     EQ 0:
    DELETE oe-boll.
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FIND inv-line WHERE RECID(inv-line) EQ ip-recid NO-LOCK.

  IF AVAIL inv-line THEN
  FIND FIRST oe-bolh
      WHERE oe-bolh.b-no eq inv-line.b-no
        AND oe-bolh.printed
        AND oe-bolh.posted
      NO-LOCK NO-ERROR.

  IF NOT AVAIL oe-bolh THEN RETURN.

  /*FOR EACH oe-boll FIELDS(qty)
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
        AND oe-boll.i-no    EQ inv-line.i-no
        AND oe-boll.line    EQ inv-line.line
        AND oe-boll.loc     NE ""
        AND oe-boll.loc-bin NE ""
      NO-LOCK:
    v-qty = v-qty + oe-boll.qty.
  END.
  
  v-qty = inv-line.ship-qty - v-qty.
  */

  v-qty = inv-line.ship-qty.
    
  IF v-qty EQ 0 THEN RETURN.

  FRAME {&FRAME-NAME}:TITLE = "Bins/Tags for " + inv-line.i-no +
                              "   (" + TRIM(STRING(v-qty,"->,>>>,>>>,>>>")) +
                              " needed)".
  RUN initialize.

  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
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
  DISPLAY browse-order auto_find fi_sortby 
      WITH FRAME Dialog-Frame.
  ENABLE br-bin RECT-4 browse-order Btn_Clear_Find auto_find Btn_OK 
         Btn_Cancel 
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

FOR EACH fg-bin
    WHERE fg-bin.company   EQ cocode
      AND fg-bin.i-no      EQ inv-line.i-no
      AND (TRIM(fg-bin.job-no) EQ "" OR
           NOT CAN-FIND(FIRST job
                        WHERE job.company EQ fg-bin.company
                          AND job.job-no  EQ fg-bin.job-no
                          AND job.job-no2 EQ fg-bin.job-no2
                          AND job.stat    EQ "H"))
    NO-LOCK
    BY fg-bin.job-no
    BY fg-bin.job-no2
    BY fg-bin.loc
    BY fg-bin.loc-bin
    BY fg-bin.tag:

  CREATE w-bin.
  ASSIGN
   w-bin.tag     = fg-bin.tag
   w-bin.rec-id  = RECID(fg-bin)
   w-bin.selekt  = ""
   w-bin.job-no  = fg-bin.job-no
   w-bin.job-no2 = fg-bin.job-no2
   w-bin.loc     = fg-bin.loc
   w-bin.loc-bin = fg-bin.loc-bin
   w-bin.qty     = fg-bin.qty.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

