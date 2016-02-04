&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: addon\windows\l-ldtag10.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input parameter ip-company like itemfg.company no-undo.
def input parameter ip-cur-val as cha no-undo.
DEF INPUT PARAMETER ip-item-no AS CHAR NO-UNDO.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */

def temp-table tt-loadtag like loadtag.

def var lv-type-dscr as cha no-undo.
DEF VAR ll-casetag AS LOG NO-UNDO.

&scoped-define fld-name-1 tt-loadtag.tag-no
&scoped-define fld-name-2 tt-loadtag.i-no
&scoped-define fld-name-3 trim(tt-loadtag.job-no)
&scoped-define SORTBY-1 BY tt-loadtag.tag-no
&scoped-define SORTBY-2 BY tt-loadtag.i-no
&scoped-define SORTBY-3 BY tt-loadtag.job-no

&global-define IAMWHAT LOOKUP

DEF VAR lv-first-time AS LOG INIT YES NO-UNDO.

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED }
{sys/inc/varasgn.i}

DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEF VAR period_pos AS INTEGER NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
   v-prgmname = USERID("NOSWEAT") + "..".
ELSE
   ASSIGN
      period_pos = INDEX(PROGRAM-NAME(1),".")
      v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
      v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-loadtag

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-loadtag.tag-no tt-loadtag.i-no tt-loadtag.i-name tt-loadtag.job-no tt-loadtag.job-no2 tt-loadtag.loc tt-loadtag.loc-bin tt-loadtag.ord-no tt-loadtag.po-no tt-loadtag.qty tt-loadtag.qty-case tt-loadtag.pallet-count   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-loadtag ~{&sortby-phrase}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-loadtag ~{&sortby-phrase}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-loadtag
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-loadtag


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 RECT-1 rd-sort bt-clear lv-search ~
bt-ok bt-cancel 
&Scoped-Define DISPLAYED-OBJECTS rd-sort lv-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 11 BY 1.14.

DEFINE BUTTON bt-clear 
     LABEL "C&lear Find" 
     SIZE 12.4 BY 1.14.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 10 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tag#", 1,
"Item#", 2,
"Job#", 3
     SIZE 56 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 137 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-loadtag SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-loadtag.tag-no FORMAT "X(23)":U
      tt-loadtag.i-no FORMAT "x(15)":U
      tt-loadtag.i-name FORMAT "x(30)":U
      tt-loadtag.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U
      tt-loadtag.job-no2 COLUMN-LABEL "" FORMAT ">9":U
      tt-loadtag.loc FORMAT "x(5)":U
      tt-loadtag.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      tt-loadtag.ord-no FORMAT ">>>>>9":U
      tt-loadtag.po-no COLUMN-LABEL "PO#" FORMAT ">>>>>9":U
      tt-loadtag.qty COLUMN-LABEL "Qty" FORMAT "->>>,>>>,>>9.9<<<<<":U
      tt-loadtag.qty-case FORMAT "->,>>>,>>9":U
      tt-loadtag.pallet-count FORMAT "->,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 137 BY 11.19
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 69
     bt-cancel AT ROW 14.1 COL 81
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     RECT-1 AT ROW 12.43 COL 1
     SPACE(0.19) SKIP(1.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Loadtag Information".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-loadtag ~{&sortby-phrase}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Loadtag Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ANY-PRINTABLE OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  if lv-first-time then
     assign lv-search:screen-value = ""
            lv-first-time = no.
                                
   lv-search:screen-value = lv-search:screen-value + keylabel(lastkey).
   apply "leave" to lv-search.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   IF AVAIL tt-loadtag THEN
      op-char-val = tt-loadtag.tag-no + "," +
                    tt-loadtag.i-no + ","   +
                    tt-loadtag.job-no + "," +
                    STRING(tt-loadtag.job-no2).
   
   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-clear Dialog-Frame
ON CHOOSE OF bt-clear IN FRAME Dialog-Frame /* Clear Find */
DO:
    EMPTY TEMP-TABLE tt-loadtag.
    RUN build-table.

    assign lv-search:screen-value = ""
           lv-search = "".
    case rd-sort:
        WHEN 1 THEN
           OPEN QUERY {&browse-name} FOR EACH tt-loadtag
                BY tt-loadtag.tag-no.
        
        WHEN 2 THEN
           OPEN QUERY {&browse-name} FOR EACH tt-loadtag
                BY tt-loadtag.i-no.

        WHEN 3 THEN
           OPEN QUERY {&browse-name} FOR EACH tt-loadtag
                BY tt-loadtag.job-no.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   IF AVAIL tt-loadtag THEN
      op-char-val = tt-loadtag.tag-no + "," +
                    tt-loadtag.i-no + ","   +
                    tt-loadtag.job-no + "," +
                    STRING(tt-loadtag.job-no2).
   
   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON LEAVE OF lv-search IN FRAME Dialog-Frame /* Search */
or return of lv-search
DO:
    IF NOT(LAST-EVENT:FUNCTION EQ "RETURN" OR
       LAST-EVENT:FUNCTION EQ "LEAVE") THEN
       RETURN.

    EMPTY TEMP-TABLE tt-loadtag.
    RUN build-table.

    assign rd-sort 
           lv-search.
    &scoped-define IAMWHAT Search
    &scoped-define where-statement BEGINS lv-search 
    case rd-sort:
        WHEN 1 THEN
           OPEN QUERY {&browse-name} FOR EACH tt-loadtag WHERE
                tt-loadtag.tag-no BEGINS lv-search
                BY tt-loadtag.tag-no.
        
        WHEN 2 THEN
           OPEN QUERY {&browse-name} FOR EACH tt-loadtag WHERE
               tt-loadtag.i-no BEGINS lv-search
               BY tt-loadtag.i-no.

        WHEN 3 THEN
        DO:
           lv-search = FILL(" ",6 - LENGTH(TRIM(lv-search))) + TRIM(lv-search).
           OPEN QUERY {&browse-name} FOR EACH tt-loadtag WHERE
                tt-loadtag.job-no BEGINS lv-search
                BY tt-loadtag.job-no.
        END.
    end.      
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
    assign rd-sort
           lv-search.
    &scoped-define IAMWHAT LOOKUP  
    
    ASSIGN
       lv-search = ""
       lv-search:SCREEN-VALUE = "".

    RUN new-rd-sort.
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
  /*
  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ ip-company
        AND sys-ctrl.name    EQ "CASETAG"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = ip-company
     sys-ctrl.name     = "CASETAG"
     sys-ctrl.descrip  = "Case Label Format?   Use Case Label as LoadTag?".
  END.
  ll-casetag = sys-ctrl.log-fld.
  */
   &scoped-define key-phrase {&fld-name-2} >= ip-cur-val
   lv-search:screen-value in frame {&frame-name} = ip-cur-val.
   
   &scoped-define sortby-phrase {&sortby-1}
   
   RUN build-table.

   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    lv-search:SCREEN-VALUE = "".
  END.

   RUN enable_UI.
   /*RUN new-rd-sort.*/

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table Dialog-Frame 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE tt-loadtag.

   FOR EACH rm-rcpth WHERE
       rm-rcpth.company EQ ip-company AND
       rm-rcpth.i-no EQ ip-item-no AND
       rm-rcpth.rita-code EQ "I"
       NO-LOCK,
       EACH rm-rdtlh OF rm-rcpth WHERE
            rm-rdtlh.company EQ ip-company AND
            rm-rdtlh.tag NE ""
            NO-LOCK,
       FIRST loadtag WHERE
             loadtag.company EQ ip-company AND
             loadtag.item-type = YES AND
             loadtag.tag-no EQ rm-rdtlh.tag
             NO-LOCK,
       FIRST rm-bin WHERE
             rm-bin.company EQ ip-company AND
             rm-bin.i-no EQ loadtag.i-no AND
             rm-bin.loc EQ loadtag.loc AND
             rm-bin.loc-bin EQ loadtag.loc-bin AND
             rm-bin.tag EQ loadtag.tag-no
             NO-LOCK:

       CREATE tt-loadtag.
       BUFFER-COPY loadtag TO tt-loadtag. 
       RELEASE tt-loadtag.
   END.
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
  DISPLAY rd-sort lv-search 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-1 RECT-1 rd-sort bt-clear lv-search bt-ok bt-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-rd-sort Dialog-Frame 
PROCEDURE new-rd-sort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* redefined for lookup */
  &scoped-define IAMWHAT LOOKUP   
         
  EMPTY TEMP-TABLE tt-loadtag.
  RUN build-table.

  DO WITH FRAME {&FRAME-NAME}:
    assign rd-sort.
    case rd-sort:
       WHEN 1 THEN
          OPEN QUERY {&browse-name} FOR EACH tt-loadtag
               BY tt-loadtag.tag-no.
       
       WHEN 2 THEN
          OPEN QUERY {&browse-name} FOR EACH tt-loadtag
               BY tt-loadtag.i-no.

       WHEN 3 THEN
          OPEN QUERY {&browse-name} FOR EACH tt-loadtag
               BY tt-loadtag.job-no.
    end.
  END.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  APPLY 'entry' TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

