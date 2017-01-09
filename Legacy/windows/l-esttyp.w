&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  def var ip-company like itemfg.company init "001" no-undo.
  def var ip-loc like est.loc init "MAIN" no-undo.
  DEF VAR ip-type AS cha NO-UNDO.
  DEF VAR ip-look-for AS cha NO-UNDO.
  def var ip-cur-val as cha no-undo.  
  def var op-char-val as cha no-undo. /* string i-code + i-name */
&else 
  def input parameter ip-company like itemfg.company no-undo.
  def input parameter ip-loc like est.loc no-undo.
  DEF INPUT PARAM ip-type AS cha NO-UNDO.
  DEF INPUT PARAM ip-look-for AS cha NO-UNDO.
  def input parameter ip-cur-val as cha no-undo.
  def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
&endif
def var lv-first-time as log init yes no-undo.
def var lv-type-dscr as cha no-undo.
DEF VAR lActive AS LOG NO-UNDO.
DEF VAR v-check-page AS LOG INIT NO NO-UNDO .
DEF VAR v-file-name AS CHAR NO-UNDO .
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = ip-company .

IF  PROGRAM-NAME(2) MATCHES "*/b-estq.w*" OR  PROGRAM-NAME(2) MATCHES "*/b-estq1.w*" THEN
    v-check-page = YES .

{sys/ref/CustList.i NEW}


&scoped-define fld-name-1 eb.est-no
&scoped-define fld-name-2 eb.cust-no
&scoped-define fld-name-3 eb.ship-name
&scoped-define fld-name-4 eb.part-no
/*
&scoped-define SORTBY-1 BY eb.est-no DESCENDING
&scoped-define SORTBY-2 BY eb.cust-no BY eb.est-no DESCENDING
&scoped-define SORTBY-3 BY eb.ship-name BY eb.est-no DESCENDING
&scoped-define SORTBY-4 BY eb.part-no BY eb.est-no DESCENDING
*/
&scoped-define SORTBY-1 BY eb.est-no DESCENDING
&scoped-define SORTBY-2 BY eb.cust-no 
&scoped-define SORTBY-3 BY eb.ship-name
&scoped-define SORTBY-4 BY eb.part-no
  
&global-define IAMWHAT LOOKUP
&SCOPED-DEFINE useMatches

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES eb

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 eb.est-no eb.cust-no eb.ship-name ~
eb.part-no eb.part-dscr1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = ip-company ~
and eb.loc = ip-loc and ~
index(ip-type,string(eb.est-type)) > 0 AND ~
    ((v-check-page AND ( (lookup(eb.cust-no,custcount) <> 0 AND eb.cust-no <> "") OR custcount = "")) OR NOT v-check-page) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = ip-company ~
and eb.loc = ip-loc and ~
index(ip-type,string(eb.est-type)) > 0 AND ~
    ((v-check-page AND ( (lookup(eb.cust-no,custcount) <> 0 AND eb.cust-no <> "") OR custcount = "")) OR NOT v-check-page) NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 eb
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 eb


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 RECT-1 rd-sort bt-clear lv-search ~
btn-enter bt-ok bt-cancel 
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

DEFINE BUTTON btn-enter 
     LABEL "&Enter" 
     SIZE 13 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Estimate#", 1,
"Customer#", 2,
"Customer Name", 3,
"Part#", 4
     SIZE 69 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      eb.est-no FORMAT "x(8)":U WIDTH 14 COLUMN-FONT 2
      eb.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U COLUMN-FONT 2
      eb.ship-name FORMAT "x(30)":U COLUMN-FONT 2
      eb.part-no FORMAT "x(15)":U COLUMN-FONT 2
      eb.part-dscr1 FORMAT "x(30)":U COLUMN-FONT 2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 114 BY 10.95
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     btn-enter AT ROW 14.1 COL 80
     bt-ok AT ROW 14.1 COL 93
     bt-cancel AT ROW 14.1 COL 103
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     RECT-1 AT ROW 12.43 COL 1
     SPACE(1.39) SKIP(1.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Estimate Information".


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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 TEXT-1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.eb"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "eb.company = ip-company
and eb.loc = ip-loc and
index(ip-type,string(eb.est-type)) > 0"
     _FldNameList[1]   > ASI.eb.est-no
"eb.est-no" ? "x(8)" "character" ? ? 2 ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.eb.cust-no
"eb.cust-no" "Customer#" ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.eb.ship-name
"eb.ship-name" ? ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.eb.part-no
"eb.part-no" ? ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.eb.part-dscr1
"eb.part-dscr1" ? ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Information */
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
   if lv-first-time then assign lv-search:screen-value = ""
                                lv-first-time = no.
                                
   lv-search:screen-value = lv-search:screen-value + keylabel(lastkey).
   apply "leave" to lv-search.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   op-char-val = eb.est-no + "," + eb.part-no.
   
   apply "window-close" to frame {&frame-name}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-clear Dialog-Frame
ON CHOOSE OF bt-clear IN FRAME Dialog-Frame /* Clear Find */
DO:
    assign lv-search:screen-value = "".
           lv-search = "".
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
        {srtord2.i 4}
    end.
    apply "entry" to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   op-char-val = eb.est-no + "," + eb.part-no.
   
   apply "window-close" to frame {&frame-name}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-enter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-enter Dialog-Frame
ON CHOOSE OF btn-enter IN FRAME Dialog-Frame /* Enter */
DO:
  /*&scoped-define IAMWHAT lookup
    
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
        {srtord2.i 4}
    end.    
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON LEAVE OF lv-search IN FRAME Dialog-Frame /* Search */
or return of lv-search
DO:
    assign rd-sort 
           lv-search.
    &scoped-define IAMWHAT Search
    &scoped-define where-statement begins lv-search 
    /*
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
    end.      
    */
    /* srtord2.i    resort include  same as srtord.i but use begins instead of >= and error */        


CASE rd-sort:
WHEN 1 THEN DO:
 
  OPEN QUERY {&browse-name} FOR EACH ASI.eb WHERE TRUE 
                            AND ASI.eb.company = ip-company 
                            and eb.loc = ip-loc 
                            AND INDEX(ip-type,STRING(eb.est-type)) > 0
                            AND (trim(eb.est-no) BEGINS lv-search OR ( eb.est-no MATCHES lv-search AND lv-search BEGINS "*"))
                             NO-LOCK.

  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
     DO:
        MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
    /*    lv-search:screen-value = "".  */
         APPLY "ENTRY" TO {&BROWSE-NAME}.
     end.    
END.
WHEN 2 THEN DO:
    &IF "{&IAMWHAT}" = "LOOKUP" &THEN
         &scoped-define sortby-phrase {&sortby-2}
    &ElseIF  "{&IAMWHAT}" = "Search" &THEN
     &IF DEFINED(useMatches) NE 0  &THEN
           &SCOPED-DEFINE KEY-PHRASE  ({&fld-name-2} BEGINS {&DATATYPE-2}(lv-search) OR ({&fld-name-2} MATCHES '*' + lv-search + '*' AND lv-search BEGINS '*' ))  
        &ELSE
           &SCOPED-DEFINE KEY-PHRASE  {&fld-name-2} BEGINS {&DATATYPE-2}(lv-search)
        &ENDIF
      /*&scoped-define where-statement begins {&datatype-2}(lv-search)
      &scoped-define key-phrase {&datatype-2}({&fld-name-2}) {&Where-statement}*/
    &endif

      {&open-query-{&browse-name}}

      IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
         DO:
            MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
            VIEW-AS ALERT-BOX.
        /*    lv-search:screen-value = "".  */
             APPLY "ENTRY" TO {&BROWSE-NAME}.
        end.    
END.

WHEN 3 THEN DO:
   &IF "{&IAMWHAT}" = "LOOKUP" &THEN
         &scoped-define sortby-phrase {&sortby-3}
    &ElseIF  "{&IAMWHAT}" = "Search" &THEN
    &IF DEFINED(useMatches) NE 0  &THEN
           &SCOPED-DEFINE KEY-PHRASE  ({&fld-name-3} BEGINS {&DATATYPE-3}(lv-search) OR ({&fld-name-3} MATCHES '*' + lv-search + '*' AND lv-search BEGINS '*' ))  
        &ELSE
           &SCOPED-DEFINE KEY-PHRASE  {&fld-name-3} BEGINS {&DATATYPE-3}(lv-search)
        &ENDIF
      /*&scoped-define where-statement begins {&datatype-3}(lv-search)
      
      &scoped-define key-phrase {&datatype-3}({&fld-name-3}) {&Where-statement} */
    &endif

      {&open-query-{&browse-name}}

      IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
         DO:
            MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
            VIEW-AS ALERT-BOX.
        /*    lv-search:screen-value = "".  */
             APPLY "ENTRY" TO {&BROWSE-NAME}.
        end.  
    
END.

WHEN 4 THEN DO:
    &IF "{&IAMWHAT}" = "LOOKUP" &THEN
         &scoped-define sortby-phrase {&sortby-4}
    &ElseIF  "{&IAMWHAT}" = "Search" &THEN
      &scoped-define where-statement begins {&datatype-4}(lv-search)
      &scoped-define key-phrase {&datatype-4}({&fld-name-4}) {&Where-statement}
    &endif

/*      {&open-query-{&browse-name}}*/
    OPEN QUERY {&browse-name} FOR EACH ASI.eb WHERE TRUE 
                              AND ASI.eb.company = ip-company 
                              and eb.loc = ip-loc 
                              AND INDEX(ip-type,STRING(eb.est-type)) > 0
                              AND (eb.part-no BEGINS lv-search OR ( eb.part-no MATCHES lv-search AND lv-search BEGINS "*"))
                              USE-INDEX part NO-LOCK.


      IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
         DO:
            MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
            VIEW-AS ALERT-BOX.
        /*    lv-search:screen-value = "".  */
             APPLY "ENTRY" TO {&BROWSE-NAME}.
        end.    
END.

END.  /* case */
  apply "entry" to {&browse-name}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
    assign rd-sort. 
      
    &scoped-define IAMWHAT LOOKUP  
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
        {srtord2.i 4}
    end.    
  
     
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
   &scoped-define key-phrase eb.company = ip-company AND eb.loc = ip-loc
  /* &scoped-define key-phrase {&fld-name-2} >= ip-cur-val
   lv-search:screen-value in frame {&frame-name} = ip-cur-val.
   */
   &scoped-define sortby-phrase {&sortby-1}
   &scoped-define IAMWHAT LOOKUP
   
   
   IF ip-look-for = "Part" THEN rd-sort = 4.

   RUN enable_UI.

   IF ip-type = "1234" THEN do:
   v-file-name  = "EF" .
   RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'EF',
                            INPUT YES,
                            OUTPUT lActive).
   END.
   ELSE DO:
       v-file-name  = "EC" .
       RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'EC',
                            INPUT YES,
                            OUTPUT lActive).
   END.
   {sys/inc/custlistform.i "v-file-name" }
   IF ou-cust-int = 0 THEN
        custcount = "".
   {sys/inc/chblankcust.i}


   DO WITH FRAME {&FRAME-NAME}:
  
     IF ip-look-for = "est" THEN DO:
        {custom/lookpos4.i &lookup-file = "eb" &lookup-field = "est-no"} 
     END.
 /*    ELSE IF ip-look-for = "Part" THEN DO:         
        {custom/lookpos4.i &lookup-file = "eb" &lookup-field = "part-no"} 
        
     END. */
   
     APPLY "value-changed" TO rd-sort.
     /*  APPLY "leave" TO lv-search. */
   END.

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
  DISPLAY rd-sort lv-search 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-1 RECT-1 rd-sort bt-clear lv-search btn-enter bt-ok bt-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

