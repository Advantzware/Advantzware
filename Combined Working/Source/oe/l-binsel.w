&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-rowid-h AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-rowid-l AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF TEMP-TABLE tt-binsel NO-UNDO LIKE fg-bin
    FIELD job# AS CHAR
    FIELD cases-out LIKE fg-bin.cases
    FIELD partial-out LIKE fg-bin.partial-count
    FIELD qty-out LIKE fg-bin.qty
    FIELD row-id AS ROWID
    FIELD tt-select AS LOG.

DEF VAR lv-first-time AS LOG INIT YES NO-UNDO.
&SCOPED-DEFINE FLD-NAME-1 tt-binsel.i-no
&SCOPED-DEFINE FLD-NAME-2 tt-binsel.job#
&SCOPED-DEFINE FLD-NAME-3 tt-binsel.loc
&SCOPED-DEFINE FLD-NAME-4 tt-binsel.loc-bin
&SCOPED-DEFINE FLD-NAME-5 tt-binsel.tag
&SCOPED-DEFINE SORTBY-1 BY {&FLD-NAME-1} BY tt-binsel.job# BY tt-binsel.loc BY tt-binsel.loc-bin BY tt-binsel.tag BY tt-binsel.cust-no 
&SCOPED-DEFINE SORTBY-2 BY {&FLD-NAME-2} {&SORTBY-1}
&SCOPED-DEFINE SORTBY-3 BY {&FLD-NAME-3} {&SORTBY-1}
&SCOPED-DEFINE SORTBY-4 BY {&FLD-NAME-4} {&SORTBY-1}
&SCOPED-DEFINE SORTBY-5 BY {&FLD-NAME-5} {&SORTBY-1}
&SCOPED-DEFINE IAMWHAT LOOKUP

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-binsel

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-binsel.i-no tt-binsel.job# tt-binsel.loc tt-binsel.loc-bin tt-binsel.tag tt-binsel.cust-no tt-select tt-binsel.cases-out tt-binsel.case-count tt-binsel.cases-unit tt-binsel.partial-out tt-binsel.qty-out   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table tt-select ~
   tt-binsel.cases-out ~
   tt-binsel.partial-out ~
   tt-binsel.qty-out   
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table tt-binsel
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table tt-binsel
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-binsel WHERE ~{&KEY-PHRASE} ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH tt-binsel WHERE ~{&KEY-PHRASE} ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-binsel
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-binsel


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-Browser-Table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table rd-sort /*Btn_Select*/ ~
/*Btn_Deselect*/ lv-search Btn_Clear_Find Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS rd-sort lv-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE BUTTON Btn_Deselect 
     LABEL "Unselect All" 
     SIZE 13 BY 1.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Select 
     LABEL "Select All" 
     SIZE 13 BY 1.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 116 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "FG Item#", 1,
"Job#", 2,
"Whs", 3,
"Bin", 4,
"Tag#", 5
     SIZE 123 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-binsel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table Dialog-Frame _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-binsel.i-no                            LABEL "FG Item#"    WIDTH 23
      tt-binsel.job#          FORMAT "x(9)"     LABEL "Job#"        WIDTH 13
      tt-binsel.loc                             LABEL "Whse"        WIDTH 10
      tt-binsel.loc-bin                         LABEL "Bin"         WIDTH 14
      tt-binsel.tag           FORMAT "x(25)"    LABEL "Tag"         WIDTH 32
      tt-binsel.cust-no                         LABEL "Cust#"       WIDTH 13
      tt-select                                 LABEL "Selected?"
      tt-binsel.cases-out     FORMAT "->>,>>9"  LABEL "Units"
      tt-binsel.case-count    FORMAT ">>>,>>9"  LABEL "Unit Count"
      tt-binsel.cases-unit    FORMAT ">>>,>>9"  LABEL "Units/!Pallet"
      tt-binsel.partial-out   FORMAT "->>,>>9"  LABEL "Partial"
      tt-binsel.qty-out       FORMAT "->,>>>,>>9"
      ENABLE /*tt-select*/
             tt-binsel.cases-out
             tt-binsel.partial-out
             tt-binsel.qty-out
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 156 BY 17.62
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     rd-sort AT ROW 18.86 COL 6 NO-LABEL
     /*Btn_Select AT ROW 18.86 COL 130
     Btn_Deselect AT ROW 18.86 COL 144*/
     lv-search AT ROW 20.05 COL 11 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 20.05 COL 130 HELP
          "CLEAR AUTO FIND Value"
     Btn_OK AT ROW 20.05 COL 144
     "By:" VIEW-AS TEXT
          SIZE 4 BY .76 AT ROW 18.86 COL 2
     SPACE(151.59) SKIP(1.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bin Locations".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB Browser-Table 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN  tt-select:VISIBLE IN BROWSE {&browse-name} = FALSE.
      

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-binsel WHERE ~{&KEY-PHRASE} ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Query            is OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bin Locations */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table Dialog-Frame
ON ANY-PRINTABLE OF Browser-Table IN FRAME Dialog-Frame
DO:
  IF lv-first-time THEN ASSIGN lv-search:SCREEN-VALUE = ""
                               lv-first-time = NO.
                                
  lv-search:SCREEN-VALUE = lv-search:SCREEN-VALUE + KEYLABEL(LASTKEY).
  APPLY "leave" TO lv-search.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table Dialog-Frame
ON DEFAULT-ACTION OF Browser-Table IN FRAME Dialog-Frame
DO:
  /* op-rowid-val = ROWID(fg-bin). */
  IF AVAIL tt-binsel THEN
  tt-select = /*LOGICAL(tt-select:SCREEN-VALUE IN BROWSE {&Browse-name})*/ YES  .
  RUN set-output.
  /*APPLY "window-close" TO FRAME {&FRAME-NAME}.*/
  APPLY "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear_Find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear_Find Dialog-Frame
ON CHOOSE OF Btn_Clear_Find IN FRAME Dialog-Frame /* Clear Find */
DO:
  ASSIGN lv-search:SCREEN-VALUE = "".
         lv-search = "".
  CASE rd-sort:
       {srtord2.i 1}
       {srtord2.i 2}
       {srtord2.i 3}
       {srtord2.i 4}
       {srtord2.i 5}
  END.
  APPLY "entry" TO {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*
&Scoped-define SELF-NAME Btn_Deselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deselect Dialog-Frame
ON CHOOSE OF Btn_Deselect IN FRAME Dialog-Frame /* Unselect All */
DO:
  RUN sel-all (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME*/


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:         
  /* op-rowid-val = ROWID(fg-bin). */

  IF AVAIL tt-binsel THEN
     tt-select = /*LOGICAL(tt-select:SCREEN-VALUE IN BROWSE {&Browse-name})*/ YES  .

  RUN set-output.
  APPLY "window-close" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*
&Scoped-define SELF-NAME Btn_Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select Dialog-Frame
ON CHOOSE OF Btn_Select IN FRAME Dialog-Frame /* Select All */
DO:
  RUN sel-all (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME  */


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON LEAVE OF lv-search IN FRAME Dialog-Frame /* Auto Find */
DO:
  ASSIGN rd-sort 
         lv-search.
  &SCOPED-DEFINE IAMWHAT SEARCH
  CASE rd-sort:
       {srtord2.i 1}
       {srtord2.i 2}
       {srtord2.i 3}
       {srtord2.i 4}
       {srtord2.i 5}
  END.      
  APPLY "entry" TO {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
  &SCOPED-DEFINE IAMWHAT LOOKUP
  ASSIGN rd-sort.
  CASE rd-sort:
       {srtord2.i 1}
       {srtord2.i 2}
       {srtord2.i 3}
       {srtord2.i 4}
       {srtord2.i 5}
  END.    
  APPLY "entry" TO {&browse-name}.
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

  ON MOUSE-SELECT-CLICK OF tt-select IN BROWSE Browser-Table 
  DO:
    IF tt-select:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes" THEN
      tt-select:SCREEN-VALUE IN BROWSE {&browse-name} = "No".
    ELSE
      tt-select:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes".

    RETURN NO-APPLY.
  END.

  ON ENTRY OF tt-binsel.cases-out IN BROWSE Browser-Table 
  DO:
    IF tt-select:SCREEN-VALUE IN BROWSE {&browse-name} EQ "No" THEN DO:
      APPLY "leave" TO tt-binsel.cases-out.
      RETURN NO-APPLY.
    END.
  END.

  ON VALUE-CHANGED OF tt-binsel.cases-out IN BROWSE Browser-Table 
  DO:
    tt-binsel.qty-out:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING((DEC(tt-binsel.cases-out:SCREEN-VALUE IN BROWSE {&browse-name}) *
                DEC(tt-binsel.case-count:SCREEN-VALUE IN BROWSE {&browse-name})) +
                DEC(tt-binsel.partial-out:SCREEN-VALUE IN BROWSE {&browse-name})).
  END.

  /*ON ENTRY OF tt-binsel.partial-out IN BROWSE Browser-Table 
  DO:
    IF tt-select:SCREEN-VALUE IN BROWSE {&browse-name} EQ "No" THEN DO:
      APPLY "leave" TO tt-binsel.partial-out.
      RETURN NO-APPLY.
    END.
  END.*/

  ON VALUE-CHANGED OF tt-binsel.partial-out IN BROWSE Browser-Table 
  DO:
    tt-binsel.qty-out:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING((DEC(tt-binsel.cases-out:SCREEN-VALUE IN BROWSE {&browse-name}) *
                DEC(tt-binsel.case-count:SCREEN-VALUE IN BROWSE {&browse-name})) +
               DEC(tt-binsel.partial-out:SCREEN-VALUE IN BROWSE {&browse-name})).
  END.

  ON ENTRY OF tt-binsel.qty-out IN BROWSE Browser-Table 
  DO:
    IF tt-select:SCREEN-VALUE IN BROWSE {&browse-name} EQ "No" THEN DO:
      APPLY "tab" TO tt-binsel.qty-out.
      RETURN NO-APPLY.
    END.
  END.

  ON VALUE-CHANGED OF tt-binsel.qty-out IN BROWSE Browser-Table 
  DO:
    ASSIGN
     tt-binsel.cases-out:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(TRUNC(DEC(tt-binsel.qty-out:SCREEN-VALUE IN BROWSE {&browse-name}) /
                     DEC(tt-binsel.case-count:SCREEN-VALUE IN BROWSE {&browse-name}),0))
     tt-binsel.partial-out:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(DEC(tt-binsel.qty-out:SCREEN-VALUE IN BROWSE {&browse-name}) -
               (DEC(tt-binsel.cases-out:SCREEN-VALUE IN BROWSE {&browse-name}) *
                DEC(tt-binsel.case-count:SCREEN-VALUE IN BROWSE {&browse-name}))).
  END.

   
  locode = g_loc.

  FIND oe-relh WHERE ROWID(oe-relh) EQ ip-rowid-h NO-LOCK NO-ERROR.

  IF AVAIL oe-relh THEN DO:
    cocode = oe-relh.company.

    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-relh.company
          AND oe-ord.cust-no EQ oe-relh.cust-no ,
        EACH oe-ordl 
        WHERE oe-ordl.company  EQ oe-ord.company
          AND oe-ordl.opened  EQ YES
          AND oe-ordl.ord-no   EQ oe-ord.ord-no
          AND oe-ordl.i-no     NE ""
          AND oe-ordl.qty      GT 0 USE-INDEX opened NO-LOCK:

     /* IF NOT CAN-FIND(FIRST tt-binsel
                      WHERE tt-binsel.company EQ oe-ordl.company
                        AND tt-binsel.i-no    EQ oe-ordl.i-no
                        AND (tt-binsel.job-no    EQ oe-ordl.job-no  ) ) THEN*/
      FOR EACH fg-bin NO-LOCK
          WHERE fg-bin.company EQ oe-ordl.company
            AND fg-bin.i-no    EQ oe-ordl.i-no
            AND (fg-bin.job-no  EQ oe-ordl.job-no ) 
            AND fg-bin.loc     NE ""
            AND fg-bin.loc-bin NE ""
            AND fg-bin.qty     GT 0:

        FIND FIRST tt-binsel
                      WHERE tt-binsel.company EQ oe-ordl.company
                        AND tt-binsel.i-no    EQ oe-ordl.i-no
                        AND (tt-binsel.job-no    EQ oe-ordl.job-no  ) NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-binsel THEN do:
          
        CREATE tt-binsel.
        BUFFER-COPY fg-bin TO tt-binsel
        ASSIGN
         tt-binsel.row-id      = ROWID(fg-bin)
         tt-binsel.job#        = TRIM(fg-bin.job-no) + "-" +
                                 STRING(fg-bin.job-no2,"99")
         tt-binsel.cases       = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                                       fg-bin.case-count,0)
         tt-binsel.cases-out   = tt-binsel.cases
         tt-binsel.partial-out = tt-binsel.partial-count
         tt-binsel.qty-out     = tt-binsel.qty.

        IF TRIM(tt-binsel.job#) EQ "-00" THEN tt-binsel.job# = "".
        END.
        
      END.
    END.
  END.
  
  IF CAN-FIND(FIRST tt-binsel) THEN DO:
    FRAME dialog-frame:TITLE = "FG Item " + TRIM(FRAME dialog-frame:TITLE) +
                               " from Open Order Line Items of Cust#: " +
                               TRIM(oe-relh.cust-no).

    RUN enable_UI.

    APPLY "entry" TO lv-search.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  END.

  ELSE
    MESSAGE "No FG Bins found for open orders for this customer..."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-oe-rell Dialog-Frame 
PROCEDURE create-oe-rell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR nufile AS LOG NO-UNDO.
  DEF VAR fil_id AS RECID NO-UNDO.
  DEF VAR bolh_id AS RECID NO-UNDO.
  DEF VAR li-rel-qty AS INT NO-UNDO.


  IF tt-binsel.qty-out GT 0 THEN
  FOR EACH oe-ordl NO-LOCK
      WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no
        AND oe-ordl.i-no    EQ fg-bin.i-no
      BY oe-ordl.line:

    RUN oe/rell-qty.p (ROWID(oe-ordl), OUTPUT li-rel-qty).

    li-rel-qty = oe-ordl.qty - li-rel-qty.
 
    IF li-rel-qty GT 0 THEN DO:
      {oe/oe-rell.a}
      ASSIGN
       oe-rell.ord-no   = oe-ordl.ord-no
       oe-rell.i-no     = oe-ordl.i-no
       oe-rell.line     = oe-ordl.line
       oe-rell.job-no   = fg-bin.job-no
       oe-rell.job-no2  = fg-bin.job-no2
       oe-rell.loc      = fg-bin.loc
       oe-rell.loc-bin  = fg-bin.loc-bin
       oe-rell.tag      = fg-bin.tag
       oe-rell.cust-no  = fg-bin.cust-no
       oe-rell.deleted  = NO
       oe-rell.posted   = NO
       oe-rell.printed  = NO
       oe-rell.qty-case = tt-binsel.case-count
       oe-rell.cases    = tt-binsel.cases-out
       oe-rell.partial  = tt-binsel.partial-out
       oe-rell.qty      = MIN(tt-binsel.qty-out,li-rel-qty).

      op-rowid-l = ROWID(oe-rell) .

      IF oe-rell.qty LT tt-binsel.qty-out THEN DO:
        IF oe-rell.cases GT TRUNC((oe-rell.qty - tt-binsel.partial-out) / oe-rell.qty-case,0) THEN
          oe-rell.cases = TRUNC((oe-rell.qty - tt-binsel.partial-out) / oe-rell.qty-case,0).

        tt-binsel.partial-out = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case).

        IF tt-binsel.partial-out GE oe-rell.qty-case AND tt-binsel.partial-out EQ 0 THEN
          oe-rell.cases = TRUNC(oe-rell.qty / oe-rell.qty-case,0).

        tt-binsel.partial-out = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case).
      END.

      ASSIGN
       tt-binsel.cases-out   = tt-binsel.cases-out - oe-rell.cases
       tt-binsel.qty-out     = tt-binsel.qty-out - oe-rell.qty.

      IF tt-binsel.cases-out GT TRUNC((tt-binsel.qty-out - tt-binsel.partial-out) / tt-binsel.case-count,0) THEN
        tt-binsel.cases-out = TRUNC((tt-binsel.qty-out - tt-binsel.partial-out) / tt-binsel.case-count,0).

      tt-binsel.partial-out = tt-binsel.qty-out - (tt-binsel.cases-out * tt-binsel.case-count).

      IF tt-binsel.partial-out GE tt-binsel.case-count AND tt-binsel.partial-out EQ 0 THEN
        tt-binsel.cases-out = TRUNC(tt-binsel.qty-out / tt-binsel.case-count,0).

      tt-binsel.partial-out = tt-binsel.qty-out - (tt-binsel.cases-out * tt-binsel.case-count).
    END.
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
  ENABLE Browser-Table rd-sort /*Btn_Select Btn_Deselect*/ lv-search Btn_Clear_Find 
         Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sel-all Dialog-Frame 
PROCEDURE sel-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-sel AS LOG NO-UNDO.

  DEF VAR lv-rowid AS ROWID NO-UNDO.


  lv-rowid = IF AVAIL tt-binsel THEN ROWID(tt-binsel) ELSE ?.

  FOR EACH tt-binsel:
    tt-select = ip-sel.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "leave" TO lv-search.
    REPOSITION {&browse-name} TO ROWID lv-rowid NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-output Dialog-Frame 
PROCEDURE set-output :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /*IF CAN-FIND(FIRST tt-binsel WHERE tt-select EQ YES) THEN
  MESSAGE "This will create release lines for all bins/tags selected from browser." SKIP
          "Do you want to continue?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll-ans AS LOG.
  IF NOT ll-ans THEN RETURN NO-APPLY.*/

  SESSION:SET-WAIT-STATE("general").   

  DO WITH FRAME {&FRAME-NAME}:
    GET FIRST {&browse-name}.

    DO WHILE AVAIL tt-binsel:
      FIND FIRST fg-bin
          WHERE ROWID(fg-bin) EQ tt-binsel.row-id
            AND tt-select EQ YES
          NO-LOCK NO-ERROR.
    
      IF AVAIL fg-bin THEN DO:

        IF fg-bin.ord-no NE 0 THEN
        FOR EACH oe-ord NO-LOCK
            WHERE oe-ord.company EQ oe-relh.company
              AND oe-ord.ord-no  EQ fg-bin.ord-no
              AND oe-ord.cust-no EQ oe-relh.cust-no
              AND oe-ord.opened  EQ YES:
            
          RUN create-oe-rell.
        END.
        ELSE IF CAN-FIND(FIRST job-hdr WHERE
             job-hdr.company EQ oe-relh.company AND
             job-hdr.job-no EQ fg-bin.job-no AND
             job-hdr.job-no2 EQ fg-bin.job-no2 AND
             job-hdr.ord-no NE 0) THEN
             FOR EACH job-hdr WHERE
                 job-hdr.company EQ oe-relh.company AND
                 job-hdr.job-no EQ fg-bin.job-no AND
                 job-hdr.job-no2 EQ fg-bin.job-no2 AND
                 job-hdr.ord-no NE 0
                 NO-LOCK,
                 FIRST oe-ord WHERE
                       oe-ord.company EQ oe-relh.company AND
                       oe-ord.ord-no EQ job-hdr.ord-no AND
                       oe-ord.cust-no EQ oe-relh.cust-no AND
                       oe-ord.opened EQ YES
                       NO-LOCK
                 BY job-hdr.ord-no:
                 
                 RUN create-oe-rell.
             END.
        ELSE
        FOR EACH oe-ord NO-LOCK
            WHERE oe-ord.company EQ oe-relh.company
              AND oe-ord.cust-no EQ oe-relh.cust-no
              AND oe-ord.opened  EQ YES
            BY oe-ord.ord-no:
          RUN create-oe-rell.
        END.
      END.
          
      GET NEXT {&browse-name}.
    END.
  END.

  SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

