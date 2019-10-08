&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: cec\b-updmul.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ip-company AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-locode AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-est-no AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-line AS INT NO-UNDO.
                    
/* Local Variable Definitions ---                                       */
DEF BUFFER b-probemk FOR reftable.
DEF BUFFER xest FOR est.
DEF BUFFER xeb FOR eb.

DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR locode AS CHAR NO-UNDO.

ASSIGN
   cocode = ip-company
   locode = ip-locode.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-5

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES probeit eb

/* Definitions for BROWSE BROWSE-5                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-5 probeit.cust-no probeit.part-no probeit.bl-qty probeit.yld-qty probeit.fact-cost probeit.full-cost probeit.sell-price probeit.YRprice   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-5 probeit.sell-price   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-5 probeit
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-5 probeit
&Scoped-define SELF-NAME BROWSE-5
&Scoped-define QUERY-STRING-BROWSE-5 FOR EACH probeit WHERE probeit.company = ip-company   AND probeit.est-no = ip-est-no   AND probeit.line = ip-line NO-LOCK, ~
             FIRST eb WHERE eb.company = probeit.company   AND eb.est-no = probeit.est-no   AND eb.part-no = probeit.part-no NO-LOCK     BY eb.form-no     BY eb.blank-no
&Scoped-define OPEN-QUERY-BROWSE-5 OPEN QUERY {&SELF-NAME} FOR EACH probeit WHERE probeit.company = ip-company   AND probeit.est-no = ip-est-no   AND probeit.line = ip-line NO-LOCK, ~
             FIRST eb WHERE eb.company = probeit.company   AND eb.est-no = probeit.est-no   AND eb.part-no = probeit.part-no NO-LOCK     BY eb.form-no     BY eb.blank-no.
&Scoped-define TABLES-IN-QUERY-BROWSE-5 probeit eb
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-5 probeit
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-5 eb


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-5}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-5 FOR 
      probeit, 
      eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-5 Dialog-Frame _FREEFORM
  QUERY BROWSE-5 DISPLAY
      probeit.cust-no FORMAT "x(8)":U WIDTH 12
      probeit.part-no COLUMN-LABEL "Part Number" FORMAT "x(20)":U
            LABEL-BGCOLOR 14
      probeit.bl-qty COLUMN-LABEL "Requested Qty" FORMAT ">>>,>>>,>>>":U
      probeit.yld-qty FORMAT ">>>,>>>,>>>":U
      probeit.fact-cost COLUMN-LABEL "Fact!Cost/M" FORMAT ">>>,>>9.99":U
      probeit.full-cost COLUMN-LABEL "Full!Cost/M" FORMAT ">>>,>>9.99":U
      probeit.sell-price COLUMN-LABEL "Sell!Price/M" FORMAT ">>>,>>9.99":U
      probeit.YRprice COLUMN-LABEL "On" FORMAT "Y/R":U
  ENABLE
      probeit.sell-price
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 120 BY 12.14
         BGCOLOR 8  ROW-HEIGHT-CHARS .65 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-5 AT ROW 1.48 COL 3 WIDGET-ID 200
     SPACE(1.59) SKIP(0.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Update Multiple Sell Prices" WIDGET-ID 100.


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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-5 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-5
/* Query rebuild information for BROWSE BROWSE-5
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH probeit WHERE probeit.company = ip-company
  AND probeit.est-no = ip-est-no
  AND probeit.line = ip-line NO-LOCK,
      FIRST eb WHERE eb.company = probeit.company
  AND eb.est-no = probeit.est-no
  AND eb.part-no = probeit.part-no NO-LOCK
    BY eb.form-no
    BY eb.blank-no.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-5 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update Multiple Sell Prices */
DO:
  IF AVAIL probeit THEN
     APPLY 'leave':U TO probeit.sell-price IN BROWSE {&browse-name}.

  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-5
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ON 'leave':U OF probeit.sell-price DO:
   IF AVAIL probeit AND
      DEC(probeit.sell-price:SCREEN-VALUE IN BROWSE browse-5) NE 
      probeit.sell-price THEN
      RUN calc-fields.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:


DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-fields Dialog-Frame 
PROCEDURE calc-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR ld-commc AS DEC NO-UNDO.
   DEF VAR ld-factc AS DEC NO-UNDO.
   DEF VAR ld-fullc AS DEC NO-UNDO.
   DEF VAR ld-price AS DEC NO-UNDO.
   DEF VAR lv-changed AS cha NO-UNDO.

   FIND FIRST xest WHERE
        xest.company = probe.company AND
        xest.est-no = probe.est-no
        NO-LOCK NO-ERROR.
  
   {cec/combasis.i}
       
       
   FIND FIRST b-probemk
       WHERE b-probemk.reftable EQ "ce/com/probemk.p"
         AND b-probemk.company  EQ probeit.company
         AND b-probemk.loc      EQ probeit.est-no
         AND b-probemk.code     EQ STRING(probeit.line,"9999999999")
         AND b-probemk.code2    EQ probeit.part-no
       NO-ERROR.
   IF AVAIL b-probemk THEN
     v-com = b-probemk.val[2] + b-probemk.val[3] +
             b-probemk.val[4] + b-probemk.val[5].
  
   {sys/inc/ceround.i}
   lv-changed = "S".
  
   DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
      ld-price = probeit.sell-price
      ld-factc = DEC(probeit.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name})
      ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                 (v-com / 100)   
      ld-fullc = DEC(probeit.full-cost:SCREEN-VALUE IN BROWSE {&browse-name})
                 - ld-commc
      ld-price = DEC(probeit.sell-price:SCREEN-VALUE IN BROWSE {&browse-name})
      ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                 (v-com / 100)
      ld-fullc = ld-fullc + ld-commc.

     FIND CURRENT probeit.

     probeit.full-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(ld-fullc,probeit.full-cost:FORMAT IN BROWSE {&browse-name}) NO-ERROR.

     ASSIGN
        probeit.full-cost = DEC(probeit.full-cost:SCREEN-VALUE IN BROWSE {&browse-name})
        probeit.sell-price = DEC(probeit.sell-price:SCREEN-VALUE). /*need for window-close event*/
     FIND CURRENT probeit NO-LOCK.
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
  ENABLE BROWSE-5 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

