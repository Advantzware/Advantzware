&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: oerep\d-casetg.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{oerep/r-loadtg.i}

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF var v-loadtag AS char NO-UNDO initial "ASI".  /* sys ctrl option */
DEF VAR ll-tab-out AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-ord

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 w-ord.ord-no w-ord.job-no w-ord.job-no2 NO-LABEL w-ord.cust-no w-ord.i-no w-ord.prod-notes w-ord.ord-qty w-ord.pcs w-ord.bundle w-ord.partial /*w-ord.total-unit */ w-ord.total-tags w-ord.l-code w-ord.case-wt w-ord.lot# w-ord.rel-lot# w-ord.draw#   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 w-ord.prod-notes ~
 ~
w-ord.pcs ~
   w-ord.bundle ~
   w-ord.partial /* w-ord.total-unit */ ~
   w-ord.total-tags ~
   w-ord.l-code ~
   w-ord.case-wt ~
   w-ord.lot# ~
   w-ord.rel-lot# ~
   w-ord.draw# /*"Bdl/Case" AT 46  "Total#" AT 55  "Total Qty" AT 65 SKIP  "Order#"  "Cust # "  "Item #"  "Ord Qty" TO 44  "Count" AT 46  "Bdl/Case" AT 55  "Per Unit" AT 65  "Tags " TO 80 SKIP */   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 w-ord
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 w-ord
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH w-ord
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH w-ord.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 w-ord
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 w-ord


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 btn_copy Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_copy 
     LABEL "&Copy" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Done" 
     SIZE 15 BY 1.62
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      w-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 DISPLAY
      w-ord.ord-no  LABEL "Order#"
      w-ord.job-no  LABEL "  Job#"
      w-ord.job-no2 NO-LABEL FORMAT "99"
      w-ord.cust-no LABEL "Cust #"
      w-ord.i-no LABEL "Item #"
      w-ord.prod-notes LABEL "Color"
      w-ord.ord-qty COLUMN-LABEL "Ord Qty"
      w-ord.pcs FORM ">>>,>>9" COLUMN-LABEL "Bdl/Case!Count"
      w-ord.bundle FORM ">>>,>>9" COLUMN-LABEL "Bdl/Case!Per Unit"
      w-ord.partial COLUMN-LABEL "Partial"
      /*w-ord.total-unit FORM ">,>>>,>>9" COLUMN-LABEL "Total Qty!Per Unit" */
      w-ord.total-tags LABEL "Labels"
      w-ord.l-code COLUMN-LABEL "Code"
      w-ord.case-wt COLUMN-LABEL "Case!Weight"
      w-ord.lot# COLUMN-LABEL "FG Lot#"
      w-ord.rel-lot# COLUMN-LABEL "Rel. Lot#"
      w-ord.draw# COLUMN-LABEL "Drawing#"
      ENABLE w-ord.prod-notes 
             w-ord.pcs
             w-ord.bundle
             w-ord.partial /* w-ord.total-unit */
             w-ord.total-tags
             w-ord.l-code
             w-ord.case-wt
             w-ord.lot#
             w-ord.rel-lot#
             w-ord.draw#
/*"Bdl/Case" AT 46
  "Total#" AT 55
  "Total Qty" AT 65 SKIP
  "Order#"
  "Cust #  "
  "Item #"
  "Ord Qty" TO 44
  "Count" AT 46
  "Bdl/Case" AT 55
  "Per Unit" AT 65
  "Tags " TO 80 SKIP
 */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142 BY 12.86
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     btn_copy AT ROW 14.33 COL 47
     Btn_OK AT ROW 14.33 COL 83
     SPACE(45.19) SKIP(1.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 0
         TITLE "Case Label Creation Detail"
         DEFAULT-BUTTON Btn_OK.


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
OPEN QUERY {&SELF-NAME} FOR EACH w-ord.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Case Label Creation Detail */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON HELP OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR hlp-recid AS RECID NO-UNDO.


  CASE FOCUS:NAME :
    WHEN "prod-notes" THEN DO:
      RUN windows/l-prdnot.w (cocode, "", FOCUS:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val).
      IF char-val NE "" THEN FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val). 
    END.
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ROW-ENTRY OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  ll-tab-out = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_copy Dialog-Frame
ON CHOOSE OF btn_copy IN FRAME Dialog-Frame /* Copy */
DO:
   RUN copy-word.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Done */
DO:
  FOR EACH w-ord,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-ord.i-no:
    itemfg.prod-notes = w-ord.prod-notes.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
ON "LEAVE" OF w-ord.pcs IN BROWSE {&browse-name} DO:
    RUN calc-total.
END.
ON "LEAVE" OF w-ord.bundle IN BROWSE {&browse-name} DO:
    RUN calc-total.
END.
/*
ON "LEAVE" OF w-ord.total-unit IN BROWSE {&browse-name} DO:
    RUN calc-total.
END.
*/

ON 'LEAVE' OF w-ord.l-code IN BROWSE {&BROWSE-NAME} DO:
  w-ord.l-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} =
      CAPS(w-ord.l-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
END.

ON "LEAVE" OF w-ord.partial IN BROWSE {&browse-name} DO:
    RUN calc-total.
END.
ON "ENTRY" OF w-ord.prod-notes IN BROWSE {&browse-name} DO:
    IF ll-tab-out                                                   AND
       w-ord.prod-notes:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      ll-tab-out = NO.
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
    END.
END.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


  FIND FIRST sys-ctrl WHERE sys-ctrl.company eq g_company
                        AND sys-ctrl.name    eq "LOADTAG"
       NO-LOCK NO-ERROR.
  ASSIGN v-loadtag = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE v-loadtag.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-total Dialog-Frame 
PROCEDURE calc-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   IF AVAIL w-ord THEN DO:
      ASSIGN
            w-ord.pcs        = int(w-ord.pcs:SCREEN-VALUE IN BROWSE {&browse-name})
            w-ord.bundle     = int(w-ord.bundle:SCREEN-VALUE)
            w-ord.partial =  INT(w-ord.partial:SCREEN-VALUE)
            w-ord.total-unit = w-ord.pcs * w-ord.bundle /*+ w-ord.partial*/ 
           /*w-ord.partial = w-ord.ord-qty - w-ord.total-unit */
            w-ord.total-tags = ((w-ord.ord-qty / w-ord.pcs) + .49) 
                                + IF w-ord.partial > 0 THEN 1 ELSE 0.

      /*IF int(w-ord.partial:SCREEN-VALUE IN BROWSE {&browse-name}) < 0 THEN w-ord.partial = 0.
      IF int(w-ord.partial:SCREEN-VALUE IN BROWSE {&browse-name}) > 0 THEN w-ord.total-tags = 1.
      */
      DISPLAY /* w-ord.total-unit */ w-ord.total-tags w-ord.partial
             WITH BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-word Dialog-Frame 
PROCEDURE copy-word :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-word FOR w-ord.
  CREATE bf-word.
  BUFFER-COPY w-ord TO bf-word.
  ASSIGN bf-word.total-tags = 1.

   {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}

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
  ENABLE BROWSE-1 btn_copy Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

