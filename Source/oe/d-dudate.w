&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
    
{sys/inc/VAR.i "new shared"}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-relh

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame oe-relh.rel-date 
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH oe-relh SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH oe-relh SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame oe-relh
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame oe-relh


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-40 tb_hdr-due tb_hdr-shp tb_lin-due ~
tb_lin-shp Btn_OK 
&Scoped-Define DISPLAYED-FIELDS oe-relh.rel-date 
&Scoped-define DISPLAYED-TABLES oe-relh
&Scoped-define FIRST-DISPLAYED-TABLE oe-relh
&Scoped-Define DISPLAYED-OBJECTS tb_hdr-due tb_hdr-shp tb_lin-due ~
tb_lin-shp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 9.52.

DEFINE VARIABLE tb_hdr-due AS LOGICAL INITIAL yes 
     LABEL "Header Due Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_hdr-shp AS LOGICAL INITIAL yes 
     LABEL "Header Last Ship Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE tb_lin-due AS LOGICAL INITIAL yes 
     LABEL "Line Item Due Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_lin-shp AS LOGICAL INITIAL yes 
     LABEL "Line Item Last Ship Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      oe-relh SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     oe-relh.rel-date AT ROW 2.19 COL 54 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     tb_hdr-due AT ROW 3.86 COL 21
     tb_hdr-shp AT ROW 5.29 COL 21
     tb_lin-due AT ROW 6.71 COL 21
     tb_lin-shp AT ROW 8.14 COL 21
     Btn_OK AT ROW 11.24 COL 30
     "Change the following dates on the order(s) to" VIEW-AS TEXT
          SIZE 52 BY 1 AT ROW 2.19 COL 4
     RECT-40 AT ROW 1 COL 1
     SPACE(0.19) SKIP(2.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Update Order Date(s) for Release#"
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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN oe-relh.rel-date IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asi.oe-relh"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update Order Date(s) for Release# */
DO:
  APPLY "choose":U TO Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF BUFFER bf-ordl FOR oe-ordl.
  SESSION:SET-WAIT-STATE("general").

  FOR EACH oe-rell
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
      USE-INDEX r-no NO-LOCK,
      FIRST oe-ord OF oe-rell:

    IF tb_hdr-due THEN oe-ord.due-date  = oe-relh.rel-date.

    IF tb_hdr-shp THEN oe-ord.last-date = oe-relh.rel-date.

    FOR EACH oe-ordl OF oe-ord
        WHERE oe-ordl.i-no EQ oe-rell.i-no
          AND oe-ordl.line EQ oe-rell.line:

      IF tb_lin-due THEN do:
         oe-ordl.req-date  = oe-relh.rel-date.
         FIND FIRST fg-set WHERE  fg-set.company = oe-relh.company
                             AND fg-set.part-no = oe-ordl.i-no NO-LOCK NO-ERROR.
         IF AVAIL fg-set THEN DO:
            FIND FIRST bf-ordl OF oe-ord WHERE bf-ordl.i-no = fg-set.set-no NO-ERROR.
            IF AVAIL bf-ordl THEN bf-ordl.req-date = oe-relh.rel-date.
         END.
      END.
      IF tb_lin-shp THEN do:
         oe-ordl.prom-date = oe-relh.rel-date.
         FIND FIRST fg-set WHERE  fg-set.company = oe-relh.company
                             AND fg-set.part-no = oe-ordl.i-no NO-LOCK NO-ERROR.
         IF AVAIL fg-set THEN DO:
            FIND FIRST bf-ordl OF oe-ord WHERE bf-ordl.i-no = fg-set.set-no NO-ERROR.
            IF AVAIL bf-ordl THEN bf-ordl.prom-date = oe-relh.rel-date.
         END.
      END.
    END.
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_hdr-due
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_hdr-due Dialog-Frame
ON VALUE-CHANGED OF tb_hdr-due IN FRAME Dialog-Frame /* Header Due Date? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_hdr-shp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_hdr-shp Dialog-Frame
ON VALUE-CHANGED OF tb_hdr-shp IN FRAME Dialog-Frame /* Header Last Ship Date? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_lin-due
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_lin-due Dialog-Frame
ON VALUE-CHANGED OF tb_lin-due IN FRAME Dialog-Frame /* Line Item Due Date? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_lin-shp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_lin-shp Dialog-Frame
ON VALUE-CHANGED OF tb_lin-shp IN FRAME Dialog-Frame /* Line Item Last Ship Date? */
DO:
  ASSIGN {&self-name}.
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

  DO TRANSACTION:
      {sys/inc/oereleasepop.i}
  END.

  ASSIGN tb_hdr-due = oereleasepop-log 
         tb_hdr-shp = oereleasepop-log 
         tb_lin-due = oereleasepop-log 
         tb_lin-shp = oereleasepop-log .

  FIND oe-relh WHERE ROWID(oe-relh) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL oe-relh THEN DO:
    FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) + " " +
                                TRIM(STRING(oe-relh.release#,">>>>>>>>>>")).

    RUN enable_UI.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
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
  DISPLAY tb_hdr-due tb_hdr-shp tb_lin-due tb_lin-shp 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE oe-relh THEN 
    DISPLAY oe-relh.rel-date 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-40 tb_hdr-due tb_hdr-shp tb_lin-due tb_lin-shp Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

