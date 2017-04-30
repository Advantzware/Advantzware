&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
{est/d-sidsid.i}

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-m-code LIKE mach.m-code NO-UNDO.
DEF INPUT PARAM ip-header LIKE mach.m-code NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-ss.

/* Local Variable Definitions ---                                       */
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-max-ss LIKE mach.max-pan-ss DECIMALS 10 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS val-01 val-02 val-03 val-04 val-05 val-06 ~
val-07 val-08 val-09 val-10 val-11 val-12 val-13 val-14 val-15 val-16 ~
val-17 val-18 val-19 val-20 btn_done btn_reset RECT-16 
&Scoped-Define DISPLAYED-OBJECTS val-01 val-02 val-03 val-04 val-05 val-06 ~
val-07 val-08 val-09 val-10 val-11 val-12 val-13 val-14 val-15 val-16 ~
val-17 val-18 val-19 val-20 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_done 
     LABEL "&Done" 
     SIZE 15 BY 1.19.

DEFINE BUTTON btn_reset AUTO-END-KEY 
     LABEL "&Reset" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE val-01 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-02 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-03 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-04 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-05 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-06 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-07 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-08 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-09 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-10 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-11 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-12 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-13 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-14 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-15 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-16 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-17 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-18 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-19 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE val-20 AS DECIMAL FORMAT ">>9.99999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 19.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     val-01 AT ROW 1.24 COL 3 COLON-ALIGNED NO-LABEL
     val-02 AT ROW 2.19 COL 3 COLON-ALIGNED NO-LABEL
     val-03 AT ROW 3.14 COL 3 COLON-ALIGNED NO-LABEL
     val-04 AT ROW 4.1 COL 3 COLON-ALIGNED NO-LABEL
     val-05 AT ROW 5.05 COL 3 COLON-ALIGNED NO-LABEL
     val-06 AT ROW 6 COL 3 COLON-ALIGNED NO-LABEL
     val-07 AT ROW 6.95 COL 3 COLON-ALIGNED NO-LABEL
     val-08 AT ROW 7.91 COL 3 COLON-ALIGNED NO-LABEL
     val-09 AT ROW 8.86 COL 3 COLON-ALIGNED NO-LABEL
     val-10 AT ROW 9.81 COL 3 COLON-ALIGNED NO-LABEL
     val-11 AT ROW 10.76 COL 3 COLON-ALIGNED NO-LABEL
     val-12 AT ROW 11.71 COL 3 COLON-ALIGNED NO-LABEL
     val-13 AT ROW 12.67 COL 3 COLON-ALIGNED NO-LABEL
     val-14 AT ROW 13.62 COL 3 COLON-ALIGNED NO-LABEL
     val-15 AT ROW 14.57 COL 3 COLON-ALIGNED NO-LABEL
     val-16 AT ROW 15.52 COL 3 COLON-ALIGNED NO-LABEL
     val-17 AT ROW 16.48 COL 3 COLON-ALIGNED NO-LABEL
     val-18 AT ROW 17.43 COL 3 COLON-ALIGNED NO-LABEL
     val-19 AT ROW 18.38 COL 3 COLON-ALIGNED NO-LABEL
     val-20 AT ROW 19.33 COL 3 COLON-ALIGNED NO-LABEL
     btn_done AT ROW 7.67 COL 37
     btn_reset AT ROW 11.48 COL 37
     RECT-16 AT ROW 1 COL 1.8
     SPACE(28.79) SKIP(0.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "for Machine:"
         CANCEL-BUTTON btn_reset.


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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* for Machine: */
DO: 
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_done Dialog-Frame
ON CHOOSE OF btn_done IN FRAME Dialog-Frame /* Done */
DO:
  RUN screen-to-var.

  FOR EACH tt-ss:
    DELETE tt-ss.
  END.

  DO li = 1 TO 20:
    CREATE tt-ss.
    tt-val = lv-max-ss[li].
  END.

  RUN sort-tt-2.

  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_reset Dialog-Frame
ON CHOOSE OF btn_reset IN FRAME Dialog-Frame /* Reset */
DO:
  RUN init-var.
  RUN var-to-screen.
  RETURN NO-APPLY.
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
    
  FRAME {&FRAME-NAME}:TITLE = TRIM(ip-header) + " " +
                              TRIM(FRAME {&FRAME-NAME}:TITLE) + " " +
                              TRIM(ip-m-code).

  RUN sort-tt.

  RUN init-var.

  RUN enable_UI.

  RUN var-to-screen.

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
  DISPLAY val-01 val-02 val-03 val-04 val-05 val-06 val-07 val-08 val-09 val-10 
          val-11 val-12 val-13 val-14 val-15 val-16 val-17 val-18 val-19 val-20 
      WITH FRAME Dialog-Frame.
  ENABLE val-01 val-02 val-03 val-04 val-05 val-06 val-07 val-08 val-09 val-10 
         val-11 val-12 val-13 val-14 val-15 val-16 val-17 val-18 val-19 val-20 
         btn_done btn_reset RECT-16 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-var Dialog-Frame 
PROCEDURE init-var :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
   li        = 0
   lv-max-ss = 0.

  FOR EACH tt-ss BY tt-seq:
    li = li + 1.
    IF li LE EXTENT(lv-max-ss) THEN lv-max-ss[li] = tt-val.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE screen-to-var Dialog-Frame 
PROCEDURE screen-to-var :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-max-ss[01] = DEC(val-01:SCREEN-VALUE)
     lv-max-ss[02] = DEC(val-02:SCREEN-VALUE)
     lv-max-ss[03] = DEC(val-03:SCREEN-VALUE)
     lv-max-ss[04] = DEC(val-04:SCREEN-VALUE)
     lv-max-ss[05] = DEC(val-05:SCREEN-VALUE)
     lv-max-ss[06] = DEC(val-06:SCREEN-VALUE)
     lv-max-ss[07] = DEC(val-07:SCREEN-VALUE)
     lv-max-ss[08] = DEC(val-08:SCREEN-VALUE)
     lv-max-ss[09] = DEC(val-09:SCREEN-VALUE)
     lv-max-ss[10] = DEC(val-10:SCREEN-VALUE)
     lv-max-ss[11] = DEC(val-11:SCREEN-VALUE)
     lv-max-ss[12] = DEC(val-12:SCREEN-VALUE)
     lv-max-ss[13] = DEC(val-13:SCREEN-VALUE)
     lv-max-ss[14] = DEC(val-14:SCREEN-VALUE)
     lv-max-ss[15] = DEC(val-15:SCREEN-VALUE)
     lv-max-ss[16] = DEC(val-16:SCREEN-VALUE)
     lv-max-ss[17] = DEC(val-17:SCREEN-VALUE)
     lv-max-ss[18] = DEC(val-18:SCREEN-VALUE)
     lv-max-ss[19] = DEC(val-19:SCREEN-VALUE)
     lv-max-ss[20] = DEC(val-20:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sort-tt Dialog-Frame 
PROCEDURE sort-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     
  li = 0.

  FOR EACH tt-ss BREAK BY tt-val DESC:
    IF FIRST-OF(tt-val) THEN
      ASSIGN
       li     = li + 1
       tt-seq = li.
    ELSE DELETE tt-ss.
  END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sort-tt-2 Dialog-Frame 
PROCEDURE sort-tt-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   li = 0.

  FOR EACH tt-ss BREAK BY tt-val DESC:
    IF FIRST-OF(tt-val) THEN
      ASSIGN
       li     = li + 1
       tt-seq = li.
    ELSE
       IF tt-val NE 0 THEN
          DELETE tt-ss.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE var-to-screen Dialog-Frame 
PROCEDURE var-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     val-01:SCREEN-VALUE = STRING(lv-max-ss[01])
     val-02:SCREEN-VALUE = STRING(lv-max-ss[02])
     val-03:SCREEN-VALUE = STRING(lv-max-ss[03])
     val-04:SCREEN-VALUE = STRING(lv-max-ss[04])
     val-05:SCREEN-VALUE = STRING(lv-max-ss[05])
     val-06:SCREEN-VALUE = STRING(lv-max-ss[06])
     val-07:SCREEN-VALUE = STRING(lv-max-ss[07])
     val-08:SCREEN-VALUE = STRING(lv-max-ss[08])
     val-09:SCREEN-VALUE = STRING(lv-max-ss[09])
     val-10:SCREEN-VALUE = STRING(lv-max-ss[10])
     val-11:SCREEN-VALUE = STRING(lv-max-ss[11])
     val-12:SCREEN-VALUE = STRING(lv-max-ss[12])
     val-13:SCREEN-VALUE = STRING(lv-max-ss[13])
     val-14:SCREEN-VALUE = STRING(lv-max-ss[14])
     val-15:SCREEN-VALUE = STRING(lv-max-ss[15])
     val-16:SCREEN-VALUE = STRING(lv-max-ss[16])
     val-17:SCREEN-VALUE = STRING(lv-max-ss[17])
     val-18:SCREEN-VALUE = STRING(lv-max-ss[18])
     val-19:SCREEN-VALUE = STRING(lv-max-ss[19])
     val-20:SCREEN-VALUE = STRING(lv-max-ss[20]).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

