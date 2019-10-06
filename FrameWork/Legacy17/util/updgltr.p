&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR lv-trans-type AS cha NO-UNDO .   /*hist or trans */
DEF VAR li-num-trans AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-comp-no lv-actnum lv-cust-no lv-tr-no ~
lv-tr-date btn-get lv-tramt Btn_OK Btn_Cancel RECT-32 RECT-33 
&Scoped-Define DISPLAYED-OBJECTS lv-comp-no lv-actnum lv-cust-no lv-tr-no ~
lv-tr-date lv-tr-dscr lv-tramt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-get 
     LABEL "&Get Info" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-next 
     LABEL "Get Next" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE lv-actnum AS CHARACTER FORMAT "X(25)":U 
     LABEL "Account#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE lv-comp-no AS CHARACTER FORMAT "X(256)":U INITIAL "001" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cust-no AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-tr-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Trans. Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-tr-dscr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Trans. Description" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE lv-tr-no AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Run#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-tramt AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Transaction Amount" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 8.57.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 3.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-comp-no AT ROW 1.71 COL 21 COLON-ALIGNED
     lv-actnum AT ROW 3.14 COL 21 COLON-ALIGNED
     lv-cust-no AT ROW 4.33 COL 21 COLON-ALIGNED
     lv-tr-no AT ROW 5.52 COL 21 COLON-ALIGNED
     lv-tr-date AT ROW 6.95 COL 21 COLON-ALIGNED
     btn-get AT ROW 8.14 COL 42
     btn-next AT ROW 8.14 COL 59
     lv-tr-dscr AT ROW 11 COL 22 COLON-ALIGNED
     lv-tramt AT ROW 12.43 COL 24 COLON-ALIGNED
     Btn_OK AT ROW 16.71 COL 18
     Btn_Cancel AT ROW 16.71 COL 52
     RECT-32 AT ROW 1.24 COL 3
     RECT-33 AT ROW 10.05 COL 3
     SPACE(11.79) SKIP(4.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update GL Transaction"
         CANCEL-BUTTON Btn_Cancel.


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

/* SETTINGS FOR BUTTON btn-next IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-tr-dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update GL Transaction */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-get
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-get Dialog-Frame
ON CHOOSE OF btn-get IN FRAME Dialog-Frame /* Get Info */
DO:
  

    ASSIGN lv-trans-type = ""
           lv-tramt = 0.


    ASSIGN lv-comp-no lv-cust-no lv-tr-no lv-tr-date lv-actnum.

    li-num-trans = 0.
    FOR EACH gltrans WHERE gltrans.company = lv-comp-no
                          AND gltrans.trnum = lv-tr-no
                          AND gltrans.tr-date = lv-tr-date
                          AND gltrans.actnum = lv-actnum
                          AND (gltrans.tr-dscr BEGINS lv-cust-no OR lv-cust-no = "")
                          NO-LOCK.
        li-num-trans = li-num-trans + 1.
    END.


    FIND FIRST gltrans WHERE gltrans.company = lv-comp-no
                          AND gltrans.trnum = lv-tr-no
                          AND gltrans.tr-date = lv-tr-date
                          AND gltrans.actnum = lv-actnum
                          AND (gltrans.tr-dscr BEGINS lv-cust-no OR lv-cust-no = "")
                          NO-LOCK NO-ERROR.
    IF NOT AVAIL gltrans THEN DO:
       FIND FIRST glhist WHERE glhist.company = lv-comp-no
                          AND glhist.tr-num = lv-tr-no
                          AND glhist.tr-date = lv-tr-date
                          AND glhist.actnum = lv-actnum
                          AND (glhist.tr-dscr BEGINS lv-cust-no OR lv-cust-no = "")
                          NO-LOCK NO-ERROR.
       IF NOT AVAIL glhist THEN DO:
           MESSAGE "No GL Trans found. Try others. " VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
       END.
       IF AVAIL glhist THEN ASSIGN lv-tramt = glhist.tr-amt
                                   lv-trans-type = "Hist"
                                   lv-tr-dscr = glhist.tr-dscr.
    END.

    ELSE ASSIGN lv-tramt = gltrans.tr-amt
                lv-tr-dscr = gltrans.tr-dscr
                lv-trans-type = "Trans".

    DISP lv-tramt lv-tr-dscr WITH FRAME {&FRAME-NAME}.
    IF li-num-trans > 1 THEN btn-next:SENSITIVE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-next Dialog-Frame
ON CHOOSE OF btn-next IN FRAME Dialog-Frame /* Get Next */
DO:
   /*  IF lv-trans-type = "Trans" THEN DO:
          FIND CURRENT gltrans NO-LOCK NO-ERROR.          
       END.
       ELSE IF lv-trans-type = "Hist" THEN DO:
          FIND CURRENT glhist NO-LOCK NO-ERROR.          
     END.
     */
     FIND NEXT gltrans WHERE gltrans.company = lv-comp-no
                          AND gltrans.trnum = lv-tr-no
                          AND gltrans.tr-date = lv-tr-date
                          AND gltrans.actnum = lv-actnum
                          AND (gltrans.tr-dscr BEGINS lv-cust-no OR lv-cust-no = "")
                          NO-LOCK NO-ERROR.
    IF NOT AVAIL gltrans THEN DO:
       FIND next glhist WHERE glhist.company = lv-comp-no
                          AND glhist.tr-num = lv-tr-no
                          AND glhist.tr-date = lv-tr-date
                          AND glhist.actnum = lv-actnum
                          AND (glhist.tr-dscr BEGINS lv-cust-no OR lv-cust-no = "")
                          NO-LOCK NO-ERROR.
       IF NOT AVAIL glhist THEN DO:
           btn-next:SENSITIVE = NO.
           MESSAGE "No More GL Trans found.... " VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
       END.
       IF AVAIL glhist THEN ASSIGN lv-tramt = glhist.tr-amt
                                   lv-tr-dscr = glhist.tr-dscr
                                   lv-trans-type = "Hist".
    END.

    ELSE ASSIGN lv-tramt = gltrans.tr-amt
                lv-tr-dscr = gltrans.tr-dscr
                lv-trans-type = "Trans".

    DISP lv-tramt lv-tr-dscr WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    MESSAGE "Are you sure you want to update ? " VIEW-AS ALERT-BOX WARNING BUTTON YES-NO
        UPDATE ll-ans AS LOG.
    IF ll-ans THEN DO:
       ASSIGN lv-tramt.

       IF lv-trans-type = "Trans" THEN DO:
          FIND CURRENT gltrans EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL gltrans THEN gltrans.tr-amt = lv-tramt.
       END.
       ELSE IF lv-trans-type = "Hist" THEN DO:
          FIND CURRENT glhist EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL glhist THEN glhist.tr-amt = lv-tramt.
       END.

    END.    
    IF li-num-trans <= 1 THEN  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = yes.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


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
  DISPLAY lv-comp-no lv-actnum lv-cust-no lv-tr-no lv-tr-date lv-tr-dscr 
          lv-tramt 
      WITH FRAME Dialog-Frame.
  ENABLE lv-comp-no lv-actnum lv-cust-no lv-tr-no lv-tr-date btn-get lv-tramt 
         Btn_OK Btn_Cancel RECT-32 RECT-33 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

