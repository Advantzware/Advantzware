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

/* Local Variable Definitions ---                                       */

DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
DEF INPUT PARAM ip-est-type AS INT NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER bf-set FOR eb.
DEF VAR lv-set-recid AS RECID NO-UNDO.
DEF VAR lv-new-set AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES eb

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame eb.stock-no eb.part-no ~
eb.part-dscr1 eb.part-dscr2 eb.len eb.wid eb.dep 
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH eb SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame eb
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame eb


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_update Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS eb.stock-no eb.part-no eb.part-dscr1 ~
eb.part-dscr2 eb.len eb.wid eb.dep 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 eb.part-no eb.part-dscr1 eb.part-dscr2 eb.len ~
eb.wid eb.dep 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Close" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btn_update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      eb SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     eb.stock-no AT ROW 1.71 COL 23 COLON-ALIGNED
          LABEL "Set FG Item#"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.part-no AT ROW 2.67 COL 23 COLON-ALIGNED
          LABEL "Set Cust Part #"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     eb.part-dscr1 AT ROW 3.62 COL 23 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     eb.part-dscr2 AT ROW 4.57 COL 23 COLON-ALIGNED
          LABEL "Part Desc"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     eb.len AT ROW 6 COL 23 COLON-ALIGNED
          LABEL "F.G. Length" FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     eb.wid AT ROW 6.95 COL 23 COLON-ALIGNED FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     eb.dep AT ROW 7.91 COL 23 COLON-ALIGNED
          LABEL "Depth" FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     btn_update AT ROW 12.43 COL 17
     Btn_Cancel AT ROW 12.43 COL 48
     SPACE(12.59) SKIP(0.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "SET Information"
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

/* SETTINGS FOR FILL-IN eb.part-dscr1 IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.part-dscr2 IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.part-no IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN eb.stock-no IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN eb.dep IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.len IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN eb.wid IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-FORMAT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.eb"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* SET Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Close */
DO:
   RUN validate-set.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_update Dialog-Frame
ON CHOOSE OF btn_update IN FRAME Dialog-Frame /* Update */
DO:
   

    IF SELF:LABEL = "&Update" THEN DO:
       SELF:LABEL = "&Save".
       btn_cancel:LABEL = "&Cancel".

       ENABLE  /*{&DISPLAYED-FIELDS} */ {&list-1} WITH FRAME {&FRAME-NAME}.
       APPLY "entry" TO eb.part-no IN FRAME {&FRAME-NAME}.
    END.
    ELSE DO WITH FRAME {&FRAME-NAME}:
         FIND CURRENT eb EXCLUSIVE-LOCK.
         ASSIGN {&DISPLAYED-FIELDS} .
         RUN validate-set.
         APPLY "go" TO FRAME {&frame-name} .

    END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   FIND bf-eb WHERE RECID(bf-eb) = ip-recid NO-LOCK NO-ERROR.
   FIND FIRST bf-set WHERE bf-set.company = bf-eb.company
                       AND bf-set.est-no = bf-eb.est-no
                       AND bf-set.form-no = 0
                       NO-LOCK NO-ERROR.
   IF NOT AVAIL bf-set THEN RUN CREATE-set.
   lv-set-recid = RECID(bf-set).
   FIND eb WHERE RECID(eb) = lv-set-recid NO-LOCK.

   RUN enable_UI.
   IF lv-new-set THEN RUN enable-all.

   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-set Dialog-Frame 
PROCEDURE create-set :
/*------------------------------------------------------------------------------
  Purpose:     /* ce/set-info.a */
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CREATE bf-set.
  ASSIGN bf-set.company = bf-eb.company
         bf-set.loc = bf-eb.loc
         bf-set.est-type = ip-est-type
         bf-set.est-no = bf-eb.est-no
         bf-set.FORM-no = 0
         bf-set.blank-no = 0
         bf-set.est-int = INT(bf-eb.est-no).

  lv-new-set = yes.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-all Dialog-Frame 
PROCEDURE enable-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  btn_update:LABEL IN FRAME {&FRAME-NAME}  = "&Save".
  ENABLE  {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.
  APPLY "entry" TO eb.stock-no IN FRAME {&FRAME-NAME}.

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
  IF AVAILABLE eb THEN 
    DISPLAY eb.stock-no eb.part-no eb.part-dscr1 eb.part-dscr2 eb.len eb.wid 
          eb.dep 
      WITH FRAME Dialog-Frame.
  ENABLE btn_update Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-set Dialog-Frame 
PROCEDURE validate-set :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-eb2 FOR eb.
  DEF BUFFER bf-est FOR est.

  FIND FIRST bf-est WHERE bf-est.company = bf-eb.company
                            AND bf-est.est-no = bf-eb.est-no NO-ERROR.

  FIND FIRST bf-eb2 WHERE bf-eb2.company = bf-eb.company
                      AND bf-eb2.est-no = bf-eb.est-no
                      AND bf-eb2.form-no = 0
                      NO-ERROR.
  IF AVAIL bf-eb2 THEN DO:
     IF bf-eb2.part-no = "" THEN DELETE bf-eb2.
     ELSE IF AVAIL bf-est THEN bf-est.est-type = 2.
  END.
  IF NOT AVAIL bf-eb2 THEN bf-est.est-type = 4.
  
  FOR EACH bf-eb2 WHERE bf-eb2.company = bf-est.company
                    AND bf-eb2.est-no = bf-est.est-no:
      bf-eb2.est-type = bf-est.est-type.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

