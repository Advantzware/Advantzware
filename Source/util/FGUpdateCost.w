&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File:   util/FGUpdateCost.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:  Sewa Singh

  Created: Fri 4 Oct 2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW SHARED VARIABLE uom-list AS CHARACTER INIT "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.
DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
 RUN util/Validate.p PERSISTENT SET hdValidator.
     THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdValidator).
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 RECT-23 lv-comp lv-cust-no cFGItem ~
std-Mat-Cost std-Lab-Cost std-Var-Cost std-Fix-Cost std-tot-Cost avg-Cost ~
last-Cost prod-uom Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-comp lv-cust-no lv-name cFGItem ~
std-Mat-Cost std-Lab-Cost std-Var-Cost std-Fix-Cost std-tot-Cost avg-Cost ~
last-Cost prod-uom 

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

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE avg-Cost AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Average Cost" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE cFGItem AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE last-Cost AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Last Cost" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-comp AS CHARACTER FORMAT "X(3)":U INITIAL "001" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE lv-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE prod-uom AS CHARACTER FORMAT "X(3)":U 
     LABEL "Prod Uom" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE std-Fix-Cost AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Std Fix OH Cost" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE std-Lab-Cost AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Std Labor Cost" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE std-Mat-Cost AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Std Mat'l Cost" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE std-tot-Cost AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Total Std Cost" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE std-Var-Cost AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Std Var OH Cost" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 4.05.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 11.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-comp AT ROW 1.48 COL 19 COLON-ALIGNED
     lv-cust-no AT ROW 2.67 COL 19 COLON-ALIGNED
     lv-name AT ROW 2.67 COL 41 COLON-ALIGNED NO-LABEL
     cFGItem AT ROW 3.86 COL 19 COLON-ALIGNED
     std-Mat-Cost AT ROW 5.52 COL 19 COLON-ALIGNED
     std-Lab-Cost AT ROW 6.71 COL 19 COLON-ALIGNED
     std-Var-Cost AT ROW 7.91 COL 19 COLON-ALIGNED
     std-Fix-Cost AT ROW 9.1 COL 19 COLON-ALIGNED
     std-tot-Cost AT ROW 10.29 COL 19 COLON-ALIGNED
     avg-Cost AT ROW 11.48 COL 19 COLON-ALIGNED
     last-Cost AT ROW 12.67 COL 19 COLON-ALIGNED
     prod-uom AT ROW 14.57 COL 19 COLON-ALIGNED
     Btn_OK AT ROW 17.91 COL 21
     Btn_Cancel AT ROW 17.91 COL 58
     RECT-22 AT ROW 1 COL 1
     RECT-23 AT ROW 5.29 COL 2
     SPACE(6.00) SKIP(2.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update FG Item Cost"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update FG Item */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME avg-Cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL avg-Cost Dialog-Frame
ON LEAVE OF avg-Cost IN FRAME Dialog-Frame /* avg-Cost */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .

    RUN valid-uom( OUTPUT lCheckError) NO-ERROR.
      IF lCheckError THEN RETURN NO-APPLY.

    MESSAGE "Are you sure you want to update? " 
        VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
    IF ll-ans THEN DO:

       FIND FIRST itemfg EXCLUSIVE-LOCK
           WHERE itemfg.company = lv-comp 
             AND itemfg.cust-no = lv-cust-no
             AND itemfg.i-no = cFGItem NO-ERROR.

       IF NOT AVAIL itemfg THEN DO:
          MESSAGE "There is no FG Item exist."
                  VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO lv-cust-no.
          RETURN NO-APPLY.
        END.

       ASSIGN itemfg.std-mat-cost = std-Mat-Cost
              itemfg.std-lab-cost = std-Lab-Cost
              itemfg.std-var-cost = std-Var-Cost
              itemfg.std-fix-cost = std-Fix-Cost
              itemfg.total-std-cost = std-tot-Cost
              itemfg.avg-cost = avg-Cost
              itemfg.last-cost = last-Cost
              itemfg.prod-uom = prod-uom.
       FIND CURRENT itemfg NO-LOCK NO-ERROR .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFGItem Dialog-Frame
ON LEAVE OF cFGItem IN FRAME Dialog-Frame /* FG Item# */
DO:
   IF LASTKEY = -1 THEN RETURN.
   ASSIGN {&self-name}.

   FIND FIRST itemfg WHERE itemfg.company = lv-comp 
                       AND itemfg.cust-no = lv-cust-no
                       AND itemfg.i-no = cFGItem
                       NO-LOCK NO-ERROR.
   IF NOT AVAIL itemfg THEN DO:
       IF lv-cust-no NE ""  THEN do:
           MESSAGE "Invalid FG Item for the customer ."
               VIEW-AS ALERT-BOX INFO.
           APPLY "entry" TO lv-cust-no.
       END.
      ELSE do:
          MESSAGE "Invalid FG Item , Try Help. ."
               VIEW-AS ALERT-BOX INFO.
          APPLY "entry" TO cFGItem.
      END.
      RETURN NO-APPLY.
   END.

   ASSIGN 
         std-Mat-Cost  =     itemfg.std-mat-cost    
         std-Lab-Cost  =     itemfg.std-lab-cost    
         std-Var-Cost  =     itemfg.std-var-cost    
         std-Fix-Cost  =     itemfg.std-fix-cost    
         std-tot-Cost  =     itemfg.total-std-cost  
         avg-Cost      =     itemfg.avg-cost        
         last-Cost     =     itemfg.last-cost       
         prod-uom      =     itemfg.prod-uom      .  

   DISPLAY std-Mat-Cost
           std-Lab-Cost
           std-Var-Cost std-Fix-Cost std-tot-Cost avg-Cost last-Cost prod-uom
           WITH FRAME {&FRAME-NAME}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME cFGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFGItem Dialog-Frame
ON HELP OF cFGItem IN FRAME Dialog-Frame /* cust */
DO:
    DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.

    RUN system/openlookup.p (lv-comp, "itemfg", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
    IF cMainField <> "" THEN cFGItem:SCREEN-VALUE = cMainField.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME last-Cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL last-Cost Dialog-Frame
ON LEAVE OF last-Cost IN FRAME Dialog-Frame /* last-Cost */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-comp Dialog-Frame
ON LEAVE OF lv-comp IN FRAME Dialog-Frame /* Company */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cust-no Dialog-Frame
ON LEAVE OF lv-cust-no IN FRAME Dialog-Frame /* Customer# */
DO:
  ASSIGN {&self-name}.
 IF LASTKEY <> -1 THEN DO:
  FIND FIRST cust WHERE cust.company = lv-comp 
                       AND cust.cust-no = lv-cust-no
                       NO-LOCK NO-ERROR.
   IF NOT AVAIL cust AND lv-cust-no NE "" THEN DO:
      MESSAGE "There is no Customer exist."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO lv-cust-no.
      RETURN NO-APPLY.
   END.
   IF AVAIL cust THEN
       lv-name:SCREEN-VALUE = cust.NAME .
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME lv-cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cust-no Dialog-Frame
ON HELP OF lv-cust-no IN FRAME Dialog-Frame /* cust */
DO:
    DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.

    RUN system/openlookup.p (lv-comp, "cust-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
    IF cMainField <> "" THEN lv-cust-no:SCREEN-VALUE = cMainField.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod-uom Dialog-Frame
ON LEAVE OF prod-uom IN FRAME Dialog-Frame /* Prod Uom */
DO:
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
    ASSIGN {&self-name}.
  IF LASTKEY <> -1 THEN DO:
      RUN valid-uom( OUTPUT lCheckError) NO-ERROR.
      IF lCheckError THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME prod-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod-uom Dialog-Frame
ON HELP OF prod-uom IN FRAME Dialog-Frame /* prod-uom */
DO:
    DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

    RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).
    RUN windows/l-stduom.w (lv-comp, uom-list, FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val NE "" THEN 
            prod-uom:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME std-Fix-Cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL std-Fix-Cost Dialog-Frame
ON LEAVE OF std-Fix-Cost IN FRAME Dialog-Frame /* std-Fix-Cost */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME std-Lab-Cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL std-Lab-Cost Dialog-Frame
ON LEAVE OF std-Lab-Cost IN FRAME Dialog-Frame /* std-Lab-Cost */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME std-Mat-Cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL std-Mat-Cost Dialog-Frame
ON LEAVE OF std-Mat-Cost IN FRAME Dialog-Frame /* std-Mat-Cost */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME std-tot-Cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL std-tot-Cost Dialog-Frame
ON LEAVE OF std-tot-Cost IN FRAME Dialog-Frame /* std-tot-Cost */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME std-Var-Cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL std-Var-Cost Dialog-Frame
ON LEAVE OF std-Var-Cost IN FRAME Dialog-Frame /* std-Var-Cost */
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
  DISPLAY lv-comp lv-cust-no lv-name cFGItem std-Mat-Cost std-Lab-Cost 
          std-Var-Cost std-Fix-Cost std-tot-Cost avg-Cost last-Cost prod-uom 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-22 RECT-23 lv-comp lv-cust-no cFGItem std-Mat-Cost std-Lab-Cost 
         std-Var-Cost std-Fix-Cost std-tot-Cost avg-Cost last-Cost prod-uom 
         Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom Dialog-Frame 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO .
  DEFINE VARIABLE cUom AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lValid AS LOGICAL NO-UNDO .
  DEFINE VARIABLE cValidMessage AS CHARACTER NO-UNDO .
  DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
  
{&methods/lValidateError.i YES}
  
  DO WITH FRAME {&FRAME-NAME}:
      RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).
      
      cUom = prod-uom:SCREEN-VALUE.

      RUN pIsValidUOM IN hdValidator (cUom, YES, OUTPUT lValid, OUTPUT cValidMessage).
      IF NOT lValid THEN DO:
          MESSAGE  cValidMessage
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          oplReturnError = YES .
          lCheckError = YES .
          APPLY "entry" TO prod-uom .
      END.

      RUN pIsValidFromList IN hdValidator ("Uom", cUom, uom-list, OUTPUT lValid, OUTPUT cValidMessage). 
      
      IF NOT lValid AND NOT lCheckError THEN DO:
          MESSAGE  cValidMessage
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          oplReturnError = YES .
          APPLY "entry" TO prod-uom  .
      END.
   END.
   
{&methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
