&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

  Created:  for  task 10301314  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ip-date AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-procat AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-i-no   AS CHAR FORMAT "x(15)" NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
 
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEFINE STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 begin_date end_date begin_cust-no ~
end_cust-no begin_type end_type begin_item-cat end_item-cat begin_item ~
end_item btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_cust-no ~
end_cust-no begin_type end_type begin_item-cat end_item-cat begin_item ~
end_item 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Effective Date#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_item AS CHARACTER FORMAT "X(15)" 
     LABEL "From FG Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_item-cat AS CHARACTER FORMAT "X(5)" 
     LABEL "From FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_type AS CHARACTER FORMAT "X(8)" 
     LABEL "From Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/12/13 
     LABEL "To Effective Date#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_item AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To FG Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_item-cat AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "To FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_type AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_date AT ROW 2.43 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 114
     end_date AT ROW 2.52 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 116
     begin_cust-no AT ROW 3.57 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 6
     end_cust-no AT ROW 3.57 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 16
     begin_type AT ROW 4.67 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning Type Number" WIDGET-ID 104
     end_type AT ROW 4.67 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending Type Number" WIDGET-ID 106
     begin_item-cat AT ROW 5.71 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning FG Product Category" WIDGET-ID 108
     end_item-cat AT ROW 5.71 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending FG Product Category" WIDGET-ID 110
     begin_item AT ROW 6.81 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning FG Item Number" WIDGET-ID 100
     end_item AT ROW 6.81 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending FG Item Number" WIDGET-ID 102
     btn-ok AT ROW 9.95 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 9.95 COL 60.2 WIDGET-ID 12
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     RECT-7 AT ROW 1 COL 2 WIDGET-ID 38
     SPACE(0.79) SKIP(3.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Price Matrix Delete Multiple Item " WIDGET-ID 100.


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

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_item:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_item-cat:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_type:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_item:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_item-cat:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_type:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Price Matrix Delete Multiple Item  */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :

       when "begin_cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* cust-no*/  
       when "end_cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* cust-no*/  
     
       when "begin_item" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfg.w (cocode, begin_cust-no, begin_item, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* item-no*/  
       when "end_item" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfg.w (cocode, begin_cust-no, end_item, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* item-no*/  
       when "begin_item-cat" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-fgcat.w (cocode, begin_item-cat, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* item-cat*/  
       when "end_item-cat" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-fgcat.w (cocode, begin_item-cat, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* item-no*/  
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Price Matrix Delete Multiple Item  */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no Dialog-Frame
ON LEAVE OF begin_cust-no IN FRAME Dialog-Frame /* From Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date Dialog-Frame
ON LEAVE OF begin_date IN FRAME Dialog-Frame /* From Effective Date# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item Dialog-Frame
ON LEAVE OF begin_item IN FRAME Dialog-Frame /* From FG Item */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_item-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item-cat Dialog-Frame
ON LEAVE OF begin_item-cat IN FRAME Dialog-Frame /* From FG Category */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_type Dialog-Frame
ON LEAVE OF begin_type IN FRAME Dialog-Frame /* From Cust Type */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok Dialog-Frame
ON CHOOSE OF btn-ok IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  message "Are you sure you want to delete these records?" 
             view-as alert-box ERROR BUTTON YES-NO UPDATE v-ans AS LOG.
     IF v-ans THEN DO:
         FOR EACH oe-prmtx WHERE oe-prmtx.company = cocode 
             AND oe-prmtx.cust-no GE begin_cust-no
             AND oe-prmtx.cust-no LE END_cust-no 
             AND oe-prmtx.custype GE begin_type
             AND oe-prmtx.custype LE END_type 
             AND oe-prmtx.procat GE begin_item-cat
             AND oe-prmtx.procat LE END_item-cat 
             AND oe-prmtx.i-no GE begin_item
             AND SUBSTRING(oe-prmtx.i-no,01,15) LE END_item  
             AND oe-prmtx.eff-date GE begin_date
             AND oe-prmtx.eff-date LE end_date
             EXCLUSIVE-LOCK:
             
             FIND FIRST reftable
                 WHERE reftable.rec_key  EQ oe-prmtx.rec_key
                 AND reftable.company  EQ "oe-prmtx"
/*                  AND date(reftable.CODE) GE begin_date */
/*                  AND date(reftable.CODE) LE END_date   */
                 USE-INDEX rec_key NO-ERROR.
             IF AVAIL reftable THEN do:
                 DELETE reftable.
             END.
             DELETE oe-prmtx .
         END.
         
         apply "window-close" to frame {&frame-name}. 
     END.
     

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no Dialog-Frame
ON LEAVE OF end_cust-no IN FRAME Dialog-Frame /* To Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date Dialog-Frame
ON LEAVE OF end_date IN FRAME Dialog-Frame /* To Effective Date# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item Dialog-Frame
ON LEAVE OF end_item IN FRAME Dialog-Frame /* To FG Item */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_item-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item-cat Dialog-Frame
ON LEAVE OF end_item-cat IN FRAME Dialog-Frame /* To FG Category */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_type Dialog-Frame
ON LEAVE OF end_type IN FRAME Dialog-Frame /* To Cust Type */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    /*{custom/usrprint.i}*/
    IF ip-date NE ? THEN ASSIGN  
        begin_date:SCREEN-VALUE     = string(ip-date)                              
        end_date:SCREEN-VALUE      = STRING(ip-date) .
    IF ip-cust-no NE "" THEN ASSIGN
        begin_cust-no:SCREEN-VALUE  = ip-cust-no                             
        end_cust-no:SCREEN-VALUE    = ip-cust-no .
    IF ip-type NE "" THEN ASSIGN
        begin_type:SCREEN-VALUE     = ip-type                           
        end_type:SCREEN-VALUE      = ip-type.
    IF ip-procat NE "" THEN ASSIGN
        begin_item-cat:SCREEN-VALUE = ip-procat
        end_item-cat:SCREEN-VALUE   = ip-procat.
    IF ip-i-no NE "" THEN ASSIGN
        begin_item:SCREEN-VALUE     = ip-i-no 
        end_item:SCREEN-VALUE       = ip-i-no .

    APPLY "entry" TO begin_date.
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
  DISPLAY begin_date end_date begin_cust-no end_cust-no begin_type end_type 
          begin_item-cat end_item-cat begin_item end_item 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-7 begin_date end_date begin_cust-no end_cust-no begin_type 
         end_type begin_item-cat end_item-cat begin_item end_item btn-ok 
         btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

