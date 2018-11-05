&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: addon/listobjs/contact_.w

  Description: from SMART.W - Template for basic SmartObject

  Author: 
  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndlset.i}
{custom/gcompany.i}
{custom/gloc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartListObject

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES contact
&Scoped-define FIRST-EXTERNAL-TABLE contact


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR contact.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-3 RECT-9 list-order ~
begin_cust-no end_cust-no begin_contact_sman end_contact_sman ~
begin_contact_zip end_contact_zip selected-printer tbLastOrder tbMailMrge 
&Scoped-Define DISPLAYED-OBJECTS list-order selected-company ~
begin_name_description begin_cust-no end_name_description end_cust-no ~
begin_contact_sman-dscr begin_contact_sman end_contact_sman-dscr ~
end_contact_sman begin_contact_zip end_contact_zip selected-printer ~
tbLastOrder tbMailMrge filename FILL-IN-Title F1 F-3 F-5 F-4 F-6 F-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define F1 F1 F-3 F-5 F-4 F-6 F-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE begin_contact_sman AS CHARACTER FORMAT "X(10)" 
     LABEL "Beginning Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE begin_contact_sman-dscr AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 31.6 BY 1
     BGCOLOR 7 .

DEFINE VARIABLE begin_contact_zip AS CHARACTER FORMAT "X(10)" 
     LABEL "Beginning Zip" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(10)" 
     LABEL "Beginning Customer Number" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE begin_name_description AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 31.6 BY 1
     BGCOLOR 7 .

DEFINE VARIABLE end_contact_sman AS CHARACTER FORMAT "X(10)" 
     LABEL "Ending Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_contact_sman-dscr AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 31.6 BY 1
     BGCOLOR 7 .

DEFINE VARIABLE end_contact_zip AS CHARACTER FORMAT "X(10)" 
     LABEL "Ending Zip" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(10)" 
     LABEL "Ending Customer Number" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_name_description AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 31.6 BY 1
     BGCOLOR 7 .

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-3 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-4 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-5 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-6 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE filename AS CHARACTER FORMAT "X(80)":U 
     LABEL "Filename" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 64 BY 1 TOOLTIP "Enter Mail Merge Filename" NO-UNDO.

DEFINE VARIABLE FILL-IN-Title AS CHARACTER FORMAT "X(80)":U 
     LABEL "Title for Mail Merge" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 64 BY 1 TOOLTIP "Title for Current Mailing" NO-UNDO.

DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(10)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE selected-printer AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Printer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 TOOLTIP "Enter Printer Number Only when Printing to Printer".

DEFINE VARIABLE list-order AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Default", 1,
"Description", 2
     SIZE 106 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 1.43.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 117 BY 2.86.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 119 BY 10.71.

DEFINE VARIABLE tbLastOrder AS LOGICAL INITIAL no 
     LABEL "Print Last Order Date" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 TOOLTIP "Print Last Order Date" NO-UNDO.

DEFINE VARIABLE tbMailMrge AS LOGICAL INITIAL no 
     LABEL "Create Mail Merge File" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 TOOLTIP "Check to Create Mail Merge" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     list-order AT ROW 1.24 COL 14 HELP
          "Select List Order" NO-LABEL
     selected-company AT ROW 2.91 COL 28 COLON-ALIGNED
     begin_name_description AT ROW 3.81 COL 48.4 COLON-ALIGNED HELP
          "Enter Beginning Description" NO-LABEL
     begin_cust-no AT ROW 3.86 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_name_description AT ROW 4.76 COL 48.4 COLON-ALIGNED HELP
          "Enter Ending Description" NO-LABEL
     end_cust-no AT ROW 4.81 COL 28 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_contact_sman-dscr AT ROW 5.71 COL 48.4 COLON-ALIGNED HELP
          "Enter Beginning Description" NO-LABEL
     begin_contact_sman AT ROW 5.76 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep"
     end_contact_sman-dscr AT ROW 6.67 COL 48.4 COLON-ALIGNED HELP
          "Enter Ending Description" NO-LABEL
     end_contact_sman AT ROW 6.71 COL 28 COLON-ALIGNED HELP
          "Enter Ending Sales Rep"
     begin_contact_zip AT ROW 3.86 COL 99 COLON-ALIGNED HELP
          "Enter Beginning Zip"
     end_contact_zip AT ROW 4.81 COL 99 COLON-ALIGNED HELP
          "Enter Ending Zip"
     selected-printer AT ROW 5.76 COL 99 COLON-ALIGNED HELP
          "Enter Printer for Control Codes"
     tbLastOrder AT ROW 6.95 COL 89
     tbMailMrge AT ROW 8.86 COL 4
     filename AT ROW 8.86 COL 48 COLON-ALIGNED
     FILL-IN-Title AT ROW 9.81 COL 48 COLON-ALIGNED
     F1 AT ROW 3.86 COL 46 NO-LABEL
     F-3 AT ROW 4.81 COL 46 NO-LABEL
     F-5 AT ROW 5.76 COL 46 NO-LABEL
     F-4 AT ROW 6.71 COL 46 NO-LABEL
     F-6 AT ROW 5.76 COL 117 NO-LABEL
     F-2 AT ROW 8.86 COL 114 NO-LABEL
     RECT-1 AT ROW 1 COL 1
     RECT-3 AT ROW 8.38 COL 2
     RECT-9 AT ROW 1 COL 1
     "List Order:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 1.24 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 2.43 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartListObject
   External Tables: contact
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 11.95
         WIDTH              = 120.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       begin_contact_sman:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* SETTINGS FOR FILL-IN begin_contact_sman-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       begin_contact_zip:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* SETTINGS FOR FILL-IN begin_name_description IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       end_contact_sman:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* SETTINGS FOR FILL-IN end_contact_sman-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       end_contact_zip:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* SETTINGS FOR FILL-IN end_name_description IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F-5 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F-6 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN filename IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       filename:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* SETTINGS FOR FILL-IN FILL-IN-Title IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-Title:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       list-order:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR FILL-IN selected-company IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       selected-company:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       selected-printer:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       tbLastOrder:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       tbMailMrge:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}
{methods/enhance.i}
{methods/listobjs/listobjs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME begin_contact_sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_contact_sman s-object
ON LEAVE OF begin_contact_sman IN FRAME F-Main /* Beginning SalesRep */
DO:
  RUN Display-Fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_contact_zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_contact_zip s-object
ON LEAVE OF begin_contact_zip IN FRAME F-Main /* Beginning Zip */
DO:
  RUN Display-Fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no s-object
ON LEAVE OF begin_cust-no IN FRAME F-Main /* Beginning Customer Number */
DO:
  RUN Display-Fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_contact_sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_contact_sman s-object
ON LEAVE OF end_contact_sman IN FRAME F-Main /* Ending SalesRep */
DO:
  RUN Display-Fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_contact_zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_contact_zip s-object
ON LEAVE OF end_contact_zip IN FRAME F-Main /* Ending Zip */
DO:
  RUN Display-Fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no s-object
ON LEAVE OF end_cust-no IN FRAME F-Main /* Ending Customer Number */
DO:
  RUN Display-Fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filename s-object
ON HELP OF filename IN FRAME F-Main /* Filename */
DO:
  DEFINE VARIABLE selected-name AS CHARACTER NO-UNDO.
  DEFINE VARIABLE sel-ok AS LOG NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

  init-dir = "users\" + USERID("NOSWEAT").
  selected-name = {&SELF-NAME}:SCREEN-VALUE.
  SYSTEM-DIALOG GET-FILE selected-name
      TITLE      "Choose Mail Merge File to SAVE ..."
      FILTERS    "Text Files (*.txt)" "*.txt"
      INITIAL-DIR init-dir
      ASK-OVERWRITE
      USE-FILENAME
      UPDATE sel-ok.
  IF NOT sel-ok THEN
  RETURN NO-APPLY.
  {&SELF-NAME}:SCREEN-VALUE = selected-name.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filename s-object
ON LEAVE OF filename IN FRAME F-Main /* Filename */
DO:
  if tbmailmrge and lastkey <> -1 and
     self:screen-value = ""
    then do:
        message "Mail Merge File must be entered. Try Help." view-as alert-box error.
        return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME list-order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL list-order s-object
ON VALUE-CHANGED OF list-order IN FRAME F-Main
DO:
  RUN Get-Display-Values.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbLastOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbLastOrder s-object
ON VALUE-CHANGED OF tbLastOrder IN FRAME F-Main /* Print Last Order Date */
DO:
  if tbMailMrge:checked then
    enable filename 
           fill-in-title with frame {&frame-name}.
  else
    disable filename 
            fill-in-title with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbMailMrge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbMailMrge s-object
ON VALUE-CHANGED OF tbMailMrge IN FRAME F-Main /* Create Mail Merge File */
DO:
  if tbMailMrge:checked then do:
     filename:screen-value in frame {&frame-name} = "C:\TEMP\mailmerg.txt".
     enable filename 
           fill-in-title 
           with frame {&frame-name}.
  end.         
  else do:
    filename:screen-value = "".
    disable filename 
            fill-in-title 
            with frame {&frame-name}.
  end.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

{custom/getcmpny.i}
{custom/getloc.i}

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create-List-Logic-Shell s-object 
PROCEDURE Create-List-Logic-Shell :
/*------------------------------------------------------------------------------
  Purpose:     Create List Logic Method
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lstlogic/lstshell.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Display-Fields s-object 
PROCEDURE Display-Fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign begin_cust-no:screen-value in frame {&frame-name} = CAPS(begin_cust-no:screen-value in frame {&frame-name})
         end_cust-no:screen-value in frame {&frame-name} = CAPS(end_cust-no:screen-value in frame {&frame-name})
         begin_contact_sman:screen-value in frame {&frame-name} = CAPS(begin_contact_sman:screen-value in frame {&frame-name})  
         end_contact_sman:screen-value in frame {&frame-name} = CAPS(end_contact_sman:screen-value in frame {&frame-name})
         begin_contact_zip:screen-value in frame {&frame-name} = CAPS(begin_contact_zip:screen-value in frame {&frame-name})  
         end_contact_zip:screen-value in frame {&frame-name} = CAPS(end_contact_zip:screen-value in frame {&frame-name}).

  find first cust where cust.company eq gcompany
                    and cust.cust-no eq begin_cust-no:screen-value in frame {&frame-name}
                  no-lock no-error.
  assign begin_name_description = if avail cust then cust.name
                                  else "".

  find first cust where cust.company eq gcompany
                    and cust.cust-no eq end_cust-no:screen-value in frame {&frame-name}
                  no-lock no-error.
  assign end_name_description = if avail cust then cust.name
                                else "".
                               
  find first sman where sman.company eq gcompany
                    and sman.sman    eq begin_contact_sman:screen-value in frame {&frame-name}
                  no-lock no-error.
  assign begin_contact_sman-dscr = if avail sman then sman.sname
                                  else "".

  find first sman where sman.company eq gcompany
                    and sman.sman    eq end_contact_sman:screen-value in frame {&frame-name}
                  no-lock no-error.
  assign end_contact_sman-dscr = if avail sman then sman.sname
                                  else "".


  display begin_name_description  end_name_description
          begin_contact_sman-dscr end_contact_sman-dscr 
     with frame {&frame-name}.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Frame-Handle s-object 
PROCEDURE Frame-Handle :
/*------------------------------------------------------------------------------
  Purpose:     Called externally to get THIS-PROCEDURE's Frame handle
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/listobjs/framehdl.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Show-Parameters s-object 
PROCEDURE Init-Show-Parameters :
/*------------------------------------------------------------------------------
  Purpose:     Supply Show Parameter setting to calling procedure.
  Parameters:  OUTPUT show parameters
  Notes:       
------------------------------------------------------------------------------*/
  {methods/listobjs/initshow.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
                       
  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
    selected-company = gcompany
    selected-company:SCREEN-VALUE IN FRAME {&FRAME-NAME} = gcompany.
                               
  for each contact where contact.company eq gcompany 
                   no-lock break by contact.sman:
    begin_contact_sman = contact.sman.
    leave.
  end.
  for each contact where contact.company eq gcompany 
                   no-lock break by contact.sman descending:
    end_contact_sman = contact.sman.
    leave.
  end.
  for each contact where contact.company eq gcompany 
                   no-lock break by contact.zip descending:
    end_contact_zip = contact.zip.
    leave.
  end.

 
  display begin_contact_sman end_contact_sman 
          end_contact_zip with frame {&frame-name}.
  
  RUN Display-Fields.
  
  {methods/listobjs/listinit.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


