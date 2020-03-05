&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: system/d-dataComp.w

  Description: To show the differences between file and database record 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Rahul Rawat

  Created: Feb 12, 2020
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER hdTempTable   AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKey     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTableNAME  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFields     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcPrimaryKey AS CHARACTER NO-UNDO.


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdQuery       AS HANDLE NO-UNDO.
DEFINE VARIABLE hdTTBuffer    AS HANDLE NO-UNDO.
DEFINE VARIABLE hdTableBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE hdJSONProcs   AS HANDLE NO-UNDO.
 
DEFINE VARIABLE lcData        AS LONGCHAR NO-UNDO.

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cbFieldName edFileRecord edDatabaseRecord ~
fiPrimaryKey 
&Scoped-Define DISPLAYED-OBJECTS cbFieldName edFileRecord edDatabaseRecord ~
fiPrimaryKey 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btUpdate 
     LABEL "Update Field" 
     SIZE 23 BY 1.67
     FONT 6.

DEFINE VARIABLE cbFieldName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Select Field" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 31.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE edDatabaseRecord AS LONGCHAR 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 66 BY 16.43
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE edFileRecord AS LONGCHAR 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 68 BY 16.43
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE fiPrimaryKey AS CHARACTER FORMAT "X(32)":U 
      VIEW-AS TEXT 
     SIZE 73 BY 1
     FONT 5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cbFieldName AT ROW 1.71 COL 12.6 COLON-ALIGNED WIDGET-ID 6
     edFileRecord AT ROW 5 COL 3 NO-LABEL WIDGET-ID 2
     edDatabaseRecord AT ROW 5 COL 73 NO-LABEL WIDGET-ID 4
     btUpdate AT ROW 21.95 COL 60 WIDGET-ID 20
     fiPrimaryKey AT ROW 1.76 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     "File Record" VIEW-AS TEXT
          SIZE 13.2 BY 1.19 AT ROW 3.71 COL 29 WIDGET-ID 8
          FONT 6
     "Database Record" VIEW-AS TEXT
          SIZE 21.2 BY 1.19 AT ROW 3.67 COL 95 WIDGET-ID 10
          FONT 6
     "Primary Key:" VIEW-AS TEXT
          SIZE 15.2 BY .62 AT ROW 1.95 COL 47.8 WIDGET-ID 22
          FONT 6
     SPACE(76.79) SKIP(21.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 
         TITLE "View Differences" WIDGET-ID 100.


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

/* SETTINGS FOR BUTTON btUpdate IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       edDatabaseRecord:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       edFileRecord:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* View Differences */
DO:
    IF VALID-HANDLE(hdQuery) THEN
        DELETE OBJECT hdQuery.
        
    IF VALID-HANDLE(hdTTBuffer) THEN
        DELETE OBJECT hdTTBuffer
        .
    IF VALID-HANDLE(hdTableBuffer) THEN
        DELETE OBJECT hdTableBuffer. 
                   
    IF VALID-HANDLE(hdJSONProcs) THEN
        DELETE OBJECT hdJSONProcs.
                
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate Dialog-Frame
ON CHOOSE OF btUpdate IN FRAME Dialog-Frame /* Update Field */
DO:
    MESSAGE "Do you want to update the field into the database?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS 
        OK-CANCEL UPDATE lCheckFlag as LOGICAL.
    
    IF lCheckFlag THEN 
        RUN pUpdateField.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFieldName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFieldName Dialog-Frame
ON VALUE-CHANGED OF cbFieldName IN FRAME Dialog-Frame /* Select Field */
DO:
    ASSIGN {&SELF-NAME}.
  
    RUN pShowDataDifference.
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
  RUN pDisplayFields. 
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
  DISPLAY cbFieldName edFileRecord edDatabaseRecord fiPrimaryKey 
      WITH FRAME Dialog-Frame.
  ENABLE cbFieldName edFileRecord edDatabaseRecord fiPrimaryKey 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayFields Dialog-Frame 
PROCEDURE pDisplayFields PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: To populate the "Select Field" Combo-Box     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        cbFieldNAME:LIST-ITEMS IN FRAME {&FRAME-NAME} = ipcFields
        fiPrimaryKey                                  = ipcPrimaryKey
        cbFieldName:SCREEN-VALUE                      = ENTRY(1,ipcFields)
        .
    APPLY "VALUE-CHANGED":U TO cbFieldName.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShowDataDifference Dialog-Frame 
PROCEDURE pShowDataDifference PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: To Show the differences between the Temp-Table and Database in the 
           editors    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    CREATE QUERY hdQuery.
    
    CREATE BUFFER hdTableBuffer FOR TABLE ipcTableName.
    
    hdTTBuffer = hdTempTable:DEFAULT-BUFFER-HANDLE. 
    
    hdQuery:SET-BUFFERS (hdTableBuffer,hdTTBuffer).
    
    hdQuery:QUERY-PREPARE("FOR EACH " + hdTableBuffer:NAME + 
                             " WHERE " + hdTableBuffer:NAME + ".rec_key" + " EQ " + '"'+ ipcRecKey + '"' + 
                            ", FIRST " + hdTTBuffer:NAME + 
                              " WHERE " + hdTTBuffer:NAME   + ".rec_key" + " EQ " + '"'+ ipcRecKey + '"').
    hdQuery:QUERY-OPEN.
    hdQuery:GET-FIRST.
    
    IF hdTTBuffer:AVAILABLE AND hdTableBuffer:AVAILABLE THEN DO:
        ASSIGN   
            edFileRecord:SCREEN-VALUE     = STRING(hdTTBuffer:BUFFER-FIELD(cbFieldName):BUFFER-VALUE)
            edDataBaseRecord:SCREEN-VALUE = STRING(hdTableBuffer:BUFFER-FIELD(cbFieldName):BUFFER-VALUE)
            .     
    
        IF hdTTBuffer:BUFFER-FIELD(cbFieldName):DATA-TYPE = "CLOB" THEN DO:
            edFileRecord:SCREEN-VALUE = DYNAMIC-FUNCTION(
                                         "fBeautifyJSON" IN hdJSONProcs,
                                          edFileRecord:SCREEN-VALUE 
                                         ).
            edDataBaseRecord:SCREEN-VALUE = DYNAMIC-FUNCTION(
                                         "fBeautifyJSON" IN hdJSONProcs,
                                          edDataBaseRecord:SCREEN-VALUE
                                         ).  
        END.
    
        btUpdate:SENSITIVE = TRUE.
    END.        
                 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateField Dialog-Frame 
PROCEDURE pUpdateField :
/*------------------------------------------------------------------------------
  Purpose: To Update the Selected Field into the DataBase 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
       
    DO TRANSACTION:        
        hdQuery:GET-CURRENT(EXCLUSIVE-LOCK).        
        hdTableBuffer:BUFFER-FIELD(cbFieldName):BUFFER-VALUE =  hdTTBuffer:BUFFER-FIELD(cbFieldName):BUFFER-VALUE NO-ERROR.           
    END.
   
    IF NOT ERROR-STATUS:ERROR THEN DO:
        cbFieldName:DELETE(cbFieldName:LOOKUP(cbFieldName)).
        IF hdTableBuffer:BUFFER-FIELD(cbFieldName):BUFFER-VALUE EQ hdTTBuffer:BUFFER-FIELD(cbFieldName):BUFFER-VALUE THEN
            ASSIGN 
                btUpdate:SENSITIVE            = FALSE
                edFileRecord:SCREEN-VALUE     = STRING(hdTTBuffer:BUFFER-FIELD(cbFieldName):BUFFER-VALUE)
                edDataBaseRecord:SCREEN-VALUE = STRING(hdTableBuffer:BUFFER-FIELD(cbFieldName):BUFFER-VALUE)
                .     
    
        IF hdTTBuffer:BUFFER-FIELD(cbFieldName):DATA-TYPE = "CLOB" THEN DO:
            edFileRecord:SCREEN-VALUE = DYNAMIC-FUNCTION(
                                         "fBeautifyJSON" IN hdJSONProcs,
                                          edFileRecord:SCREEN-VALUE 
                                         ).
         edDataBaseRecord:SCREEN-VALUE = DYNAMIC-FUNCTION(
                                         "fBeautifyJSON" IN hdJSONProcs,
                                          edDataBaseRecord:SCREEN-VALUE
                                         ).  
        END.
        hdTableBuffer:BUFFER-RELEASE().
        MESSAGE "Data Updated Successfully"
            VIEW-AS ALERT-BOX INFORMATION BUTTON OK.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

