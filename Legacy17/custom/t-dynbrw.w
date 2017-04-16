&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

/* r-dynbrws */
DEFINE VARIABLE name-hdl AS WIDGET-HANDLE.
DEFINE VARIABLE num-hdl AS WIDGET-HANDLE.
DEFINE VARIABLE address-hdl AS WIDGET-HANDLE.
DEFINE VARIABLE calc-col-hdl AS WIDGET-HANDLE.
DEFINE VARIABLE browse-hdl AS WIDGET-HANDLE.
DEFINE VARIABLE buff-field-hdl AS WIDGET-HANDLE.
DEFINE VARIABLE brws-col-hdl AS WIDGET-HANDLE.
DEFINE BUTTON btn-delete LABEL "Delete".
DEFINE BUTTON btn-quit LABEL "&Quit"  AUTO-ENDKEY.
DEFINE VARIABLE j AS INTEGER.
                
  
DEFINE QUERY q1 FOR cust SCROLLING. 
             
OPEN QUERY q1 FOR EACH cust NO-LOCK.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.86 BY 11.92.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 11.91
         WIDTH              = 79.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartFrameCues" fFrameWin _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartFrame,ab,49268
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */
CREATE BROWSE browse-hdl    
       ASSIGN TITLE = "Dynamic Browse" 
       FRAME = FRAME {&frame-name}:HANDLE 
       QUERY = QUERY q1:HANDLE     
       X = 2         
       Y = 2        
       WIDTH = 74     
       DOWN = 10     
       VISIBLE = YES    
       SENSITIVE = TRUE  
       SEPARATORS = YES
       READ-ONLY = NO
    /*br_table:SELECTABLE IN FRAME F-Main             = TRUE
    br_table:MOVABLE IN FRAME F-Main                = TRUE
    br_table:RESIZABLE IN FRAME F-Main              = TRUE
    br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
    br_table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE
    br_table:COLUMN-MOVABLE IN FRAME F-Main         = TRUE
    br_table:ROW-RESIZABLE IN FRAME F-Main          = TRUE.
    */
      COLUMN-RESIZABLE = YES
      ROW-RESIZABLE = YES
      RESIZABLE = YES
      COLUMN-MOVABLE = YES
     .

ON row-display OF browse-hdl DO: 
     IF VALID-HANDLE(calc-col-hdl) THEN 
        calc-col-hdl:SCREEN-VALUE = STRING(cust.name).
END.
REPOSITION q1 TO ROW 1.           
num-hdl = browse-hdl:ADD-LIKE-COLUMN("cust.cust-no").
name-hdl = browse-hdl:ADD-LIKE-COLUMN("cust.name").
address-hdl = browse-hdl:ADD-LIKE-COLUMN("cust.addr[1]").
/*calc-col-hdl = browse-hdl:ADD-CALC-COLUMN("cust.cr-lim","->,>>>,>>9.99","","Credit Limit").
*/    
    /* Refresh needs to be done if ADD-CALC-COLUMN is done after the browse* is displayed. In ROW-DISPLAY trigger, we can only set the calc field's* screen-value if the handle is set. And the handle is set after the* ADD-CALC-COLUMN method is done. */
    
browse-hdl:refresh().
browse-hdl:EXPANDABLE = YES.
        
ON row-leave OF browse-hdl DO: 
   IF browse-hdl:CURRENT-ROW-MODIFIED THEN DO:
      REPEAT j = 1 TO browse-hdl:NUM-COLUMNS:
         brws-col-hdl = browse-hdl:GET-BROWSE-COLUMN(j).      
         IF brws-col-hdl:MODIFIED THEN DO:  
             buff-field-hdl = brws-col-hdl:BUFFER-FIELD.
                    /* if buff-field-hdl is unknown, this is a calculated fieldand cannot be updated */            
            IF buff-field-hdl NE ? THEN      
                     buff-field-hdl:BUFFER-VALUE = brws-col-hdl:SCREEN-VALUE.       
         END.
      END.
   END.
END.
/*
ON CHOOSE OF btn-delete DO:   /*  LABEL "DeleteDynBrowse". */
   DELETE WIDGET browse-hdl.
END.
        
ON CHOOSE OF btn-quit DO:
            QUIT.
END.
*/
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
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
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
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
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

