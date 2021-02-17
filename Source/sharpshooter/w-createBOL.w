&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter/w-relbol.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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
{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}
{system/sysconst.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompanyName AS CHARACTER NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

DEFINE VARIABLE glScanTrailer AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiTag btDelete btPrintBOL 
&Scoped-Define DISPLAYED-OBJECTS fiTag fiTrailer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-releaseitems AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-releasetags AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_releasefilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_viewfginquiry AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btChange 
     LABEL "Change" 
     SIZE 15 BY 1.48.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete Selected Tag".

DEFINE BUTTON btPrintBOL 
     LABEL "Print BOL" 
     SIZE 22 BY 1.91.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tag #" 
     VIEW-AS FILL-IN 
     SIZE 66.2 BY 1.38 TOOLTIP "Enter Tag #" NO-UNDO.

DEFINE VARIABLE fiTrailer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Trailer #" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1.38 TOOLTIP "Enter Trailer #" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btChange AT ROW 3.19 COL 61.6 WIDGET-ID 14 NO-TAB-STOP 
     fiTag AT ROW 4.81 COL 17.8 COLON-ALIGNED WIDGET-ID 8
     fiTrailer AT ROW 4.81 COL 99.6 COLON-ALIGNED WIDGET-ID 10
     btDelete AT ROW 12.57 COL 194.8 WIDGET-ID 16 NO-TAB-STOP 
     btPrintBOL AT ROW 27.1 COL 172.2 WIDGET-ID 12 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204 BY 28.33
         BGCOLOR 15 FONT 17 WIDGET-ID 100.

DEFINE FRAME Option-frame
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204 BY 1.91
         BGCOLOR 21  WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Create BOL"
         HEIGHT             = 28.33
         WIDTH              = 204
         MAX-HEIGHT         = 36.57
         MAX-WIDTH          = 204
         VIRTUAL-HEIGHT     = 36.57
         VIRTUAL-WIDTH      = 204
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME Option-frame:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btChange IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btChange:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiTrailer IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME Option-frame
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Create BOL */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Create BOL */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btChange
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btChange W-Win
ON CHOOSE OF btChange IN FRAME F-Main /* Change */
DO:
    RUN pInvalidRelease.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete W-Win
ON CHOOSE OF btDelete IN FRAME F-Main /* Delete */
DO:
    {methods/run_link.i "REL-TAGS-SOURCE" "DeleteReleaseTag"}
    
    RUN pScanRelease.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrintBOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrintBOL W-Win
ON CHOOSE OF btPrintBOL IN FRAME F-Main /* Print BOL */
DO:
    RUN state-changed (
        INPUT THIS-PROCEDURE,
        INPUT "print-bol"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON ANY-KEY OF fiTag IN FRAME F-Main /* Tag # */
DO:
    IF KEY-LABEL(LASTKEY) EQ "ENTER" OR KEY-LABEL(LASTKEY) EQ "TAB" AND SELF:SCREEN-VALUE NE "" THEN
        APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON ENTRY OF fiTag IN FRAME F-Main /* Tag # */
DO:
    SELF:SET-SELECTION ( 1, -1).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON LEAVE OF fiTag IN FRAME F-Main /* Tag # */
DO:
    IF LASTKEY EQ -1 OR SELF:SCREEN-VALUE EQ "" THEN
        RETURN.

    IF glScanTrailer THEN
        RETURN.        

    RUN state-changed (
            INPUT THIS-PROCEDURE,
            INPUT "scan-tag"
            ).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTrailer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTrailer W-Win
ON ANY-KEY OF fiTrailer IN FRAME F-Main /* Trailer # */
DO:
    IF KEY-LABEL(LASTKEY) EQ "ENTER" OR KEY-LABEL(LASTKEY) EQ "TAB" AND SELF:SCREEN-VALUE NE "" THEN
        APPLY "LEAVE" TO SELF.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTrailer W-Win
ON ENTRY OF fiTrailer IN FRAME F-Main /* Trailer # */
DO:
    SELF:SET-SELECTION ( 1, -1).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTrailer W-Win
ON LEAVE OF fiTrailer IN FRAME F-Main /* Trailer # */
DO:
    RUN state-changed (
        INPUT THIS-PROCEDURE,
        INPUT "scan-tag"
        ).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME Option-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 196.80 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/releasefilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_releasefilter ).
       RUN set-position IN h_releasefilter ( 3.00 , 1.80 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 58.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/viewfginquiry.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_viewfginquiry ).
       RUN set-position IN h_viewfginquiry ( 4.52 , 177.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 17.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-releaseitems.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-releaseitems ).
       RUN set-position IN h_b-releaseitems ( 6.52 , 3.40 ) NO-ERROR.
       RUN set-size IN h_b-releaseitems ( 5.91 , 190.60 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-releasetags.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-releasetags ).
       RUN set-position IN h_b-releasetags ( 12.62 , 3.20 ) NO-ERROR.
       RUN set-size IN h_b-releasetags ( 14.29 , 190.80 ) NO-ERROR.

       /* Links to SmartObject h_releasefilter. */
       RUN add-link IN adm-broker-hdl ( h_releasefilter , 'RELEASE':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_releasefilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartBrowser h_b-releaseitems. */
       RUN add-link IN adm-broker-hdl ( h_viewfginquiry , 'FGInq':U , h_b-releaseitems ).
       RUN add-link IN adm-broker-hdl ( h_b-releaseitems , 'REL-ITEMS':U , THIS-PROCEDURE ).

       /* Links to SmartBrowser h_b-releasetags. */
       RUN add-link IN adm-broker-hdl ( h_b-releasetags , 'REL-TAGS':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_releasefilter ,
             FRAME Option-frame:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-releaseitems ,
             fiTrailer:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY fiTag fiTrailer 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fiTag btDelete btPrintBOL 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME Option-frame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-Option-frame}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN spGetSessionParam (INPUT "Company", OUTPUT cCompany).
    RUN spGetSessionParam (INPUT "CompanyName", OUTPUT cCompanyName).
    RUN spGetSessionParam (INPUT "Location", OUTPUT cLocation).

    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - {&awversion}" + " - " 
                         + cCompanyName + " - " + cLocation.
                         
    fiTrailer:HIDDEN = NOT glScanTrailer.
    
    RUN pInvalidRelease.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInValidRelease W-Win 
PROCEDURE pInValidRelease PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    ASSIGN
        fiTag:SENSITIVE     = FALSE
        fiTrailer:SENSITIVE = FALSE
        btChange:HIDDEN     = TRUE
        btChange:SENSITIVE  = FALSE
        .

    {methods/run_link.i "RELEASE-SOURCE" "EnableRelease"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintBOL W-Win 
PROCEDURE pPrintBOL PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iReleaseID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBOLID     AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    DEFINE VARIABLE oReleaseHeader AS oe.ReleaseHeader NO-UNDO.
    
    {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
    
    iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID")).
    
    {methods/run_link.i "REL-TAGS-SOURCE" "CreateBOLFromRelease" "(INPUT cCompany, INPUT iReleaseID, OUTPUT lSuccess, OUTPUT cMessage, OUTPUT iBOLID)"}

    IF NOT lSuccess THEN DO:
        MESSAGE cMessage
        VIEW-AS ALERT-BOX ERROR.
        
        RETURN.
    END.    
 
    MESSAGE "Release posted. BOL # '" + STRING(iBOLID) + "' is generated"
    VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanRelease W-Win 
PROCEDURE pScanRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iReleaseID AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE oReleaseHeader AS oe.ReleaseHeader NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    {methods/run_link.i "REL-ITEMS-SOURCE" "EmptyReleaseItems"}

    {methods/run_link.i "REL-TAGS-SOURCE" "EmptyReleaseTags"}
    
    {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
    
    IF NOT VALID-OBJECT(oReleaseHeader) THEN DO:
        MESSAGE "Invalid Release#"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    IF NOT oReleaseHeader:IsAvailable() THEN DO:
        MESSAGE "Invalid Release#"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.    

    IF NOT LOGICAL(oReleaseHeader:GetValue("Printed")) THEN DO:
        MESSAGE "Release# '" + oReleaseHeader:GetValue("ReleaseID") + "' is not printed"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    IF LOGICAL(oReleaseHeader:GetValue("Posted")) THEN DO:
        MESSAGE "Release# '" + oReleaseHeader:GetValue("ReleaseID") + "' is already posted"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID")).
    
    {methods/run_link.i "REL-ITEMS-SOURCE" "BuildReleaseItems" "(INPUT cCompany, INPUT iReleaseID)"}

    {methods/run_link.i "REL-TAGS-SOURCE" "BuildReleaseTags" "(INPUT cCompany, INPUT iReleaseID)"}
    
    fiTag:SCREEN-VALUE = "".
    
    APPLY "ENTRY" TO fiTag.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanTag W-Win 
PROCEDURE pScanTag PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iReleaseID AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    DEFINE VARIABLE oReleaseHeader AS oe.ReleaseHeader NO-UNDO.
    
    {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
    
    iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID")).
    
    {methods/run_link.i "REL-TAGS-SOURCE" "CreateReleaseTag" "(INPUT cCompany, INPUT iReleaseID, INPUT fiTag:SCREEN-VALUE, INPUT fiTrailer:SCREEN-VALUE, OUTPUT lError, OUTPUT cMessage)"}

    IF lError THEN DO:
        MESSAGE cMessage
        VIEW-AS ALERT-BOX ERROR.
        
        fiTag:SCREEN-VALUE = "".
                
        APPLY "ENTRY" TO fiTag.

        RETURN.
    END.    
    
    RUN pScanRelease.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidRelease W-Win 
PROCEDURE pValidRelease PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    ASSIGN
        fiTag:SENSITIVE     = TRUE
        fiTrailer:SENSITIVE = TRUE
        btChange:HIDDEN     = FALSE
        btChange:SENSITIVE  = TRUE
        .

    {methods/run_link.i "RELEASE-SOURCE" "DisableRelease"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_Exit W-Win 
PROCEDURE Select_Exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN dispatch (
        INPUT "exit"
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus W-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/run_link.i "RELEASE-SOURCE" "Set-Focus"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.
  
    CASE p-state:
        WHEN "release-invalid" THEN DO:
            RUN pInValidRelease.
        END.
        WHEN "release-valid" THEN DO:
            RUN pValidRelease.
            
            RUN pScanRelease.
        END.
        WHEN "scan-tag" THEN DO:
            RUN pScanTag.
        END.
        WHEN "print-bol" THEN DO:
            RUN pPrintBOL.
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

