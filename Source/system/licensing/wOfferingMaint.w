&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hBufferField AS HANDLE NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR hBrowse AS HANDLE NO-UNDO.
DEF VAR hBrowseCol AS HANDLE NO-UNDO EXTENT 7.
DEF VAR lInRow AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttOfferingMatrix
    FIELD iOfferingCode AS INT  
    FIELD cOfferingCode AS CHAR FORMAT "x(22)" LABEL "Offering Code"
    FIELD cDescription AS CHAR FORMAT "x(113)" LABEL "Description"
    FIELD xPlan1 AS CHAR FORMAT "x(6)" LABEL "  Est"
    FIELD xPlan2 AS CHAR FORMAT "x(6)" LABEL " Base"
    FIELD xPlan3 AS CHAR FORMAT "x(6)" LABEL " Full"
    FIELD xPlan4 AS CHAR FORMAT "x(6)" LABEL " Full+"
    FIELD xPlan5 AS CHAR FORMAT "x(6)" LABEL "  All"
    . 
    
FOR EACH zOfferingMatrix NO-LOCK:
    FIND FIRST ttOfferingMatrix WHERE 
        ttOfferingMatrix.iOfferingCode EQ zOfferingMatrix.iOfferingCode
        NO-ERROR.
    IF NOT AVAIL ttOfferingMatrix THEN DO:
        CREATE ttOfferingMatrix.
        ASSIGN 
            ttOfferingMatrix.iOfferingCode = zOfferingMatrix.iOfferingCode.
        FIND FIRST zOfferings NO-LOCK WHERE 
            zOfferings.iOfferingCode EQ zOfferingMatrix.iOfferingCode
            NO-ERROR.
        ASSIGN
            ttOfferingMatrix.cOfferingCode = IF AVAIL zOfferings THEN zOfferings.cOfferingCode ELSE ""
            ttOfferingMatrix.cDescription = IF AVAIL zOfferings THEN zOfferings.cDescription ELSE "".
    END.
    CASE zOfferingMatrix.iPlanLevel:
        WHEN 1 THEN ASSIGN ttOfferingMatrix.xPlan1 = IF zOfferingMatrix.lOptional THEN "   O" ELSE 
                                                       IF zOfferingMatrix.lDefault THEN "   X" ELSE
                                                       "". 
        WHEN 2 THEN ASSIGN ttOfferingMatrix.xPlan2 = IF zOfferingMatrix.lOptional THEN "   O" ELSE 
                                                       IF zOfferingMatrix.lDefault THEN "   X" ELSE
                                                       "". 
        WHEN 3 THEN ASSIGN ttOfferingMatrix.xPlan3 = IF zOfferingMatrix.lOptional THEN "   O" ELSE 
                                                       IF zOfferingMatrix.lDefault THEN "   X" ELSE
                                                       "". 
        WHEN 4 THEN ASSIGN ttOfferingMatrix.xPlan4 = IF zOfferingMatrix.lOptional THEN "   O" ELSE 
                                                       IF zOfferingMatrix.lDefault THEN "   X" ELSE
                                                       "". 
        WHEN 5 THEN ASSIGN ttOfferingMatrix.xPlan5 = IF zOfferingMatrix.lOptional THEN "   O" ELSE 
                                                       IF zOfferingMatrix.lDefault THEN "   X" ELSE
                                                       "". 
    END CASE. 
END.    

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bUpdate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bUpdate 
     LABEL "Update" 
     SIZE 15 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     bUpdate AT ROW 29.57 COL 82 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 183.6 BY 30.76 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Product Package Subscription Options"
         HEIGHT             = 30.76
         WIDTH              = 183.6
         MAX-HEIGHT         = 34.62
         MAX-WIDTH          = 185.8
         VIRTUAL-HEIGHT     = 34.62
         VIRTUAL-WIDTH      = 185.8
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Product Package Subscription Options */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Product Package Subscription Options */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bUpdate wWin
ON CHOOSE OF bUpdate IN FRAME fMain /* Update */
DO:
    DEF VAR ictr AS INT.
    DEF VAR hColumn AS HANDLE.
    CASE SELF:LABEL:
        WHEN "Update" THEN DO:
            DO ictr = 3 TO 7:
                hBrowseCol[ictr] = hBrowse:GET-BROWSE-COLUMN(iCtr).
                hBrowseCol[ictr]:READ-ONLY = FALSE.
            END.
            ASSIGN 
                SELF:LABEL = "Save".
        END.  
        WHEN "Save" THEN DO:
            DO ictr = 3 TO 7:
                hBrowseCol[ictr] = hBrowse:GET-BROWSE-COLUMN(iCtr).
                hBrowseCol[ictr]:READ-ONLY = TRUE.
            END.
            ASSIGN 
                SELF:LABEL = "Update".
        END.  
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */


{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  ENABLE bUpdate 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
CREATE BUFFER hBuffer FOR TABLE "ttOfferingMatrix".
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH ttOfferingMatrix by ttOfferingMatrix.iOfferingCode").
hQuery:QUERY-OPEN.

CREATE BROWSE hBrowse
    ASSIGN 
        SEPARATORS = FALSE 
        ROW-MARKERS = FALSE 
        FRAME   = FRAME fMain:HANDLE  
        QUERY   = hQuery  
        ROW     = 1.48
        COLUMN  = 5
        WIDTH   = 175
        HEIGHT  = 27.62 
        VISIBLE = TRUE 
        READ-ONLY = FALSE 
        SENSITIVE = TRUE 
    TRIGGERS:
        ON ROW-ENTRY DO:
            ASSIGN 
                lInRow = TRUE.
        END.
        ON ROW-LEAVE DO:
            IF hBrowse:CURRENT-ROW-MODIFIED THEN DO:
            END.
        END.
    END TRIGGERS.
        
ASSIGN 
    hBrowseCol[1] = hBrowse:ADD-LIKE-COLUMN(hBuffer:BUFFER-FIELD(2))
    hBrowseCol[2] = hBrowse:ADD-LIKE-COLUMN(hBuffer:BUFFER-FIELD(3))
    hBrowseCol[3] = hBrowse:ADD-LIKE-COLUMN(hBuffer:BUFFER-FIELD(4))
    hBrowseCol[4] = hBrowse:ADD-LIKE-COLUMN(hBuffer:BUFFER-FIELD(5))
    hBrowseCol[5] = hBrowse:ADD-LIKE-COLUMN(hBuffer:BUFFER-FIELD(6))
    hBrowseCol[6] = hBrowse:ADD-LIKE-COLUMN(hBuffer:BUFFER-FIELD(7))
    hBrowseCol[7] = hBrowse:ADD-LIKE-COLUMN(hBuffer:BUFFER-FIELD(8)).
    
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

