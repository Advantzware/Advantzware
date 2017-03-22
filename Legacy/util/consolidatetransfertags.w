&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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

{src/adm2/widgetprto.i}

DEF TEMP-TABLE tt-rcpth LIKE fg-rcpth.
DEF TEMP-TABLE tt-rdtlh LIKE fg-rdtlh.
DEF STREAM s1.
DEF STREAM s2.
/* Local Variable Definitions ---                                       */
 {custom/globdefs.i} 
{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

assign
 cocode = gcompany.
IF cocode EQ '' THEN
    cocode = '001'.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain itemfg.i-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain itemfg.i-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain itemfg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain itemfg
&Scoped-define QUERY-STRING-fMain FOR EACH itemfg SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH itemfg SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-fMain itemfg


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg.i-no 
&Scoped-define ENABLED-TABLES itemfg
&Scoped-define FIRST-ENABLED-TABLE itemfg
&Scoped-Define ENABLED-OBJECTS btOk 
&Scoped-Define DISPLAYED-FIELDS itemfg.i-no 
&Scoped-define DISPLAYED-TABLES itemfg
&Scoped-define FIRST-DISPLAYED-TABLE itemfg


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btOk 
     LABEL "OK" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     itemfg.i-no AT ROW 3.14 COL 18 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     btOk AT ROW 6.24 COL 21 WIDGET-ID 6
     "Enter an item number and press OK" VIEW-AS TEXT
          SIZE 48 BY 1.19 AT ROW 1.48 COL 16 WIDGET-ID 4
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.2 BY 10.48 WIDGET-ID 100.


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
         TITLE              = "Consolidate Transfer Transactions"
         HEIGHT             = 10.48
         WIDTH              = 79.2
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "asi.itemfg"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consolidate Transfer Transactions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consolidate Transfer Transactions */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOk wWin
ON CHOOSE OF btOk IN FRAME fMain /* OK */
DO:
  DEF VAR v-i-no LIKE itemfg.i-no NO-UNDO.
  v-i-no = asi.itemfg.i-no:SCREEN-VALUE.
  FIND FIRST itemfg WHERE itemfg.i-no EQ v-i-no NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
      MESSAGE "Invalid item number entered."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  RUN clean-up-transactions (INPUT asi.itemfg.i-no:SCREEN-VALUE).
  MESSAGE "Done!"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clean-up-transactions wWin 
PROCEDURE clean-up-transactions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ip-i-no LIKE fg-rdtlh.i-no NO-UNDO.    
    OUTPUT STREAM s1 TO c:\tmp\consolidateTransfersH.txt.
    OUTPUT STREAM s2 TO c:\tmp\consolidateTransfersD.txt.

    EMPTY TEMP-TABLE tt-rcpth.
    EMPTY TEMP-TABLE tt-rdtlh.
    
    CURRENT-WINDOW:WIDTH-CHARS = 200.
    
    FOR EACH fg-rcpth NO-LOCK
      WHERE fg-rcpth.company      EQ cocode /* itemfg.company */
        AND fg-rcpth.i-no         EQ ip-i-no /* itemfg.i-no */
        AND fg-rcpth.rita-code    EQ "T" 
      USE-INDEX tran,
    
      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code
      USE-INDEX rm-rdtl.
      EXPORT STREAM s1 fg-rcpth.
      EXPORT STREAM s2 fg-rdtlh.
      FIND FIRST tt-rdtlh WHERE tt-rdtlh.company EQ fg-rdtlh.company
                            AND tt-rdtlh.i-no    EQ fg-rdtlh.i-no
                            AND tt-rdtlh.cust-no    EQ fg-rdtlh.cust-no
                            AND tt-rdtlh.loc     EQ fg-rdtlh.loc
                            AND tt-rdtlh.loc-bin EQ fg-rdtlh.loc-bin
                            AND tt-rdtlh.tag     EQ fg-rdtlh.tag
                            AND tt-rdtlh.job-no  EQ fg-rdtlh.job-no
                            AND tt-rdtlh.job-no2 EQ fg-rdtlh.job-no2
                            AND tt-rdtlh.trans-date EQ fg-rcpth.trans-date
                            AND tt-rdtlh.trans-time EQ fg-rdtlh.trans-time
                            AND (IF fg-rdtlh.qty LT 0 THEN tt-rdtlh.qty LT 0 ELSE TRUE)
                            AND (IF fg-rdtlh.qty GT 0 THEN tt-rdtlh.qty GT 0 ELSE TRUE)
                          NO-ERROR.
      IF NOT AVAIL tt-rdtlh THEN DO:
          CREATE tt-rdtlh.
          BUFFER-COPY fg-rdtlh TO tt-rdtlh.
          tt-rdtlh.trans-date = fg-rcpth.trans-date.
          tt-rdtlh.qty = fg-rdtlh.qty.
      END.
      ELSE DO:
          
          tt-rdtlh.qty = tt-rdtlh.qty + fg-rdtlh.qty.
          DELETE fg-rdtlh.
      END.
    END.
    

    FOR EACH fg-rcpth NO-LOCK
      WHERE fg-rcpth.company      EQ cocode
        AND fg-rcpth.i-no         EQ ip-i-no
        AND fg-rcpth.rita-code    EQ "T" 
      USE-INDEX tran,
    
      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
    
        AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code
      USE-INDEX rm-rdtl.
    
      FIND FIRST tt-rdtlh WHERE tt-rdtlh.company EQ fg-rdtlh.company
                            AND tt-rdtlh.i-no    EQ fg-rdtlh.i-no
                            AND tt-rdtlh.cust-no    EQ fg-rdtlh.cust-no
                            AND tt-rdtlh.loc     EQ fg-rdtlh.loc
                            AND tt-rdtlh.loc-bin EQ fg-rdtlh.loc-bin
                            AND tt-rdtlh.tag     EQ fg-rdtlh.tag
                            AND tt-rdtlh.job-no  EQ fg-rdtlh.job-no
                            AND tt-rdtlh.job-no2 EQ fg-rdtlh.job-no2
                            AND tt-rdtlh.trans-date EQ fg-rcpth.trans-date
                            AND tt-rdtlh.trans-time EQ fg-rdtlh.trans-time
                            AND (IF fg-rdtlh.qty LT 0 THEN tt-rdtlh.qty LT 0 ELSE TRUE)
                            AND (IF fg-rdtlh.qty GT 0 THEN tt-rdtlh.qty GT 0 ELSE TRUE)
                            AND tt-rdtlh.r-no    EQ fg-rdtlh.r-no
                          NO-ERROR.
      IF AVAIL tt-rdtlh THEN
          fg-rdtlh.qty = tt-rdtlh.qty.
    END.
    
    /* If any fg-rdtlh were deleted, make sure no orphan fg-rcpth are left */
    FOR EACH fg-rcpth 
      WHERE fg-rcpth.company      EQ cocode
        AND fg-rcpth.i-no         EQ ip-i-no 
        AND fg-rcpth.rita-code    EQ "T" 
      USE-INDEX tran:
    
      FIND FIRST fg-rdtlh WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
                            AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code
                          USE-INDEX rm-rdtl                     
                          NO-LOCK NO-ERROR.
      IF NOT AVAIL fg-rdtlh THEN
          DELETE fg-rcpth.

    END.

    OUTPUT STREAM s1 CLOSE.
    OUTPUT STREAM s2 CLOSE.
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

  {&OPEN-QUERY-fMain}
  GET FIRST fMain.
  IF AVAILABLE itemfg THEN 
    DISPLAY itemfg.i-no 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE itemfg.i-no btOk 
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

