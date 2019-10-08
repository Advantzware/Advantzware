&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS bTableWin 
/*------------------------------------------------------------------------

  File: adm2\src\browser.w

  Description: SmartDataBrowser Object

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "fa/sdoFaControl.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table FA-entity Entity-name Yr Prd ~
Number-prd calc-mode aqui-service tag-update fa-gl-disp fa-gl-clear ~
Currency-cod gl-installed Job-no last-ad last-prd not-used11 not-used12 ~
not-used13 not-used14 not-used15 not-used16 not-used17 not-used18 ~
not-used19 not-used110 not-used111 not-used112 not-used113 not-used21 ~
not-used22 not-used23 not-used24 not-used25 not-used26 not-used27 ~
not-used28 not-used29 not-used210 not-used211 not-used212 not-used213 ~
state-fed View-login 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH rowObject
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH rowObject.
&Scoped-define TABLES-IN-QUERY-br_table rowObject
&Scoped-define FIRST-TABLE-IN-QUERY-br_table rowObject


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE TEMP-TABLE RowObject
    {{&DATA-FIELD-DEFS}}
    {src/adm2/robjflds.i}.

DEFINE QUERY br_table FOR 
      rowObject SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table bTableWin _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      FA-entity FORMAT "x(8)":U WIDTH 11.43
      Entity-name FORMAT "x(40)":U
      Yr FORMAT ">>>9":U
      Prd FORMAT "99":U
      Number-prd FORMAT "99":U
      calc-mode FORMAT "x(3)":U WIDTH 20.72
      aqui-service FORMAT "Acquired/Service":U WIDTH 22.43
      tag-update FORMAT "Yes/No":U WIDTH 11.43
      fa-gl-disp FORMAT "x(8)":U WIDTH 13.57
      fa-gl-clear FORMAT "x(8)":U WIDTH 13.43
      Currency-cod FORMAT "x(3)":U
      gl-installed FORMAT "yes/no":U
      Job-no FORMAT "x(8)":U
      last-ad FORMAT "99":U
      last-prd FORMAT "99":U
      not-used11 FORMAT "99/99/99":U
      not-used12 FORMAT "99/99/99":U
      not-used13 FORMAT "99/99/99":U
      not-used14 FORMAT "99/99/99":U
      not-used15 FORMAT "99/99/99":U
      not-used16 FORMAT "99/99/99":U
      not-used17 FORMAT "99/99/99":U
      not-used18 FORMAT "99/99/99":U
      not-used19 FORMAT "99/99/99":U
      not-used110 FORMAT "99/99/99":U
      not-used111 FORMAT "99/99/99":U
      not-used112 FORMAT "99/99/99":U
      not-used113 FORMAT "99/99/99":U
      not-used21 FORMAT "99/99/99":U
      not-used22 FORMAT "99/99/99":U
      not-used23 FORMAT "99/99/99":U
      not-used24 FORMAT "99/99/99":U
      not-used25 FORMAT "99/99/99":U
      not-used26 FORMAT "99/99/99":U
      not-used27 FORMAT "99/99/99":U
      not-used28 FORMAT "99/99/99":U
      not-used29 FORMAT "99/99/99":U
      not-used210 FORMAT "99/99/99":U
      not-used211 FORMAT "99/99/99":U
      not-used212 FORMAT "99/99/99":U
      not-used213 FORMAT "99/99/99":U
      state-fed FORMAT "x(1)":U
      View-login FORMAT "yes/no":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS SIZE 66 BY 6.67
         BGCOLOR 8 FONT 3 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataBrowser
   Data Source: "fa/sdoFaControl.w"
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW bTableWin ASSIGN
         HEIGHT             = 6.88
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB bTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW bTableWin
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.FA-entity
"FA-entity" ? ? "character" ? ? ? ? ? ? no ? no no "11.43" yes no no "U" "" ""
     _FldNameList[2]   = _<SDO>.rowObject.Entity-name
     _FldNameList[3]   = _<SDO>.rowObject.Yr
     _FldNameList[4]   = _<SDO>.rowObject.Prd
     _FldNameList[5]   = _<SDO>.rowObject.Number-prd
     _FldNameList[6]   > _<SDO>.rowObject.calc-mode
"calc-mode" ? ? "character" ? ? ? ? ? ? no ? no no "20.72" yes no no "U" "" ""
     _FldNameList[7]   > _<SDO>.rowObject.aqui-service
"aqui-service" ? ? "logical" ? ? ? ? ? ? no ? no no "22.43" yes no no "U" "" ""
     _FldNameList[8]   > _<SDO>.rowObject.tag-update
"tag-update" ? ? "logical" ? ? ? ? ? ? no ? no no "11.43" yes no no "U" "" ""
     _FldNameList[9]   > _<SDO>.rowObject.fa-gl-disp
"fa-gl-disp" ? ? "character" ? ? ? ? ? ? no ? no no "13.57" yes no no "U" "" ""
     _FldNameList[10]   > _<SDO>.rowObject.fa-gl-clear
"fa-gl-clear" ? ? "character" ? ? ? ? ? ? no ? no no "13.43" yes no no "U" "" ""
     _FldNameList[11]   = _<SDO>.rowObject.Currency-cod
     _FldNameList[12]   = _<SDO>.rowObject.gl-installed
     _FldNameList[13]   = _<SDO>.rowObject.Job-no
     _FldNameList[14]   = _<SDO>.rowObject.last-ad
     _FldNameList[15]   = _<SDO>.rowObject.last-prd
     _FldNameList[16]   = _<SDO>.rowObject.not-used11
     _FldNameList[17]   = _<SDO>.rowObject.not-used12
     _FldNameList[18]   = _<SDO>.rowObject.not-used13
     _FldNameList[19]   = _<SDO>.rowObject.not-used14
     _FldNameList[20]   = _<SDO>.rowObject.not-used15
     _FldNameList[21]   = _<SDO>.rowObject.not-used16
     _FldNameList[22]   = _<SDO>.rowObject.not-used17
     _FldNameList[23]   = _<SDO>.rowObject.not-used18
     _FldNameList[24]   = _<SDO>.rowObject.not-used19
     _FldNameList[25]   = _<SDO>.rowObject.not-used110
     _FldNameList[26]   = _<SDO>.rowObject.not-used111
     _FldNameList[27]   = _<SDO>.rowObject.not-used112
     _FldNameList[28]   = _<SDO>.rowObject.not-used113
     _FldNameList[29]   = _<SDO>.rowObject.not-used21
     _FldNameList[30]   = _<SDO>.rowObject.not-used22
     _FldNameList[31]   = _<SDO>.rowObject.not-used23
     _FldNameList[32]   = _<SDO>.rowObject.not-used24
     _FldNameList[33]   = _<SDO>.rowObject.not-used25
     _FldNameList[34]   = _<SDO>.rowObject.not-used26
     _FldNameList[35]   = _<SDO>.rowObject.not-used27
     _FldNameList[36]   = _<SDO>.rowObject.not-used28
     _FldNameList[37]   = _<SDO>.rowObject.not-used29
     _FldNameList[38]   = _<SDO>.rowObject.not-used210
     _FldNameList[39]   = _<SDO>.rowObject.not-used211
     _FldNameList[40]   = _<SDO>.rowObject.not-used212
     _FldNameList[41]   = _<SDO>.rowObject.not-used213
     _FldNameList[42]   = _<SDO>.rowObject.state-fed
     _FldNameList[43]   = _<SDO>.rowObject.View-login
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON CTRL-END OF br_table IN FRAME F-Main
DO:
  APPLY "END":U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON CTRL-HOME OF br_table IN FRAME F-Main
DO:
  APPLY "HOME":U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON END OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsend.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON HOME OF br_table IN FRAME F-Main
DO:
  {src/adm2/brshome.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON OFF-END OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsoffnd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON OFF-HOME OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsoffhm.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON SCROLL-NOTIFY OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsscrol.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  {src/adm2/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK bTableWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN initializeObject.        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI bTableWin  _DEFAULT-DISABLE
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

