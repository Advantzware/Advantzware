&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          ptdb1            PROGRESS
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
&Scoped-define DATA-FIELD-DEFS "fa/sdodeptab.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table method Auto-sl Ratio Conv Manual ~
soyd Depr1 Depr2 Depr3 Depr4 Depr5 Depr6 Depr7 Depr8 Depr9 Depr10 Depr11 ~
Depr12 Depr13 Depr14 Depr15 Depr16 Depr17 Depr18 Depr19 Depr20 Depr21 ~
Depr22 Depr23 Depr24 Depr25 Depr26 Depr27 Depr28 Depr29 Depr30 Depr31 ~
Depr32 Depr33 Depr34 Depr35 Depr36 Depr37 Depr38 Depr39 Depr40 
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
      method FORMAT "x(5)":U WIDTH 8.43
      Auto-sl FORMAT "Straight line/Declining":U
      Ratio FORMAT "9.99":U WIDTH 15.86
      Conv FORMAT "x(3)":U
      Manual FORMAT "yes/no":U WIDTH 9.43
      soyd FORMAT "Yes/No":U
      Depr1 COLUMN-LABEL "Yr 1" FORMAT ">>,>>>,>>9.99":U
      Depr2 COLUMN-LABEL "Yr 2" FORMAT ">>,>>>,>>9.99":U
      Depr3 COLUMN-LABEL "Yr 3" FORMAT ">>,>>>,>>9.99":U
      Depr4 COLUMN-LABEL "Yr 4" FORMAT ">>,>>>,>>9.99":U
      Depr5 COLUMN-LABEL "Yr 5" FORMAT ">>,>>>,>>9.99":U
      Depr6 FORMAT ">>,>>>,>>9.99":U
      Depr7 FORMAT ">>,>>>,>>9.99":U
      Depr8 FORMAT ">>,>>>,>>9.99":U
      Depr9 FORMAT ">>,>>>,>>9.99":U
      Depr10 FORMAT ">>,>>>,>>9.99":U
      Depr11 FORMAT ">>,>>>,>>9.99":U
      Depr12 FORMAT ">>,>>>,>>9.99":U
      Depr13 FORMAT ">>,>>>,>>9.99":U
      Depr14 FORMAT ">>,>>>,>>9.99":U
      Depr15 FORMAT ">>,>>>,>>9.99":U
      Depr16 FORMAT ">>,>>>,>>9.99":U
      Depr17 FORMAT ">>,>>>,>>9.99":U
      Depr18 FORMAT ">>,>>>,>>9.99":U
      Depr19 FORMAT ">>,>>>,>>9.99":U
      Depr20 FORMAT ">>,>>>,>>9.99":U
      Depr21 FORMAT ">>,>>>,>>9.99":U
      Depr22 FORMAT ">>,>>>,>>9.99":U
      Depr23 FORMAT ">>,>>>,>>9.99":U
      Depr24 FORMAT ">>,>>>,>>9.99":U
      Depr25 FORMAT ">>,>>>,>>9.99":U
      Depr26 FORMAT ">>,>>>,>>9.99":U
      Depr27 FORMAT ">>,>>>,>>9.99":U
      Depr28 FORMAT ">>,>>>,>>9.99":U
      Depr29 FORMAT ">>,>>>,>>9.99":U
      Depr30 FORMAT ">>,>>>,>>9.99":U
      Depr31 FORMAT ">>,>>>,>>9.99":U
      Depr32 FORMAT ">>,>>>,>>9.99":U
      Depr33 FORMAT ">>,>>>,>>9.99":U
      Depr34 FORMAT ">>,>>>,>>9.99":U
      Depr35 FORMAT ">>,>>>,>>9.99":U
      Depr36 FORMAT ">>,>>>,>>9.99":U
      Depr37 FORMAT ">>,>>>,>>9.99":U
      Depr38 FORMAT ">>,>>>,>>9.99":U
      Depr39 FORMAT ">>,>>>,>>9.99":U
      Depr40 FORMAT ">>,>>>,>>9.99":U
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
   Data Source: "fa/sdodeptab.w"
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
     _FldNameList[1]   > _<SDO>.rowObject.method
"method" ? ? "character" ? ? ? ? ? ? no ? no no "8.43" yes no no "U" "" ""
     _FldNameList[2]   = _<SDO>.rowObject.Auto-sl
     _FldNameList[3]   > _<SDO>.rowObject.Ratio
"Ratio" ? ? "decimal" ? ? ? ? ? ? no ? no no "15.86" yes no no "U" "" ""
     _FldNameList[4]   = _<SDO>.rowObject.Conv
     _FldNameList[5]   > _<SDO>.rowObject.Manual
"Manual" ? ? "logical" ? ? ? ? ? ? no ? no no "9.43" yes no no "U" "" ""
     _FldNameList[6]   = _<SDO>.rowObject.soyd
     _FldNameList[7]   > _<SDO>.rowObject.Depr1
"Depr1" "Yr 1" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > _<SDO>.rowObject.Depr2
"Depr2" "Yr 2" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > _<SDO>.rowObject.Depr3
"Depr3" "Yr 3" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > _<SDO>.rowObject.Depr4
"Depr4" "Yr 4" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > _<SDO>.rowObject.Depr5
"Depr5" "Yr 5" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[12]   = _<SDO>.rowObject.Depr6
     _FldNameList[13]   = _<SDO>.rowObject.Depr7
     _FldNameList[14]   = _<SDO>.rowObject.Depr8
     _FldNameList[15]   = _<SDO>.rowObject.Depr9
     _FldNameList[16]   = _<SDO>.rowObject.Depr10
     _FldNameList[17]   = _<SDO>.rowObject.Depr11
     _FldNameList[18]   = _<SDO>.rowObject.Depr12
     _FldNameList[19]   = _<SDO>.rowObject.Depr13
     _FldNameList[20]   = _<SDO>.rowObject.Depr14
     _FldNameList[21]   = _<SDO>.rowObject.Depr15
     _FldNameList[22]   = _<SDO>.rowObject.Depr16
     _FldNameList[23]   = _<SDO>.rowObject.Depr17
     _FldNameList[24]   = _<SDO>.rowObject.Depr18
     _FldNameList[25]   = _<SDO>.rowObject.Depr19
     _FldNameList[26]   = _<SDO>.rowObject.Depr20
     _FldNameList[27]   = _<SDO>.rowObject.Depr21
     _FldNameList[28]   = _<SDO>.rowObject.Depr22
     _FldNameList[29]   = _<SDO>.rowObject.Depr23
     _FldNameList[30]   = _<SDO>.rowObject.Depr24
     _FldNameList[31]   = _<SDO>.rowObject.Depr25
     _FldNameList[32]   = _<SDO>.rowObject.Depr26
     _FldNameList[33]   = _<SDO>.rowObject.Depr27
     _FldNameList[34]   = _<SDO>.rowObject.Depr28
     _FldNameList[35]   = _<SDO>.rowObject.Depr29
     _FldNameList[36]   = _<SDO>.rowObject.Depr30
     _FldNameList[37]   = _<SDO>.rowObject.Depr31
     _FldNameList[38]   = _<SDO>.rowObject.Depr32
     _FldNameList[39]   = _<SDO>.rowObject.Depr33
     _FldNameList[40]   = _<SDO>.rowObject.Depr34
     _FldNameList[41]   = _<SDO>.rowObject.Depr35
     _FldNameList[42]   = _<SDO>.rowObject.Depr36
     _FldNameList[43]   = _<SDO>.rowObject.Depr37
     _FldNameList[44]   = _<SDO>.rowObject.Depr38
     _FldNameList[45]   = _<SDO>.rowObject.Depr39
     _FldNameList[46]   = _<SDO>.rowObject.Depr40
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

