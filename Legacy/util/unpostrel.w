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
&Scoped-Define ENABLED-OBJECTS fiCompany fiOrder btGetInfo btFix 
&Scoped-Define DISPLAYED-OBJECTS fiCompany fiOrder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btFix 
     LABEL "Fix Release Status" 
     SIZE 28 BY 1.14.

DEFINE BUTTON btGetInfo 
     LABEL "Show Release Info" 
     SIZE 33 BY 1.14.

DEFINE VARIABLE fiCompany AS CHARACTER FORMAT "X(256)":U 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiOrder AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Release #" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiCompany AT ROW 1.71 COL 12 COLON-ALIGNED WIDGET-ID 2
     fiOrder AT ROW 3.14 COL 12 COLON-ALIGNED WIDGET-ID 4
     btGetInfo AT ROW 3.14 COL 34 WIDGET-ID 8
     btFix AT ROW 23.38 COL 9 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 158.2 BY 24.62 WIDGET-ID 100.


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
         TITLE              = "Fix Release Status (Posting ran but BOLs not created)"
         HEIGHT             = 24.62
         WIDTH              = 158.2
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 158.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 158.2
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartWindowCues" wWin _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartWindow,ab,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
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
ON END-ERROR OF wWin /* Fix Release Status (Posting ran but BOLs not created) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Fix Release Status (Posting ran but BOLs not created) */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFix wWin
ON CHOOSE OF btFix IN FRAME fMain /* Fix Release Status */
DO:
  DEF VAR cnt AS INT NO-UNDO.
  DEF VAR v-num-found AS INT NO-UNDO.
  v-num-found = 0.

  for each oe-rell where oe-rell.company = fiCompany:SCREEN-VALUE IN FRAME fMain
    and oe-rell.r-no = INTEGER(fiOrder:SCREEN-VALUE IN FRAME fMain)  no-lock.

    find first oe-relh where oe-relh.r-no = oe-rell.r-no no-error.
    find first oe-boll where oe-boll.company = oe-rell.company
     and oe-boll.ord-no = oe-rell.ord-no
     and oe-boll.rel-no = oe-rell.rel-no
     no-lock no-error.

    find first oe-rel where oe-rel.r-no = oe-rell.link-no no-lock no-error.

    IF AVAIL oe-boll THEN
        v-num-found = v-num-found + 1.

    find oe-ordl where oe-ordl.company = oe-rell.company
      and oe-ordl.ord-no = oe-rell.ord-no
      and oe-ordl.line = oe-rell.line no-lock no-error.
     
  END.

  for each oe-rell where oe-rell.company = fiCompany:SCREEN-VALUE IN FRAME fMain
    and oe-rell.r-no = INTEGER(fiOrder:SCREEN-VALUE IN FRAME fMain)  no-lock.

    find first oe-relh where oe-relh.r-no = oe-rell.r-no no-error.
    find first oe-boll where oe-boll.company = oe-rell.company
     and oe-boll.ord-no = oe-rell.ord-no
     and oe-boll.rel-no = oe-rell.rel-no
     no-lock no-error.

    find first oe-rel where oe-rel.r-no = oe-rell.link-no no-lock no-error.
    IF NOT AVAIL oe-boll THEN
      oe-relh.posted = no.

    find oe-ordl where oe-ordl.company = oe-rell.company
      and oe-ordl.ord-no = oe-rell.ord-no
      and oe-ordl.line = oe-rell.line no-lock no-error.      
    IF NOT AVAIL oe-rel THEN
     run c:\temp\asi\oe\cleanrel.p (input rowid(oe-ordl)).
     
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGetInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGetInfo wWin
ON CHOOSE OF btGetInfo IN FRAME fMain /* Show Release Info */
DO:
  RUN populate-browse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}
    

/* Procedure h-TestDynBrowse.p -- test dynamic temp-table
   methods. */
DEFINE VARIABLE hTT    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTTBuf AS HANDLE     NO-UNDO.
DEFINE VARIABLE hQuery AS HANDLE     NO-UNDO.
DEFINE VARIABLE iSeq   AS INTEGER    NO-UNDO INIT 1000.
DEFINE VARIABLE cName  LIKE oe-ordl.cust       NO-UNDO.
DEFINE VARIABLE iCNum  LIKE oe-ordl.ord-no    NO-UNDO.
DEFINE VARIABLE cRep   AS CHAR    NO-UNDO.
DEFINE VARIABLE hBrowse AS HANDLE     NO-UNDO.
DEFINE VARIABLE hColumn AS HANDLE     NO-UNDO.
DEFINE FRAME BrowseFrame WITH SIZE 100 BY 10.

CREATE TEMP-TABLE hTT.
hTT:CREATE-LIKE("oe-rell","ord-no").
hTT:ADD-FIELDS-FROM("oe-rell","posted").
hTT:ADD-FIELDS-FROM("oe-rell","printed").
hTT:ADD-FIELDS-FROM("oe-rell","rel-no").
hTT:ADD-FIELDS-FROM("oe-rell","r-no").
hTT:ADD-NEW-FIELD("release#", "INTEGER",0,"999999",0,"Rel","Rel#").
hTT:ADD-NEW-FIELD("avail-bol", "LOGICAL",0,"YES/NO",NO,"Avil Bol","Avail!BOL").
hTT:ADD-NEW-FIELD("avail-rel", "LOGICAL",0,"YES/NO",NO,"Avil Rel","Avail!Rel").
hTT:ADD-NEW-FIELD("avail-relh", "LOGICAL",0,"YES/NO",NO,"Avil Relh","Avail!Relh").
hTT:ADD-NEW-FIELD("Sequence", "INTEGER",0,"9999",1000).
/* hTT:ADD-LIKE-INDEX("CustNum","CustNum","Customer"). */
hTT:ADD-NEW-INDEX("SeqIndex", YES).  /* Yes, make it the primary index. */
hTT:ADD-INDEX-FIELD("SeqIndex", "Sequence").
hTT:TEMP-TABLE-PREPARE("CustSequence").
hTTBuf = hTT:DEFAULT-BUFFER-HANDLE.

/* Populate the temp-table with values from the database. */
/*
FOR EACH Customer WHERE State = "NH", SalesRep OF Customer:
    hTTBuf:BUFFER-CREATE().
    hTTBuf:BUFFER-COPY(BUFFER Customer:HANDLE).
    hTTBuf:BUFFER-COPY(BUFFER SalesRep:HANDLE).
    hTTBuf:BUFFER-FIELD("Sequence"):BUFFER-VALUE = iSeq.
    iSeq = iSeq + 1.
END.
  */
/*
RUN CREATE-tt.
/*
FOR EACH oe-rell WHERE oe-rell.company = '001' 
     AND oe-rell.r-no = 5637.
    hTTBuf:BUFFER-CREATE().
    hTTBuf:BUFFER-COPY(BUFFER oe-rell:HANDLE).    
    hTTBuf:BUFFER-FIELD("Sequence"):BUFFER-VALUE = iSeq.
    iSeq = iSeq + 1.
END.
*/
/* Now create a query for the temp-table buffer and display values. */
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hTTBuf).
hQuery:QUERY-PREPARE("FOR EACH CustSequence").

CREATE BROWSE hBrowse
    ASSIGN ROW = 5 COL = 1
           WIDTH = 79 DOWN = 15
           FRAME = FRAME fMain:HANDLE
           QUERY = hQuery
           SENSITIVE = YES 
           SEPARATORS = YES
           ROW-MARKERS = NO
           VISIBLE = YES
    TRIGGERS:

    END.

/* hBrowse:ADD-COLUMNS-FROM(hTTBuf, "SalesRep,Country,address,Address2,State").
  */
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("ord-no")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("r-no")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("Sequence")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("Printed")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("Posted")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("rel-no")).
/* hTTBuf:BUFFER-FIELD("SalesRep"):VALIDATE-EXPRESSION = "".           */
/* hColumn = hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("SalesRep")). */
/* hColumn = hBrowse:ADD-CALC-COLUMN("DECIMAL","ZZ,ZZZ,ZZ9.99",0,"Available",4). */

hQuery:QUERY-OPEN().
ENABLE ALL WITH FRAME fMain.
/* WAIT-FOR CLOSE OF CURRENT-WINDOW. */


*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt wWin 
PROCEDURE create-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR iSeq AS INT INIT 1.

for each oe-rell where oe-rell.company = fiCompany:SCREEN-VALUE IN FRAME fMain
    and oe-rell.r-no = INTEGER(fiOrder:SCREEN-VALUE IN FRAME fMain)  no-lock.

    find first oe-relh where oe-relh.r-no = oe-rell.r-no no-error.
    find first oe-boll where oe-boll.company = oe-rell.company
     and oe-boll.ord-no = oe-rell.ord-no
     and oe-boll.rel-no = oe-rell.rel-no
     no-lock no-error.

    find first oe-rel where oe-rel.r-no = oe-rell.link-no no-lock no-error.

    /* oe-relh.posted = no. */

    find oe-ordl where oe-ordl.company = oe-rell.company
      and oe-ordl.ord-no = oe-rell.ord-no
      and oe-ordl.line = oe-rell.line no-lock no-error.
      
      hTTBuf:BUFFER-CREATE().
      hTTBuf:BUFFER-COPY(BUFFER oe-rell:HANDLE).    
      hTTBuf:BUFFER-FIELD("avail-bol"):BUFFER-VALUE = AVAIL(oe-boll).
      hTTBuf:BUFFER-FIELD("avail-rel"):BUFFER-VALUE = AVAIL(oe-rel).
      hTTBuf:BUFFER-FIELD("avail-relh"):BUFFER-VALUE = AVAIL(oe-relh).
      IF AVAIL(oe-relh) THEN
          hTTBuf:BUFFER-FIELD("release#"):BUFFER-VALUE = oe-relh.release#.
      hTTBuf:BUFFER-FIELD("Sequence"):BUFFER-VALUE = iSeq.
      iSeq = iSeq + 1.

      /*
     run c:\temp\asi\oe\cleanrel.p (input rowid(oe-ordl)). */
     
END.

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
  DISPLAY fiCompany fiOrder 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fiCompany fiOrder btGetInfo btFix 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populate-browse wWin 
PROCEDURE populate-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN CREATE-tt.
/*
FOR EACH oe-rell WHERE oe-rell.company = '001' 
     AND oe-rell.r-no = 5637.
    hTTBuf:BUFFER-CREATE().
    hTTBuf:BUFFER-COPY(BUFFER oe-rell:HANDLE).    
    hTTBuf:BUFFER-FIELD("Sequence"):BUFFER-VALUE = iSeq.
    iSeq = iSeq + 1.
END.
*/
/* Now create a query for the temp-table buffer and display values. */
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hTTBuf).
hQuery:QUERY-PREPARE("FOR EACH CustSequence").

CREATE BROWSE hBrowse
    ASSIGN ROW = 5 COL = 1
           WIDTH = 79 DOWN = 15
           FRAME = FRAME fMain:HANDLE
           QUERY = hQuery
           SENSITIVE = YES 
           SEPARATORS = YES
           ROW-MARKERS = NO
           VISIBLE = YES
    TRIGGERS:

    END.

/* hBrowse:ADD-COLUMNS-FROM(hTTBuf, "SalesRep,Country,address,Address2,State").
  */
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("ord-no")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("r-no")).

hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("Printed")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("Posted")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("avail-bol")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("avail-rel")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("avail-relh")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("rel-no")).
hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("Sequence")).
/* hTTBuf:BUFFER-FIELD("SalesRep"):VALIDATE-EXPRESSION = "".           */
/* hColumn = hBrowse:ADD-LIKE-COLUMN(hTTBuf:BUFFER-FIELD("SalesRep")). */
/* hColumn = hBrowse:ADD-CALC-COLUMN("DECIMAL","ZZ,ZZZ,ZZ9.99",0,"Available",4). */

hQuery:QUERY-OPEN().
/* DISABLE ALL WITH FRAME fMain. */
ENABLE ALL WITH FRAME fMain.
/* WAIT-FOR CLOSE OF CURRENT-WINDOW. */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

