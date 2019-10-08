&ANALYZE-SUSPEND VERSION-NUMBER UIBV9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&SCOPED-DEFINE in IN FRAME {&FRAME-NAME}
&SCOPED-DEFINE inb IN BROWSE {&BROWSE-NAME} 
&SCOPED-DEFINE sv SCREEN-VALUE {&IN}
&SCOPED-DEFINE svb SCREEN-VALUE {&INB}
&SCOPED-DEFINE tp THIS-PROCEDURE
&SCOPED-DEFINE WOn lDummy = SESSION:set-wait-state ("general").
&SCOPED-DEFINE WOff lDummy = SESSION:set-wait-state ("").

DEF {1} {2} SHARED VAR cCompany AS CHAR NO-UNDO.
DEF {1} {2} SHARED VAR cCondition AS CHAR NO-UNDO.
DEF {1} {2} SHARED VAR cFileName AS CHAR NO-UNDO.
DEF {1} {2} SHARED VAR cInitVal AS CHAR EXTENT 250 NO-UNDO.
DEF {1} {2} SHARED VAR cKeyList AS CHAR NO-UNDO.
DEF {1} {2} SHARED VAR cLocation AS CHAR NO-UNDO.
DEF {1} {2} SHARED VAR hDialog AS HANDLE NO-UNDO.
DEF {1} {2} SHARED VAR hWindow AS HANDLE NO-UNDO.
DEF {1} {2} SHARED VAR iCurrPage AS INT NO-UNDO.
DEF {1} {2} SHARED VAR iLoopStat AS INT NO-UNDO.
DEF {1} {2} SHARED VAR rSaveRowid AS ROWID NO-UNDO.

DEF {2} SHARED VAR g_company AS CHAR NO-UNDO.
DEF {2} SHARED VAR g_loc AS CHAR NO-UNDO.
DEF {2} SHARED VAR g_sysdate AS DATE NO-UNDO.
DEF {2} SHARED VAR g_period AS INT NO-UNDO.
DEF {2} SHARED VAR g_init AS LOG NO-UNDO.
DEF {2} SHARED VAR g_batch AS LOG NO-UNDO.
DEF {2} SHARED VAR g_batch-rowid AS ROWID NO-UNDO.

DEF {2} SHARED VAR hBrowser AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hBrowser1 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hBrowser2 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hBrowser3 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hBrowser4 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hBrowser5 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hBrowser6 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hBrowser7 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hBrowser8 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hBrowser9 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hColumn AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hColumns AS HANDLE NO-UNDO EXTENT 40.
DEF {2} SHARED VAR hParent AS HANDLE NO-UNDO. 
DEF {2} SHARED VAR hPupdsave AS HANDLE NO-UNDO. 
DEF {2} SHARED VAR hSdo AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hSdo1 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hSdo2 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hSdo3 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hSdo4 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hSdo5 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hSdo6 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hSdo7 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hSdo8 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hSdo9 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hViewer AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hViewer1 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hViewer2 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hViewer3 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hViewer4 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hViewer5 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hViewer6 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hViewer7 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hViewer8 AS HANDLE NO-UNDO.
DEF {2} SHARED VAR hViewer9 AS HANDLE NO-UNDO.

DEF VAR cipvResolver AS CHAR NO-UNDO.
DEF VAR cLastSelection AS CHAR NO-UNDO.
DEF VAR cPgm AS CHAR NO-UNDO.
DEF VAR cRecordList AS CHAR NO-UNDO.
DEF VAR cResult AS CHAR NO-UNDO.
DEF VAR cUseIndex AS CHARACTER NO-UNDO.
DEF VAR cWhereClause AS CHARACTER NO-UNDO.
DEF VAR handleToBrowse AS HANDLE NO-UNDO.
DEF VAR hHelp AS HANDLE NO-UNDO.
DEF VAR hSave AS HANDLE NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR lAddNew AS LOG NO-UNDO.
DEF VAR lBegin AS LOG INIT YES NO-UNDO.
DEF VAR lDelete AS LOG NO-UNDO.
DEF VAR lDeleteOk AS LOG NO-UNDO.
DEF VAR lDoCopy AS LOG NO-UNDO.
DEF VAR lFirstTime AS LOG INIT YES NO-UNDO.
DEF VAR lModified AS LOG NO-UNDO.
DEF VAR lOK AS LOG NO-UNDO.
DEF VAR lSave AS LOG NO-UNDO.
DEF VAR lSortChange AS LOG NO-UNDO.
DEF VAR rOldRowid AS ROWID NO-UNDO.
DEF VAR rRowid AS ROWID NO-UNDO.

/* Set user, company, location */
FIND users NO-LOCK WHERE 
    users.user_id = USERID(LDBNAME(1)) 
    NO-ERROR.
FIND FIRST usercomp NO-LOCK WHERE 
    usercomp.user_id = USERID(LDBNAME(1)) AND
    usercomp.loc = '' AND
    usercomp.company_default = YES
    NO-ERROR.
ASSIGN 
    g_company = IF AVAIL usercomp THEN usercomp.company ELSE "001"
    cCompany = g_company.
FIND FIRST usercomp NO-LOCK WHERE 
    usercomp.user_id = USERID("NOSWEAT") AND
    usercomp.company = g_company AND
    usercomp.loc NE "" AND
    usercomp.loc_default = yes
    NO-ERROR.
ASSIGN 
    g_loc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN"
    cLocation = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


