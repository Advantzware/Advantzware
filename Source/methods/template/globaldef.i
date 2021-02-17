&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : browsercustomdef.i
    Purpose     : Browser customization based on New Designes

    Syntax      : {methods/template/brwcustomdef.i}

    Description : Browser customization defination as per new enhanced UI 


    Author(s)   : Anjly
    Created     : 08/10/20
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&if defined(exclude-globaldef) = 0 &then 

DEFINE VARIABLE deOrigWinWidth      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deOrigWinHeight     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deDeltaWidth        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deDeltaHeight       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dePrevDeltaWidth    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dePrevDeltaHeight   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cPageVisited        AS CHARACTER NO-UNDO.
DEFINE VARIABLE widresizedlise      AS CHARACTER NO-UNDO.
DEFINE VARIABLE hTempObjHand        AS HANDLE    NO-UNDO.
DEFINE VARIABLE pgno                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lastBtnPos          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deTempColPos        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deResizeVal         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deDeltaWidthchange  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deDeltaHeightchange AS DECIMAL   NO-UNDO.
DEFINE VARIABLE hTempWinmn          AS HANDLE    NO-UNDO.
DEFINE VARIABLE cSmartObjList       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCntWidHand         AS INTEGER   NO-UNDO.
DEFINE VARIABLE deRowPos            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deColPos            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deWidth             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deHeight            AS DECIMAL   NO-UNDO.

DEFINE VARIABLE iWinState AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE pagewinsize 
    FIELDS pageNo    AS CHARACTER 
    FIELDS WinWidth  AS DECIMAL
    FIELDS winHeight AS DECIMAL.

DEFINE TEMP-TABLE toreposition 
    FIELDS widhand    AS CHARACTER 
    FIELDS widType    AS CHARACTER
    FIELDS colpos     AS DECIMAL
    FIELDS rowpos     AS DECIMAL  
    FIELDS widwidth   AS DECIMAL
    FIELDS widheight  AS DECIMAL
    FIELDS resizepage AS CHARACTER.
    
&global-define exclude-globaldef true
&endif





