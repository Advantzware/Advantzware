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

&if defined(exclude-brw-custom) = 0 &then
DEFINE VARIABLE hColumnRowColor  AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE cColHandList     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCnt             AS INTEGER NO-UNDO. 
DEFINE VARIABLE cTemp            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hTemp            AS HANDLE NO-UNDO. 
DEFINE VARIABLE hCurrentColumn   AS WIDGET-HANDLE NO-UNDO. 
DEFINE VARIABLE hPrevColumn      AS WIDGET-HANDLE NO-UNDO. 
DEFINE VARIABLE lsortBy          AS LOGICAL INITIAL TRUE NO-UNDO.

&global-define exclude-brw-custom true
&endif





