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
DEFINE VARIABLE colHand      AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE colHandList AS CHAR NO-UNDO.
DEFINE VARIABLE icnt        as integer no-undo. 

&global-define exclude-brw-custom true
&endif





