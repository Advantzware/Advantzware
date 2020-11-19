&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : globaldef.i
    Purpose     : Global definationtions

    Syntax      : {methods/template/globaldef.i}

    Description : 


    Author(s)   : Anjly
    Created     : 11/18/20
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&if defined(exclude-globaldef) = 0 &then 

DEFINE VARIABLE deOrigWinWidth   AS DECIMAL NO-UNDO.

&global-define exclude-globaldef true
&endif





