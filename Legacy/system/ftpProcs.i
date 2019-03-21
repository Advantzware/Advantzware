
/*------------------------------------------------------------------------
    File        : ftpProcs.i
    Purpose     : 

    Syntax      :

    Description : Defines temp tables for use with ftpProcs.p

    Author(s)   : 
    Created     : Wed Nov 14 14:42:15 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE ttConfig 
    FIELD importFormat AS CHAR
    FIELD destName     AS CHAR FORMAT "x(20)"
    FIELD ftp-site     AS CHAR FORMAT "x(30)"
    FIELD ftp-user     AS CHAR 
    FIELD ftp-passwd   AS CHAR FORMAT "x(12)"
    FIELD ftp-mode     AS CHAR 
    FIELD ftp-software AS CHAR
    FIELD ftp-dir      AS CHAR 
    FIELD ftp-binary   AS CHAR
    FIELD ftp-script   AS CHAR
    FIELD ftp-cmd      AS CHAR
    INDEX importFormat        importFormat
    INDEX destName IS PRIMARY destName.

DEFINE TEMP-TABLE ttScriptLines
    FIELD scriptLineNum  AS INTEGER 
    FIELD scriptLineText AS CHARACTER 
    INDEX i1 scriptLineNum.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
