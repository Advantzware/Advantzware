
/*------------------------------------------------------------------------
    File        : ttTag.i
    Purpose     : 

    Syntax      :

    Description : Holds Temp-table Tag DB table

    Author(s)   : AC
    Created     : 04-28-2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE {&Table-Name} NO-UNDO
    FIELD rec_key               AS CHARACTER            LABEL "Record Key"     FORMAT "x(21)"
    FIELD linkTable             AS CHARACTER            LABEL "Link Table"     FORMAT "x(24)"
    FIELD linkRecKey            AS CHARACTER            LABEL "Link RecKey"    FORMAT "x(21)"
    FIELD tagType               AS CHARACTER            LABEL "Type"           FORMAT "x(8)"
    FIELD description           AS CHARACTER            LABEL "Description"    FORMAT "x(60)"
    FIELD Note                  AS CHARACTER EXTENT 5   LABEL "Note"           FORMAT "x(60)"
    FIELD createUser            AS CHARACTER            LABEL "Create User"    FORMAT "x(12)"
    FIELD createDT              AS DATETIME             LABEL "Create DtTm"    FORMAT "99/99/9999 HH:MM:SS.SSS"
    FIELD updateUser            AS CHARACTER            LABEL "Update User"    FORMAT "x(12)"
    FIELD updateDT              AS DATETIME             LABEL "Update DtTm"    FORMAT "99/99/9999 HH:MM:SS.SSS"
    FIELD ownerUser             AS CHARACTER            LABEL "Owner"          FORMAT "x(12)"
    FIELD statusCode            AS CHARACTER            LABEL "Status"         FORMAT "x(8)"
    FIELD groupCode             AS CHARACTER            LABEL "Group"          FORMAT "x(8)"
    . 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
