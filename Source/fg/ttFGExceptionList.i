
/*------------------------------------------------------------------------
    File        : ttFGExceptionList.i
    Purpose     : 

    Syntax      :

    Description : Holds temp-table definition for ttFGExceptionList

    Author(s)   : Sewa Singh
    Created     : Fri Nov 27 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFGExceptionList 
    FIELD cCompany    AS CHARACTER LABEL "Company"   
    FIELD cItem       AS CHARACTER LABEL "FG Item"
    FIELD cDesc       AS CHARACTER LABEL "Item Name"
    FIELD cTag        AS CHARACTER LABEL "Tag"
    FIELD dtDate      AS DATE      LABEL "Rec Date"
    FIELD cUser       AS CHARACTER LABEL "User"
    FIELD cRitaCode   AS CHARACTER LABEL "Rita Code"
    FIELD cReason     AS CHARACTER LABEL "Reason".
   
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
