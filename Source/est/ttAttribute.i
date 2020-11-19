
/*------------------------------------------------------------------------
    File        : ttAttribute.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Oct 29 14:21:14 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttAttribute NO-UNDO
    FIELD attributeID    AS INTEGER
    FIELD attributeName  AS CHARACTER
    FIELD attributeValue AS CHARACTER
    INDEX attributeID  IS PRIMARY attributeID ASCENDING
    .
    

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
