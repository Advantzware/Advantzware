
/*------------------------------------------------------------------------
    File        : ttEstCopySheet.i
    Purpose     : 

    Syntax      :

    Description : Temp Tables to hold info of CopyFrom Blank and CopyInto Blanks

    Author(s)   : Vivek
    Created     : 05/24/2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttBlanksToCopy 
    FIELD riEb AS ROWID 
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER
    FIELD cPartNo AS CHARACTER FORMAT "X(15)"
    FIELD cPartDescription AS CHARACTER FORMAT "X(30)"
    FIELD lIsSelected AS LOGICAL
    . 

DEFINE TEMP-TABLE ttBlanksToCopyInto
    FIELD riEb AS ROWID
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER
    FIELD cPartNo AS CHARACTER FORMAT "X(15)"
    FIELD cPartDescription AS CHARACTER FORMAT "X(30)"
    FIELD lIsSelected AS LOGICAL
    FIELD lCopyDie AS LOGICAL 
    FIELD lCopyCad AS LOGICAL 
    FIELD lCopyOtherAttributes AS LOGICAL 
    FIELD lDoReCalculation AS LOGICAL
    .    
        
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
