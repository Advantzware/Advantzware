
/*------------------------------------------------------------------------
    File        : ttArtiosAttributes.i
    Purpose     : Used in consumer as NEW SHARED and in builder as SHARED

    Syntax      :

    Description : Temp-table definition for Artios Attributes

    Author(s)   : BV
    Created     : Sun Jun 03 21:26:04 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} TEMP-TABLE ttArtiosAttributes
    FIELD CadFile AS CHARACTER
    FIELD Attribute AS CHARACTER
    FIELD AttributeType AS CHARACTER
    FIELD ValueImported AS CHARACTER
    FIELD ValueMapped AS CHARACTER
    FIELD LoadMessage AS CHARACTER
    .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
