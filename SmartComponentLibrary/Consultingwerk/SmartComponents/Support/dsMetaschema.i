/*------------------------------------------------------------------------
    File        : dsMetaschema.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 02 17:14:11 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{Consultingwerk/SmartComponents/Support/eDatabase.i}
{Consultingwerk/SmartComponents/Support/eFile.i}
{Consultingwerk/SmartComponents/Support/eField.i}

DEFINE DATASET dsMetaschema 
    FOR eDatabase, eFile, eField 
    DATA-RELATION fileRelation FOR eDatabase, eFile
        RELATION-FIELDS (DatabaseName, DatabaseName)
    DATA-RELATION fieldRelation FOR eFile, eField
        RELATION-FIELDS (DatabaseName, DatabaseName, FileName, FileName) 
        .