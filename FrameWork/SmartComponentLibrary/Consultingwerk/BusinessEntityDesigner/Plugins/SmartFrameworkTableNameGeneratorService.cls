/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : SmartFrameworkTableNameGeneratorService
    Purpose     : This ITableNameGeneratorService implementation is based
                  in the SmartDB.SmartTable.TempTableName field. When the
                  database table is not existing in the SmartDB or the field
                  is not set, the database table name is returned.
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 25 22:12:12 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                  FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.Plugins.*   FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Services.*  FROM PROPATH .
USING Consultingwerk.Exceptions.*                       FROM PROPATH .
USING Consultingwerk.Framework.*                        FROM PROPATH .  
USING Consultingwerk.OERA.*                             FROM PROPATH . 
USING Progress.Lang.*                                   FROM PROPATH .

{ Consultingwerk/products.i }

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.SmartFrameworkTableNameGeneratorService 
    IMPLEMENTS ITableNameGeneratorService: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }  
&IF DEFINED (SmartFramework) NE 0 &THEN         
    { Consultingwerk/SmartFramework/System/dsTable.i }
&ENDIF  
    /*------------------------------------------------------------------------------
        Purpose: Generates a name for a temp-table
        Notes:   
        @param pcDatabaseName The name of the database table
        @param pcTableName The name of the database table
        @param dsBusinessEntity The Business Entity Designer Dataset
        @return The name for the new temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateTempTableName (pcDatabaseName AS CHARACTER, 
                                                   pcTableName AS CHARACTER, 
                                                   DATASET FOR dsBusinessEntity):
        
&IF DEFINED (SmartFramework) NE 0 &THEN         
        DEFINE VARIABLE oFetchDataRequest AS IFetchDataRequest NO-UNDO .
        DEFINE VARIABLE cQueryString      AS CHARACTER         NO-UNDO .

        ASSIGN cQueryString = SUBSTITUTE ("FOR EACH eSmartTable WHERE eSmartTable.DatabaseName = &1 AND eSmartTable.TableName = &2":U,
                                          QUOTER (pcDatabaseName), 
                                          QUOTER (pcTableName)) . 

        oFetchDataRequest = NEW FetchDataRequest ("eSmartTable":U,
                                                  cQueryString,
                                                  1) .

        FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                       "Consultingwerk.SmartFramework.System.TableBusinessEntity":U,
                                                       oFetchDataRequest,
                                                       OUTPUT DATASET dsTable) .

        FIND FIRST eSmartTable NO-ERROR . 
        
        IF NOT AVAILABLE eSmartTable THEN DO:
            RETURN (NEW TableNameGeneratorService ()):GenerateTempTableName (pcDatabaseName, 
                                                                             pcTableName,
                                                                             DATASET dsBusinessEntity BY-REFERENCE) .
        END.
        
        IF CharacterType:IsNotNullOrEmpty (eSmartTable.TempTableName) THEN
            RETURN eSmartTable.TempTableName .
        ELSE 
            RETURN pcTableName .  
        
&ELSE
        UNDO, THROW NEW NotSupportedException ("GenerateTempTableName":U,
                                               "Consultingwerk.BusinessEntityDesigner.Plugins.SmartFrameworkTableNameGeneratorService":U,
                                               "The use of the SmartFrameworkTableNameGeneratorService requires the SmartFramework") . 
&ENDIF

&IF DEFINED (SmartFramework) NE 0 &THEN         
        FINALLY:
            DATASET dsTable:EMPTY-DATASET () .        
        END FINALLY.
&ENDIF
    END METHOD .

END CLASS.
