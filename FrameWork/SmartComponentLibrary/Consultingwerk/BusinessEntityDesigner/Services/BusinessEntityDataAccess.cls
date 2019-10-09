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
    File        : BusinessEntityDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 19:46:15 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.* FROM PROPATH .
USING Consultingwerk.OERA.*                   FROM PROPATH .
USING Progress.Lang.*                         FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDataAccess INHERITS DataAccess: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the &1 class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        /* NOOP */

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:                    
        @param dsBusinessEntity DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsBusinessEntity BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed                                                                      
        Notes:   Not used in this class
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Not used in this class
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetch data from the data source
        Notes:   When the Context property of the IFetchDataRequest object is "<NEW>" then a new business entity will be initialized 
        @param poFetchDataRequest The IFetchDataRequest object with the parameters for this call
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID FetchData (poFetchDataRequest AS Consultingwerk.OERA.IFetchDataRequest):

        IF poFetchDataRequest:Context > "":U THEN . 
            ELSE RETURN .  

        IF ENTRY (2, poFetchDataRequest:Context, CHR(1)) = "<NEW>":U THEN DO:
            DATASET dsBusinessEntity:EMPTY-DATASET () .
            
            CREATE eBusinessEntity .
            ASSIGN eBusinessEntity.BusinessEntityName            = "New":U
                   eBusinessEntity.DefaultTablePrefix            = BusinessEntityDesignerSettings:DefaultTablePrefix
                   eBusinessEntity.DefaultTableSuffix            = BusinessEntityDesignerSettings:DefaultTableSuffix
                   eBusinessEntity.DefaultBeforeTablePrefix      = BusinessEntityDesignerSettings:DefaultBeforeTablePrefix
                   eBusinessEntity.DefaultBeforeTableSuffix      = BusinessEntityDesignerSettings:DefaultBeforeTableSuffix
                   eBusinessEntity.DistinctTempTableIncludeFiles = BusinessEntityDesignerSettings:DistinctTempTableIncludeFiles
                   eBusinessEntity.GenerateDatasetController     = BusinessEntityDesignerSettings:GenerateDatasetController
                .
                
            RETURN . 
        END. 

        DATASET dsBusinessEntity:READ-XML ("FILE":U, ENTRY (2, poFetchDataRequest:Context, CHR (1)), "EMPTY":U, ?, ?) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose:  Save changes to physical storage                
       Notes:    Override to DataAccess base class                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID SaveChanges ():
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition) 
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of 
                 DataAccess:FetchData () query preparation                        
        @param pcTable The temp table name to return the source (database table) field name for
        @param pcColumn The temp table column name to return the source (database table) field name for             
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the base query string used to retrieve data for the temp
                 tables. This query string will be appended by the query provided by
                 the consumer (FetchDataRequest object).                                                                      
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of 
                 DataAccess:FetchData () query preparation                        
        @param pcTable The temp table name to return the source default (database) query string for
        @return The base query string used to retrieve data for the given temp-table
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTER):

    END METHOD.

END CLASS.