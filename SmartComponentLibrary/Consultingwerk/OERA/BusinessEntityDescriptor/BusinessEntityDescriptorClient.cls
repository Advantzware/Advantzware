/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : BusinessEntityDescriptorClient
    Purpose     : Serivce which provides caching services for the Business   
                  Entity Descriptor. Used by ABL Clients only
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 12 16:16:26 CEST 2015
    Notes       : Code executed on the backend is not supposed to use this
                  caching service, as the Business Entity itself uses a 
                  cache (in the IBusinessEntityDescriptorService) to 
                  retrieve the Business Entity Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .  
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .
USING Consultingwerk.Framework.* FROM PROPATH.

CLASS Consultingwerk.OERA.BusinessEntityDescriptor.BusinessEntityDescriptorClient 
    IMPLEMENTS IBusinessEntityDescriptorClient: 

    DEFINE TEMP-TABLE ttBusinessEntityDescriptor NO-UNDO 
        FIELD BusinessEntityName AS CHARACTER 
        FIELD BusinessEntityDescriptor AS Progress.Lang.Object  
        INDEX BusinessEntityName IS UNIQUE PRIMARY BusinessEntityName . 

    /*------------------------------------------------------------------------------
        Purpose: Clears the Cache 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearCache ():

        EMPTY TEMP-TABLE ttBusinessEntityDescriptor .
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Business Entity Descriptor of a Business Entity 
        Notes:   
        @param pcBusinessEntityName The name of the Business Entity to return the Business Entity Descriptor for
        @return The Business Entity Descriptor of the given Business Entity
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IBusinessEntityDescriptor GetBusinessEntityDescriptor (pcBusinessEntityName AS CHARACTER):

        DEFINE VARIABLE oBusinessEntityDescriptor AS IBusinessEntityDescriptor NO-UNDO .  

        FIND ttBusinessEntityDescriptor WHERE ttBusinessEntityDescriptor.BusinessEntityName = pcBusinessEntityName
            NO-ERROR .

        IF AVAILABLE ttBusinessEntityDescriptor THEN DO:
            IF VALID-OBJECT (ttBusinessEntityDescriptor.BusinessEntityDescriptor) THEN 
                RETURN CAST (ttBusinessEntityDescriptor.BusinessEntityDescriptor, IBusinessEntityDescriptor) .
        END.

        ASSIGN oBusinessEntityDescriptor = THIS-OBJECT:RetrieveBusinessEntityDescriptor (pcBusinessEntityName) .

        IF NOT AVAILABLE ttBusinessEntityDescriptor THEN
            CREATE ttBusinessEntityDescriptor . 

        ASSIGN ttBusinessEntityDescriptor.BusinessEntityName       = pcBusinessEntityName 
               ttBusinessEntityDescriptor.BusinessEntityDescriptor = oBusinessEntityDescriptor . 

        RETURN oBusinessEntityDescriptor .
    
        FINALLY:
            ErrorHelper:ResetErrorStatus() .        
        END FINALLY.
    

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves a Business Entity Descriptor from the Backend
        Notes:   
        @param pcBusinessEntityName The name of the Business Entity to return the Business Entity Descriptor for
        @return The Business Entity Descriptor of the given Business Entity
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED IBusinessEntityDescriptor RetrieveBusinessEntityDescriptor (pcBusinessEntityName AS CHARACTER):
		
        DEFINE VARIABLE hDataset   AS HANDLE                               NO-UNDO.
        DEFINE VARIABLE oParameter AS GetBusinessEntityDescriptorParameter NO-UNDO . 
		
        oParameter = NEW GetBusinessEntityDescriptorParameter (FALSE) . 

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       pcBusinessEntityName,
                                                       "GetBusinessEntityDescriptor":U,
                                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                       oParameter) .

        IF VALID-HANDLE (hDataset) THEN 
            DELETE OBJECT hDataset . 

        RETURN oParameter:Descriptor . 

	END METHOD .

END CLASS.
