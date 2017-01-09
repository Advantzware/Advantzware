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
    File        : RepositoryServiceTask
    Purpose     : Server side proxy task to the IRepositoryService implementation
                  on the AppServer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 28 20:59:15 CET 2012
    Notes       : The AppServer needs to start an IRepositoryService implementation
                  during the start of the AppServer process. This task provides
                  an Interface to the IRepositoryService that is accessible 
                  from the client through the RepositoryServiceClient class
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.Framework.RepositoryServices.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .  

CLASS Consultingwerk.Framework.RepositoryServices.RepositoryServiceTask
    IMPLEMENTS Consultingwerk.OERA.IBusinessService: 

    /*------------------------------------------------------------------------------
        Purpose: Returns a list of class names that extend the given base class 
                 (including the given class)                                                                      
        Notes:  
        @param poParameter Am RepositoryTaskParameter object containing the parameter for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClassChildren (poParameter AS RepositoryTaskParameter):

        DEFINE VARIABLE oRepository AS IRepositoryService NO-UNDO . 
                
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poParameter """Parameter Object"":U"} .                
                
        ASSIGN oRepository = {Consultingwerk/get-service.i Consultingwerk.Framework.RepositoryServices.IRepositoryService} .        

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oRepository """IRepositoryService"":U"} . 

        poParameter:ClassChildren = oRepository:ClassChildren(poParameter:ObjectType) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the repository structure for objects of the given type                                                                     
        Notes: 
        @param phDataset INPUT-OUTPUT DATASET-HANDLE containing the an empty temp-table with a single field per object attribute     
        @param poParameter Am RepositoryTaskParameter object containing the parameter for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetEmptyObjectStructure (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                                poParameter AS RepositoryTaskParameter):
        
        DEFINE VARIABLE oRepository AS IRepositoryService NO-UNDO . 
                
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poParameter """Parameter Object"":U"} .               
                
        ASSIGN oRepository = {Consultingwerk/get-service.i Consultingwerk.Framework.RepositoryServices.IRepositoryService} .        

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oRepository """IRepositoryService"":U"} . 

        IF VALID-HANDLE (phDataset) THEN  
            DELETE OBJECT phDataset . 
            
        oRepository:GetEmptyObjectStructure (poParameter:ObjectType,
                                             OUTPUT DATASET-HANDLE phDataset) .

        IF VALID-HANDLE (phDataset) THEN  
            DELETE OBJECT phDataset . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the repository data for the named object                                                                     
        Notes:                                                                        
        @param phDataset INPUT-OUTPUT DATASET-HANDLE containing the repository definition of the given object     
        @param poParameter Am RepositoryTaskParameter object containing the parameter for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectByName (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                        poParameter AS RepositoryTaskParameter):
        
        DEFINE VARIABLE oRepository AS IRepositoryService NO-UNDO . 
                
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poParameter """Parameter Object"":U"} .               
                
        ASSIGN oRepository = {Consultingwerk/get-service.i Consultingwerk.Framework.RepositoryServices.IRepositoryService} .        

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oRepository """IRepositoryService"":U"} . 

        IF VALID-HANDLE (phDataset) THEN  
            DELETE OBJECT phDataset . 
            
        oRepository:GetObjectByName (poParameter:ObjectName,
                                     OUTPUT DATASET-HANDLE phDataset) .

        IF VALID-HANDLE (phDataset) THEN  
            DELETE OBJECT phDataset . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the repository data for the named object                                                                     
        Notes:                                                                        
        @param phDataset INPUT-OUTPUT DATASET-HANDLE containing the repository definition of the given object                                                                   
        @param poParameter Am RepositoryTaskParameter object containing the parameter for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreObject (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                    poParameter AS RepositoryTaskParameter):

        DEFINE VARIABLE oRepository AS IRepositoryService NO-UNDO . 
                
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poParameter """Parameter Object"":U"} .               
                
        ASSIGN oRepository = {Consultingwerk/get-service.i Consultingwerk.Framework.RepositoryServices.IRepositoryService} .        

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oRepository """IRepositoryService"":U"} . 

        oRepository:StoreObject (poParameter:New,
                                 DATASET-HANDLE phDataset) .

        IF VALID-HANDLE (phDataset) THEN  
            DELETE OBJECT phDataset . 

    END METHOD.

END CLASS.