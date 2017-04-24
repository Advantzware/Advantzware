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
    File        : IRequestAuthorizationProvider
    Purpose     : Provides Authorization for Requests at the Service 
                  Interface level
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 24 18:40:09 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH . 
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.Framework.Authorization.IRequestAuthorizationProvider:  

    /*------------------------------------------------------------------------------
        Purpose: Provides Authorization for FetchData Requests                                                                       
        Notes:                                                  
        @param pcEntityName The name of the Business Entity
        @param poRequest The IFetchDataRequest instance with the parameter for the FetchData request   
        @return Logical value indicating if the FetchData request is restricted                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsFetchDataRestricted (pcEntityName AS CHARACTER,
                                                 poRequest AS IFetchDataRequest) .

    /*------------------------------------------------------------------------------
        Purpose: Provides Authorization for InvokeMethod Requests                                                                    
        Notes:                                                  
        @param pcEntityName The name of the Business Entity
        @param pcMethodName The name of the method to be executed
        @param phDataset The Handle of the Dataset received
        @param poParameter The reference to the parameter object
        @return Logical value indicating if the InvokeMethod request is restricted                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsInvokeMethodRestricted (pcEntityName AS CHARACTER,
                                                    pcMethodName AS CHARACTER,
                                                    phDataset AS HANDLE,
                                                    poParameter AS Progress.Lang.Object) . 
    
    /*------------------------------------------------------------------------------
        Purpose: Provides Authorization for InvokeTask Requests                                                                    
        Notes:                                                  
        @param pcTaskName The name of the Business Task
        @param pcMethodName The name of the method to be executed
        @param piNumDatasets The count of the datasets passed to the InvokeTask Method
        @param phDataset1 The Handle of the Dataset received
        @param phDataset2 The Handle of the Dataset received
        @param phDataset3 The Handle of the Dataset received
        @param phDataset4 The Handle of the Dataset received
        @param phDataset5 The Handle of the Dataset received
        @param poParameter The reference to the parameter object
        @return Logical value indicating if the InvokeTask request is restricted                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsInvokeTaskRestricted (pcTaskName AS CHARACTER, 
                                                  pcMethodName AS CHARACTER, 
                                                  piNumDatasets AS INTEGER, 
                                                  phDataset1 AS HANDLE, 
                                                  phDataset2 AS HANDLE, 
                                                  phDataset3 AS HANDLE, 
                                                  phDataset4 AS HANDLE, 
                                                  phDataset5 AS HANDLE, 
                                                  poParameter AS Progress.Lang.Object):
        
    /*------------------------------------------------------------------------------
        Purpose: Provides Authorization for SaveChanges Requests                                                                    
        Notes:                                                  
        @param pcEntityName The name of the Business Entity
        @param phDataset The Handle of the Dataset received
        @return Logical value indicating if the Save Changes request is restricted                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsSaveChangesRestricted (pcEntityName AS CHARACTER, 
                                                   phDataset AS HANDLE) . 

END INTERFACE.
