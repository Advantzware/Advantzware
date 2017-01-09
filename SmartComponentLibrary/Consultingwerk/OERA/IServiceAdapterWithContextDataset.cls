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
    File        : IServiceAdapterWithContextDataset
    Purpose     : Interface to be implemented by ServiceAdapter implementations
                  that support a custom context dataset
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 16 11:57:41 CEST 2011
    Notes       : This interface is no longer required to use. As of SCL-1045 
                  the basic IServiceAdapter interface was enhanced to contain 
                  all ServiceAdapter functionality. As such, all methods of 
                  the ServiceAdapter class are directly accessible from the 
                  FrameworkSettings:ServiceAdapter reference withour the 
                  previous need to case to this interface.
  ----------------------------------------------------------------------*/

USING Consultingwerk.* .
USING Consultingwerk.OERA.* .
USING Progress.Lang.*.

INTERFACE Consultingwerk.OERA.IServiceAdapterWithContextDataset:  
  
    /*------------------------------------------------------------------------------
        Purpose: Invokes a custom method of the BusinessEntity.                                                                     
        Notes:     
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcEntityName The name of the business entity
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset INPUT-OUTPUT DATASET-HANDLE The handle of the dataset of the business entity, optionally INPUT, optionally OUTPUT
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InvokeMethod (pcPartition      AS CHARACTER, 
                                     pcEntityName     AS CHARACTER,
                                     pcMethodName     AS CHARACTER,
                                     INPUT-OUTPUT DATASET-HANDLE phDataset,
                                     poParameter      AS ISerializable,
                                     phContextDataset AS HANDLE) .
    
    /*------------------------------------------------------------------------------
        Purpose: Reads data from the Backend                                                                      
        Notes:   Request Parameters are send using the IFetchDataRequest object    
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcEntityName The name of the business entity
        @param poFetchDataRequest The IFetchDataRequest object with the data for this request
        @param phDataset OUTPUT DATASET-HANDLE The resulting dataset
        @param phContextDataset The handle of the context dataset for this call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RetrieveData (pcPartition        AS CHARACTER,
                                     pcEntityName       AS CHARACTER,
                                     poFetchDataRequest AS IFetchDataRequest,  
                                     OUTPUT DATASET-HANDLE phDataSet,
                                     phContextDataset   AS HANDLE) . 
                                       
    /*------------------------------------------------------------------------------
        Purpose: Saves modified (Create, Update, Delete) data to a Business Entity                                                                        
        Notes:   It's recommended but not required to just pass a changes dataset         
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcEntityName The name of the business entity
        @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
        @param pcContext INPUT-OUTPUT The context to be passed to the business entity
        @param phContextDataset The handle of the context dataset for this call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SubmitData (pcPartition                 AS CHARACTER,
                                   pcEntityName                AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset, 
                                   INPUT-OUTPUT pcContext      AS CHARACTER,
                                   phContextDataset            AS HANDLE) .                                        

    /*------------------------------------------------------------------------------
        Purpose: Returns an Empty Dataset (schema) to the client.                                                                       
        Notes:   The caller of this method is responsible to clean up the Dataset
                 when no longer used! The called should always invoke this method
                 with the dataset output parameter BY-REFERENCE!        
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcEntityName The name of the business entity
        @param phDataset OUTPUT DATASET-HANDLE The empty dataset
        @param phContextDataset The handle of the context dataset for this call
    ------------------------------------------------------------------------------*/                                   
    METHOD PUBLIC VOID FetchDataset (pcPartition           AS CHARACTER, 
                                     pcEntityName          AS CHARACTER, 
                                     OUTPUT DATASET-HANDLE phDataset,
                                     phContextDataset      AS HANDLE) .                                        

END INTERFACE.