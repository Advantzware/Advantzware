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
    File        : IServiceAdapterWithInvokeTask
    Purpose     : Interface to be implemented by ServiceAdapter implementations
                  that support the InvokeTask methods
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 08 18:46:22 CEST 2011
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

INTERFACE Consultingwerk.OERA.IServiceAdapterWithInvokeTask:  
  
    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class.                                                                        
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method       
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method 
        @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method 
        @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method 
        @param phDataset4 INPUT-OUTPUT DATASET-HANDLE The handle of the fourth dataset to be passed to the task method 
        @param phDataset5 INPUT-OUTPUT DATASET-HANDLE The handle of the fifth dataset to be passed to the task method 
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER, 
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset5,
                                   poParameter AS ISerializable,
                                   phContextDataset AS HANDLE) .

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class.                                                                        
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method       
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method 
        @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method 
        @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method 
        @param phDataset4 INPUT-OUTPUT DATASET-HANDLE The handle of the fourth dataset to be passed to the task method 
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER, 
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset4,
                                   poParameter AS ISerializable,
                                   phContextDataset AS HANDLE) .

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class.                                                                        
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method 
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method 
        @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method 
        @param phDataset3 INPUT-OUTPUT DATASET-HANDLE The handle of the third dataset to be passed to the task method 
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER, 
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset3,
                                   poParameter AS ISerializable,
                                   phContextDataset AS HANDLE) .

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class.                                                                        
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method  
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method 
        @param phDataset2 INPUT-OUTPUT DATASET-HANDLE The handle of the second dataset to be passed to the task method 
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER, 
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset2,
                                   poParameter AS ISerializable,
                                   phContextDataset AS HANDLE) .

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class.                                                                        
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method  
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset1 INPUT-OUTPUT DATASET-HANDLE The handle of the first dataset to be passed to the task method 
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER, 
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE phDataset1,
                                   poParameter AS ISerializable,
                                   phContextDataset AS HANDLE) .

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class.                                                                        
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InvokeTask (pcPartition AS CHARACTER, 
                                   pcTaskName AS CHARACTER,
                                   pcMethodName AS CHARACTER,
                                   poParameter AS ISerializable,
                                   phContextDataset AS HANDLE) .
  
END INTERFACE.