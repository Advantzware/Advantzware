/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IServiceAdapterWithInvokeTaskAsync
    Purpose     : Interface to be implemented by ServiceAdapter implementations
                  that support the InvokeTask methods
    Syntax      : 
    Description : See SCL-246
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 11 14:04:43 CET 2014
    Notes       : When async tasks are used, the client will need to provide 
                  valid dataset handles in order to receive data from the
                  task. Datasets will be serialized as XLM or JSON from 
                  the server and the data will be returned into the dataset
                  instances provided by the caller
                  The poContext object reference may be used to provide 
                  context information to be returned in the AsyncTaskCompleted
                  event 
                  Async task calls on the AppServer do receive the context 
                  dataset. However the context dataset is not returned by to 
                  the caller 
                  This interface is no longer required to use. As of SCL-1045 
                  the basic IServiceAdapter interface was enhanced to contain 
                  all ServiceAdapter functionality. As such, all methods of 
                  the ServiceAdapter class are directly accessible from the 
                  FrameworkSettings:ServiceAdapter reference withour the 
                  previous need to case to this interface.
  ----------------------------------------------------------------------*/

USING Consultingwerk.*      FROM PROPATH .  
USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IServiceAdapterWithInvokeTaskAsync:  
  
    /*------------------------------------------------------------------------------
        Purpose: Raised when an Async Task has completed                                                                     
        Notes:          
        @param sender The reference to the object that raised the event
        @param e The AsyncTaskCompletedEventArgs instance with the data for this event                                                                            
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AsyncTaskCompleted SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                           e AS AsyncTaskCompletedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class asynchronously
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset1 The handle of the first dataset to be passed to the task method
        @param phDataset2 The handle of the second dataset to be passed to the task method
        @param phDataset3 The handle of the third dataset to be passed to the task method
        @param phDataset4 The handle of the fourth dataset to be passed to the task method
        @param phDataset5 The handle of the fifth dataset to be passed to the task method
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call
        @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
        @return The handle of the async AppServer request
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          phDataset3 AS HANDLE,
                                          phDataset4 AS HANDLE,
                                          phDataset5 AS HANDLE,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object) .

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class asynchronously
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset1 The handle of the first dataset to be passed to the task method
        @param phDataset2 The handle of the second dataset to be passed to the task method
        @param phDataset3 The handle of the third dataset to be passed to the task method
        @param phDataset4 The handle of the fourth dataset to be passed to the task method
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call
        @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
        @return The handle of the async AppServer request
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          phDataset3 AS HANDLE,
                                          phDataset4 AS HANDLE,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object) .

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class asynchronously
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset1 The handle of the first dataset to be passed to the task method
        @param phDataset2 The handle of the second dataset to be passed to the task method
        @param phDataset3 The handle of the third dataset to be passed to the task method
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call
        @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
        @return The handle of the async AppServer request
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          phDataset3 AS HANDLE,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object) .

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class asynchronously
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset1 The handle of the first dataset to be passed to the task method
        @param phDataset2 The handle of the second dataset to be passed to the task method
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call
        @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
        @return The handle of the async AppServer request
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          phDataset2 AS HANDLE,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object) .

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class.
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param phDataset1 The handle of the first dataset to be passed to the task method
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call
        @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
        @return The handle of the async AppServer request
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          phDataset1 AS HANDLE,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object) .

    /*------------------------------------------------------------------------------
        Purpose: Invokes a method in a Task class.
        Notes:   When no MethodName is passed then "Invoke" will be executed as the
                 default task method
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcTaskName The name of the task class
        @param pcMethodName The name of the business entity method to invoke
        @param poParameter The serializable parameter object to the method
        @param phContextDataset The handle of the context dataset for this call
        @param poContext The reference to the context object to be returned with the AsyncTaskCompleted event
        @return The handle of the async AppServer request
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE InvokeTaskAsync (pcPartition AS CHARACTER,
                                          pcTaskName AS CHARACTER,
                                          pcMethodName AS CHARACTER,
                                          poParameter AS ISerializable,
                                          phContextDataset AS HANDLE,
                                          poContext AS Progress.Lang.Object) .

END INTERFACE.
