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
    File        : ServiceInterfaceLogger
    Purpose     : Logs requests to the ServiceInface 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 14 13:41:41 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.*     FROM PROPATH . 
USING Consultingwerk.Util.*     FROM PROPATH . 
USING Progress.Lang.*           FROM PROPATH .

CLASS Consultingwerk.OERA.ServiceInterfaceLogger: 

    /*------------------------------------------------------------------------------
        Purpose: Logger method for the FetchData method of the ServiceInterface 
                 class.                                                                       
        Notes:    
        @param pcEntityName The name of the Business Entity used in the current request                                             
        @param poFetchDataRequest The IFetchDataRequest object with the dat for the current FetchDataRequest call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LogFetchDataRequest (pcEntityName AS CHARACTER,
                                                   poFetchDataRequest AS IFetchDataRequest):
        
        LogManager:WriteMessage (SUBSTITUTE ("FetchData Request to Business Entity: &1"{&TRAN}, pcEntityName), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .

        IF NOT VALID-OBJECT (poFetchDataRequest) THEN 
            LogManager:WriteMessage ("   <no valid IFetchDataRequest object passed as parameter>"{&TRAN}, 
                                     ServiceInterface:CUSTOM_LOG_ENTRY) .
        ELSE DO:
            LogManager:WriteMessage (SUBSTITUTE ("   Tables:        &1&6"{&TRAN} + 
                                                 "   Queries:       &2&6"{&TRAN} +
                                                 "   NumRecords:    &3&6"{&TRAN} +
                                                 "   Context:       &4&6"{&TRAN} +
                                                 "   CustomContext: &5&6"{&TRAN},                                                                                                     
                                                 poFetchDataRequest:Tables,
                                                 poFetchDataRequest:Queries,
                                                 poFetchDataRequest:NumRecords,
                                                 poFetchDataRequest:Context,
                                                 poFetchDataRequest:CustomContext,
                                                 "~n":U), 
                                     ServiceInterface:CUSTOM_LOG_ENTRY) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Logger method for the FetchData method of the ServiceInterface 
                 class.                                                                       
        Notes:   Writes log information about the method runtime 
        @param pcEntityName The name of the Business Entity used in the current request                                             
        @param pdtStart The start time of the request, used to calculate runtime with interval from NOW                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LogFetchDataRequestRuntime (pcEntityName AS CHARACTER,
                                                          pdtStart AS DATETIME):
        
        LogManager:WriteMessage (SUBSTITUTE ("Finished FetchData Request to Business Entity: &1"{&TRAN}, pcEntityName), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .

        LogManager:WriteMessage (SUBSTITUTE ("Runtime FetchData Request: &1 msec"{&TRAN}, INTERVAL (NOW, pdtStart, "milliseconds":U)), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Logger method for the FetchDataset method of the ServiceInterface 
                 class.                                                                       
        Notes:                           
        @param pcEntityName The name of the Business Entity used in the current request                                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LogFetchDatasetRequest (pcEntityName AS CHARACTER):
        
        LogManager:WriteMessage (SUBSTITUTE ("FetchDataset Request to Business Entity: &1"{&TRAN}, pcEntityName), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Logger method for the InvokeMethod method of the ServiceInterface 
                 class.                                                                       
        Notes:    
        @param pcEntityName The name of the Business Entity used in the current request              
        @param pcMethodName The name of the method used for the current request
        @param phDataset The handle of the Dataset passed with the current request
        @param poParameter The parameter object passed with the current request                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LogInvokeMethodRequest (pcEntityName AS CHARACTER,
                                                      pcMethodName AS CHARACTER,
                                                      phDataset AS HANDLE,
                                                      poParameter AS Progress.Lang.Object):
    
        LogManager:WriteMessage (SUBSTITUTE ("InvokeMethod Request to Business Entity: &1"{&TRAN}, pcEntityName), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .
    
        LogManager:WriteMessage (SUBSTITUTE ("   Method-Name:       &1"{&TRAN}, pcMethodName), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .
  
        IF VALID-HANDLE (phDataset) THEN 
            LogManager:WriteMessage (SUBSTITUTE ("   Dataset:           &1"{&TRAN}, phDataset:NAME), 
                                     ServiceInterface:CUSTOM_LOG_ENTRY) .
        ELSE                                      
            LogManager:WriteMessage (SUBSTITUTE ("   Dataset:           &1"{&TRAN}, "<no dataset>"{&TRAN}), 
                                     ServiceInterface:CUSTOM_LOG_ENTRY) .
            
        IF VALID-OBJECT (poParameter) THEN 
            LogManager:WriteMessage (SUBSTITUTE ("   Parameter Object:  &1"{&TRAN}, poParameter:GetClass():TypeName), 
                                     ServiceInterface:CUSTOM_LOG_ENTRY) .
        ELSE                                      
            LogManager:WriteMessage (SUBSTITUTE ("   Parameter Object:  &1"{&TRAN}, "<no parameter object>"{&TRAN}), 
                                     ServiceInterface:CUSTOM_LOG_ENTRY) .
    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Logger method for the InvokeMethod method of the ServiceInterface 
                 class.                                                                       
        Notes:   
        @param pcEntityName The name of the Business Entity used in the current request              
        @param pcMethodName The name of the method used for the current request
        @param phDataset The handle of the Dataset passed with the current request
        @param plcParameter The longchar representation of the serialized parameter object passed with the current request                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LogInvokeMethodRequest (pcEntityName AS CHARACTER,
                                                      pcMethodName AS CHARACTER,
                                                      phDataset AS HANDLE,
                                                      plcParameter AS LONGCHAR):
    
        LogManager:WriteMessage (SUBSTITUTE ("InvokeMethod Request to Business Entity: &1"{&TRAN}, pcEntityName), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .

        LogManager:WriteMessage (SUBSTITUTE ("   Serialized Params: &1"{&TRAN}, plcParameter), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Logger method for the InvokeMethod method of the ServiceInterface 
                 class.                                                                       
        Notes:   Writes log information about the method runtime 
        @param pcEntityName The name of the Business Entity used in the current request                                             
        @param pcMethodName The name of the Business Entity method invoked
        @param pdtStart The start time of the request, used to calculate runtime with interval from NOW                                                                                
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID LogInvokeMethodRequestRuntime (pcEntityName AS CHARACTER,
	                                                         pcMethodName AS CHARACTER,
                                                             pdtStart AS DATETIME):
		
        LogManager:WriteMessage (SUBSTITUTE ("Finished InvokeMethod Request to Business Entity: &1:&2()"{&TRAN}, pcEntityName, pcMethodName), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .

        LogManager:WriteMessage (SUBSTITUTE ("Runtime InvokeMethod Request: &1 msec"{&TRAN}, INTERVAL (NOW, pdtStart, "milliseconds":U)), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Logger method for the SaveChanges method of the ServiceInterface 
                 class.                                                                       
        Notes:     
        @param pcEntityName The name of the Business Entity used in the current request              
        @param phDataset The handle of the Dataset passed with the current request
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LogSaveChangesRequest (pcEntityName AS CHARACTER,
                                                     phDataset AS HANDLE) .
                                                     
        LogManager:WriteMessage (SUBSTITUTE ("SaveChanges Request to Business Entity: &1"{&TRAN}, pcEntityName), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .
    
        IF VALID-HANDLE (phDataset) THEN 
            LogManager:WriteMessage (SUBSTITUTE ("   Dataset:           &1"{&TRAN}, phDataset:NAME), 
                                     ServiceInterface:CUSTOM_LOG_ENTRY) .
        ELSE                                      
            LogManager:WriteMessage (SUBSTITUTE ("   Dataset:           &1"{&TRAN}, "<no dataset>"{&TRAN}), 
                                     ServiceInterface:CUSTOM_LOG_ENTRY) .
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Logger method for the Save Changes method of the ServiceInterface 
                 class.                                                                       
        Notes:   Writes log information about the method runtime 
        @param pcEntityName The name of the Business Entity used in the current request                                             
        @param pdtStart The start time of the request, used to calculate runtime with interval from NOW                                                                                
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID LogSaveChangesRequestRuntime (pcEntityName AS CHARACTER,
                                                            pdtStart AS DATETIME):
	
        LogManager:WriteMessage (SUBSTITUTE ("Finished SaveChanges Request to Business Entity: &1"{&TRAN}, pcEntityName), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .

        LogManager:WriteMessage (SUBSTITUTE ("Runtime SaveChanges Request: &1 msec"{&TRAN}, INTERVAL (NOW, pdtStart, "milliseconds":U)), 
                                 ServiceInterface:CUSTOM_LOG_ENTRY) .

	END METHOD .

END CLASS.
