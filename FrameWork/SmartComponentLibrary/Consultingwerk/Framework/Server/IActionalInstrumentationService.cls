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
    File        : IActionalInstrumentationService
    Purpose     : Interface describing the methods required for Actional 
                  Instrumentation Services
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Thu Jan 26 23:59:39 CET 2012
    Notes       : Actual Instrumentation is no longer supported. See SCL-868
                  Progress removed the code in OpenEdge 11.0 and we do no 
                  longer support OpenEdge 11.0 - but we leave this interface 
                  here in case customers are using it anyway
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.Server.IActionalInstrumentationService:  
  
    /*------------------------------------------------------------------------------
        Purpose: Creates an Actional Interaction                                                                        
        Notes:                                                          
        @param pcProcedureName The name of the procedure or class that creates the Interaction - usually from PROGRAM-NAME(1)            
        @param poFields A ListNameValuePair of fields to send with the interaction  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateInteraction (pcProcedureName AS CHARACTER,
                                          poFields AS Consultingwerk.ListNameValuePair).

    /*------------------------------------------------------------------------------
        Purpose: Creates an Actional Interaction. also logging the field values for 
                 a given buffer handle                                                                      
        Notes:                                                          
        @param pcProcedureName The name of the procedure or class that creates the Interaction - usually from PROGRAM-NAME(1)           
        @param poFields A ListNameValuePair of fields to send with the interaction  
        @param phBuffer The handle of a Buffer to log the fields with the Actional Interaction
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateInteraction (pcProcedureName AS CHARACTER,
                                          poFields AS Consultingwerk.ListNameValuePair,
                                          phBuffer AS HANDLE).

END INTERFACE.