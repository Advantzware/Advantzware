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
    File        : ActionalInstrumentationService
    Purpose     : Default ActionalInstrumentation Service
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Thu Jan 26 23:59:39 CET 2012
    Notes       : Actual Instrumentation is no longer supported. See SCL-868
                  Progress removed the code in OpenEdge 11.0 and we do no 
                  longer support OpenEdge 11.0 - but we leave this class 
                  here in case customers are using it anyway
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.*                  FROM PROPATH .
USING Consultingwerk.Framework.Server.* FROM PROPATH .
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.Framework.Server.ActionalInteractionService 
    IMPLEMENTS IActionalInstrumentationService: 

    /*------------------------------------------------------------------------------
        Purpose: Creates an Actional Interaction                                                                      
        Notes:                                                          
        @param pcProcedureName The name of the procedure or class that creates the Interaction - usually from PROGRAM-NAME(1)           
        @param poFields A ListNameValuePair of fields to send with the interaction  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateInteraction (pcProcedureName AS CHARACTER,
                                          poFields AS ListNameValuePair):
        
        /* Mike Fechner, Consultingwerk Ltd. 27.01.2012
           Support for Actional Instrumentation, only with OpenEdge 11.0, no longer supported on OpenEdge 11.1 */
        &IF PROVERSION EQ "11.0" &THEN
        
        /* Default to the caller */
        IF pcProcedureName > "":U THEN .
        ELSE 
            ASSIGN pcProcedureName = PROGRAM-NAME (2) .
        
        {actional/instrument.i &INIT=TRUE}

        {Consultingwerk/foreachABL.i NameValuePair oPair in poFields}
            {actional/instrument.i &FIELD=oPair:Name &VALUE=oPair:Value}
        END.

        {actional/instrument.i &BEGIN=TRUE &OPNAME=pcProcedureName}
        &ENDIF

    END METHOD.

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
                                          phBuffer AS HANDLE):

        /* Mike Fechner, Consultingwerk Ltd. 27.01.2012
           Support for Actional Instrumentation, only with OpenEdge 11.0, no longer supported on OpenEdge 11.1 */
        &IF PROVERSION EQ "11.0" &THEN

        /* Default to the caller */
        IF pcProcedureName > "":U THEN .
        ELSE 
            ASSIGN pcProcedureName = PROGRAM-NAME (2) .

        IF phBuffer:AVAILABLE THEN 
            Consultingwerk.Util.BufferHelper:AddBufferFieldsToList (phBuffer, poFields, TRUE) . 

        CreateInteraction (pcProcedureName, poFields) .
        &ENDIF
        
    END METHOD . 

END CLASS.
