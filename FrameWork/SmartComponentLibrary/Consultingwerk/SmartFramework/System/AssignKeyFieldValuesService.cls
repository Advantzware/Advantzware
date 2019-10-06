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
    File        : AssignKeyFieldValuesService
    Purpose     : Default implementation of the IAssignKeyFieldValuesService
                  Service Interface.
                  Assigns a new GUID to the key fields
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 03 12:15:05 CEST 2015
    Notes       : See SmartDataAccess:AssignKeyFieldValues(), SCL-1041,
                  Controlled by the SmartTable.KeyFieldAssignmentGUID 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.*        FROM PROPATH .  
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.AssignKeyFieldValuesService
    IMPLEMENTS IAssignKeyFieldValuesService:  
  
    /*------------------------------------------------------------------------------
        Purpose: Assigns key fields to a new record 
        Notes:   
        @param pcKeyFieldAssignmentKey The key that indicated which key value should be assigned
        @param phBuffer The temp-table buffer (after buffer) to assign key field values to
        @param pcKeyFields Comma delimited list of key fields to assign
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignKeyFields (pcKeyFieldAssignmentKey AS CHARACTER,
                                        phBuffer AS HANDLE,
                                        pcKeyFields AS CHARACTER):
        
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cField AS CHARACTER NO-UNDO .
        
        DO i = 1 TO NUM-ENTRIES (pcKeyFields):        
           
           ASSIGN cField = ENTRY (i, pcKeyFields) .
           
           IF phBuffer:BUFFER-FIELD (cField):BUFFER-VALUE = "":U OR 
              phBuffer:BUFFER-FIELD (cField):BUFFER-VALUE = ? THEN 
               
           phBuffer:BUFFER-FIELD (cField):BUFFER-VALUE = GUID . 
        
        END.
        
    END METHOD .

END CLASS .
