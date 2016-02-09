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
    File        : AfterCreateRecordEventArgs
    Purpose     : Event argument for the AfterCreateRecord event
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 17 23:24:00 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*  FROM PROPATH .
USING System.EventArgs FROM ASSEMBLY . 

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Implementation.AfterCancelCreateRecordEventArgs 
    INHERITS System.EventArgs: 
    
    /*------------------------------------------------------------------------------
        Purpose: A reference to the new record
        Notes:                                                                         
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY BufferHandle AS HANDLE NO-UNDO 
    GET.
    PROTECTED SET.
    
    /*------------------------------------------------------------------------------
        Purpose: The name of the BusinessEntity the raised this event
        Notes:                                                                         
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET.
        
    /*------------------------------------------------------------------------------
        Purpose: The handle of the Query using the Buffers of the DataAdapter
        Notes:   Potentially used to Query Buffer handles (no. 2 and up) to 
                 refetch them in an Event Handler for later taking new records on 
                 the joined table into account for CREATE-RESULT-LIST-ENTRY                                                                           
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY QueryHandle AS HANDLE NO-UNDO 
    GET.
    PROTECTED SET.        
        
    /*------------------------------------------------------------------------------
        Purpose: The name of the BusinessEntity's table the raised this event
        Notes:                                                                         
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY TableName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AfterCancelCreateRecordEventArgs class 
        Notes:   
        @param pcBusinessEntityName The business entity name
        @param pcTableName The name of the business entity table
        @param phBufferHandle The handle of the temp-table buffer
        @param phQueryHandle The handle of the query navigating the buffer                 
    ------------------------------------------------------------------------------*/   
    CONSTRUCTOR PUBLIC AfterCancelCreateRecordEventArgs (pcBusinessEntityName AS CHARACTER,
                                                         pcTableName AS CHARACTER,
                                                         phBufferHandle AS HANDLE,
                                                         phQueryHandle AS HANDLE):
        SUPER ().
        
        ASSIGN THIS-OBJECT:BusinessEntityName = pcBusinessEntityName
               THIS-OBJECT:TableName          = pcTableName
               THIS-OBJECT:BufferHandle       = phBufferHandle 
               THIS-OBJECT:QueryHandle        = phQueryHandle.
        
    END CONSTRUCTOR.

END CLASS.
