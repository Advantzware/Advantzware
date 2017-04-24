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
    File        : BeforeCreateRecordEventArgs
    Purpose     : Event argument for the BeforeCreateRecord event
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 17 23:24:00 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.BeforeCancelCreateRecordEventArgs 
    INHERITS System.ComponentModel.CancelEventArgs: 
    
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
        Purpose: A message indicating why the create has been cancelled.
        Notes:                                                                         
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY CancelMessage AS CHARACTER NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: When set to TRUE (in an event handler) the method CreateRecord
                 will not use BufferHelper:CanCreateDefaultRow () to validate if the
                 new record can be added to the current (client side) query result 
                 list
        Notes:   Set to TRUE when performing manual validation if the new record
                 can be added to the current queries result list.                                                                         
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY SkipUniqueRecordValidation AS LOGICAL NO-UNDO INIT FALSE
    GET.
    SET.
    
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
        Purpose: Constructor for the BeforeCancelCreateRecordEventArgs class  
        Notes:      
        @param pcBusinessEntityName The business entity name
        @param pcTableName The name of the business entity table
        @param phBufferHandle The handle of the temp-table buffer
        @param phQueryHandle The handle of the query navigating the buffer                 
    ------------------------------------------------------------------------------*/   
    CONSTRUCTOR PUBLIC BeforeCancelCreateRecordEventArgs (pcBusinessEntityName AS CHARACTER,
                                                          pcTableName AS CHARACTER,
                                                          phBufferHandle AS HANDLE,
                                                          phQueryHandle AS HANDLE):
        SUPER ().
        
        ASSIGN THIS-OBJECT:BusinessEntityName = pcBusinessEntityName
               THIS-OBJECT:TableName          = pcTableName
               THIS-OBJECT:BufferHandle       = phBufferHandle 
               THIS-OBJECT:QueryHandle        = phQueryHandle .
        
    END CONSTRUCTOR.

END CLASS.
