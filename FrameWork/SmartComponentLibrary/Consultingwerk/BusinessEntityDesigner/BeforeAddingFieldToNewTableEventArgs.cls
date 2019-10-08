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
    File        : BeforeAddingFieldToNewTableEventArgs
    Purpose     : Event argument to the BeforeAddingFieldToNewTable Event
                  of the BusinessEntityDatasetController
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 26 17:06:44 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.* FROM PROPATH .  
USING Progress.Lang.*                         FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.BeforeAddingFieldToNewTableEventArgs 
    INHERITS CancelableEventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Database Name associated with the event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatabaseName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Field Name associated with the event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Table Name associated with the event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BeforeAddingFieldToNewTableEventArgs class
        Notes:   
        @param pcDatabaseName The Database Name associated with the event
        @param pcTableName The Table Name associated with the event
        @param pcFieldName The Field Name associated with the event
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BeforeAddingFieldToNewTableEventArgs (pcDatabaseName AS CHARACTER,
                                                             pcTableName AS CHARACTER,
                                                             pcFieldName AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:DatabaseName = pcDatabaseName
               THIS-OBJECT:TableName    = pcTableName
               THIS-OBJECT:FieldName    = pcFieldName . 
        
    END CONSTRUCTOR.

END CLASS.
