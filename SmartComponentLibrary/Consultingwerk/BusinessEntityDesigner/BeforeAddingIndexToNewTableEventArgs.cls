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
    File        : BeforeAddingIndexToNewTableEventArgs
    Purpose     : Event argument to the BeforeAddingIndewToNewTable Event
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

CLASS Consultingwerk.BusinessEntityDesigner.BeforeAddingIndexToNewTableEventArgs 
    INHERITS CancelableEventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Database Name associated with the event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatabaseName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Sort of the Index Fields associated with the event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldSort AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Index Fields associated with the event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IndexFields AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Index Name associated with the event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IndexName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the if the Index is the primary Index associated with the event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IndexPrimary AS LOGICAL NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the if the Index is an unique Index associated with the event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IndexUnique AS LOGICAL NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the if the Index is a word Index associated with the event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IndexWord AS LOGICAL NO-UNDO 
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
        Purpose: Constructor for the BeforeAddingIndexToNewTableEventArgs class
        Notes:   
        @param pcDatabaseName The Database Name associated with the event
        @param pcTableName The Table Name associated with the event
        @param pcIndexName The Index Name associated with the event
        @param pcIndexFields The Index Fields associated with the event
        @param pcFieldSort The Sort of the Index fields associated with the event 
        @param plPrimaryIndex Is the index an primary index
        @param plUniqueIndex Is the index unique
        @param plWordIndex Is the index a word index 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BeforeAddingIndexToNewTableEventArgs (pcDatabaseName AS CHARACTER,
                                                             pcTableName AS CHARACTER,
                                                             pcIndexName AS CHARACTER,
                                                             pcIndexFields AS CHARACTER,
                                                             pcFieldSort AS CHARACTER,
                                                             plPrimaryIndex AS LOGICAL,
                                                             plUniqueIndex AS LOGICAL,
                                                             plWordIndex AS LOGICAL):
        SUPER ().
        
        ASSIGN THIS-OBJECT:DatabaseName = pcDatabaseName
               THIS-OBJECT:TableName    = pcTableName
               THIS-OBJECT:IndexName    = pcIndexName
               THIS-OBJECT:IndexFields  = pcIndexFields
               THIS-OBJECT:FieldSort    = pcFieldSort 
               THIS-OBJECT:IndexPrimary = plPrimaryIndex
               THIS-OBJECT:IndexUnique  = plUniqueIndex
               THIS-OBJECT:IndexWord    = plWordIndex . 
        
    END CONSTRUCTOR.

END CLASS.
