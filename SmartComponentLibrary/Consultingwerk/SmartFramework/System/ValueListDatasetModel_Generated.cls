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
    File        : ValueList_Generated
    Purpose     : Auto Generated portion of the ValueList class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:31,993+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.ValueListBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.ValueListDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/System/dsValueList.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartValueList table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartValueList AS SmartValueListTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartValueListEntry table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartValueListEntry AS SmartValueListEntryTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.System.ValueListBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ValueListDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ValueListDatasetModel_Generated ():
        SUPER (DATASET dsValueList:HANDLE).

        THIS-OBJECT:SmartValueList = NEW SmartValueListTableModel (THIS-OBJECT, BUFFER eSmartValueList:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartValueList) .

        THIS-OBJECT:SmartValueListEntry = NEW SmartValueListEntryTableModel (THIS-OBJECT, BUFFER eSmartValueListEntry:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartValueListEntry) .

    
    END CONSTRUCTOR.

  

END CLASS.
