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
    File        : Message_Generated
    Purpose     : Auto Generated portion of the Message class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:29,428+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.MessageBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.MessageDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/System/dsMessage.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartMessage table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartMessage AS SmartMessageTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.System.MessageBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MessageDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MessageDatasetModel_Generated ():
        SUPER (DATASET dsMessage:HANDLE).

        THIS-OBJECT:SmartMessage = NEW SmartMessageTableModel (THIS-OBJECT, BUFFER eSmartMessage:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartMessage) .

    
    END CONSTRUCTOR.

  

END CLASS.
