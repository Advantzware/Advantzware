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
    File        : Message
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:48:32,161+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.MessageDatasetModel
    INHERITS MessageDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MessageDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MessageDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MessageDatasetModel class
        Notes:   
        @param pMessageGroup The filter value for the eSmartMessage.MessageGroup field
        @param pMessageNumber The filter value for the eSmartMessage.MessageNumber field
        @param pLanguageGuid The filter value for the eSmartMessage.LanguageGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MessageDatasetModel (pMessageGroup AS CHARACTER, pMessageNumber AS INTEGER, pLanguageGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartMessage:Filter:MessageGroup:EQ (pMessageGroup) .
        THIS-OBJECT:SmartMessage:Filter:MessageNumber:EQ (pMessageNumber) .
        THIS-OBJECT:SmartMessage:Filter:LanguageGuid:EQ (pLanguageGuid) .

        THIS-OBJECT:SmartMessage:Filter:Run () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MessageDatasetModel class
        Notes:   
        @param pMessageGuid The filter value for the eSmartMessage.MessageGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MessageDatasetModel (pMessageGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartMessage:Filter:MessageGuid:EQ (pMessageGuid) .

        THIS-OBJECT:SmartMessage:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
