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
    File        : SmartMessageInterface
    Purpose     : Service Interface for SmartMessage requests
    Syntax      : Singleton run class for REST Adapter
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 09:03:35 CEST 2015
    Notes       : SCL-921
  ----------------------------------------------------------------------*/

@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                    FROM PROPATH .
USING Consultingwerk.Exceptions.*                   FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.Framework.*                    FROM PROPATH .
USING Consultingwerk.Framework.Session.*            FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*        FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .
USING Consultingwerk.Web2.Services.Exceptions.*     FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartMessage.*   FROM PROPATH .
USING Consultingwerk.Web2.SmartFramework.Language.* FROM PROPATH .
USING Consultingwerk.Web2.SmartFramework.Message.*  FROM PROPATH .
USING Progress.Json.ObjectModel.*                   FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.Services.SmartMessage.SmartMessageInterface:

    { Consultingwerk/SmartFramework/System/dsMessage.i }

    DEFINE VARIABLE oConfig AS IConfigurationProvider NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartMessageInterface class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMessageInterface ():

        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        SUPER ().

        FileAssert:Exists (".restapplicationsettings":U) .

        ASSIGN cFileName = FileHelper:FindFile (".restapplicationsettings":U) .

        ASSIGN oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider
                                                       "NEW ConfigurationProvider (cFileName)"} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a SmartMessage based on the Message Group and Number
                 as a JSON representation
        Notes:
        @param pcMessageGroup The message group of the message to return
        @param piMessageNumber The message number of the message to return
        @param pcMessage The JSON structure of the returned message
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    METHOD PUBLIC VOID GetSmartMessage (pcMessageGroup AS CHARACTER,
                                        piMessageNumber AS INTEGER,
                                        OUTPUT pcMessage AS LONGCHAR):

        ASSIGN pcMessage = THIS-OBJECT:GetSerializableSmartMessage (pcMessageGroup, piMessageNumber):Serialize () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a SmartMessage based on the Message Group and Number
                 as a JSON representation
        Notes:
        @param pcMessageGroup The message group of the message to return
        @param piMessageNumber The message number of the message to return
        @return The JSON structure of the returned message
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC JsonObject GetSmartMessage (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER):

        RETURN THIS-OBJECT:GetSerializableSmartMessage (pcMessageGroup, piMessageNumber):ToJsonObject () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a SmartMessage based on the Message Group and Number
                 as a JSON representation
        Notes:
        @param pcMessageGroup The message group of the message to return
        @param piMessageNumber The message number of the message to return
        @return The SmartSerializableMessage structure representing the returned message
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED SerializableSmartMessage GetSerializableSmartMessage (pcMessageGroup AS CHARACTER,
                                                                           piMessageNumber AS INTEGER):

        DEFINE VARIABLE oGetMessageParameter AS GetMessageParameter      NO-UNDO .
        DEFINE VARIABLE oMessage             AS SerializableSmartMessage NO-UNDO .

        ServiceInterface:Activate () .

        ASSIGN oGetMessageParameter = NEW GetMessageParameter (pcMessageGroup,
                                                               piMessageNumber,
                                                               SessionManager:LanguageKey) .

        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.System.MessageBusinessEntity":U,
                                       "GetMessage":U,
                                       INPUT-OUTPUT DATASET dsMessage BY-REFERENCE,
                                       oGetMessageParameter) .

        FIND FIRST eSmartMessage NO-ERROR .

        IF NOT AVAILABLE eSmartMessage THEN
            UNDO, THROW NEW Web2ServiceInterfaceException ("Unable to find requested Smartmessage."{&TRAN}, 0) .

        oMessage = NEW SerializableSmartMessage () .

        ASSIGN oMessage:MessageGroup  = eSmartMessage.MessageGroup
               oMessage:MessageNumber = eSmartMessage.MessageNumber
               oMessage:MessageText   = eSmartMessage.MessageText
               oMessage:MessageDetail = eSmartMessage.MessageDetail
               oMessage:Language      = SerializableSmartLanguage:FromLanguageGuid (eSmartMessage.LanguageGuid).

        DO ON ERROR UNDO, THROW:
&IF DEFINED (NativeEnums) NE 0 &THEN
            ASSIGN oMessage:MessageType =  CAST (Progress.Lang.Enum:ToObject ("Consultingwerk.SmartFramework.MessageTypeEnum":U,
                                                                              eSmartMessage.MessageType),
                                                 Consultingwerk.SmartFramework.MessageTypeEnum) .
&ELSE
            ASSIGN oMessage:MessageType = MessageTypeEnum:FromString (eSmartMessage.MessageType) .
&ENDIF

            @SuppressUnusedWarnings.
            CATCH err AS InvalidValueException:
                /* Ignore */
            END CATCH.
        END.

        RETURN oMessage .

        FINALLY:
            ServiceInterface:Deactivate () .
        END FINALLY.

    END METHOD .

END CLASS.
