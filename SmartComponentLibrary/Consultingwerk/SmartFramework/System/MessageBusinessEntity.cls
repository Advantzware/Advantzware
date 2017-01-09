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
    File        : MessageBusinessEntity
    Purpose     : Business Entity for Message
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 09.01.2013 08:02:19
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*        FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

@BusinessEntityTable (name="eSmartMessage", MandatoryColumns="MessageGroup,MessageNumber", DefaultSort="BY eSmartMessage.MessageGroup BY eSmartMessage.MessageNumber") .

CLASS Consultingwerk.SmartFramework.System.MessageBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/System/dsMessage.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MessageBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MessageBusinessEntity ():
        SUPER (DATASET dsMessage:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.MessageDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Client callable methode to retrieve a specific message either
                 in the client's language, the unspecified language or the first
                 language found in the SmartMessage table
        Notes:
        @param dsMessage INPUT-OUTPUT Dataset with the message
        @param poGetMessageParameter The GetMessageParameter with the request data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetMessage (INPUT-OUTPUT DATASET {&DATASET-NAME},
                                   poGetMessageParameter AS GetMessageParameter):

        DEFINE VARIABLE oRequest AS IFetchDataRequest NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poGetMessageParameter,
                                                       "GetMessageParameter":U) .

        /* First try the client session language - if specified */
        IF poGetMessageParameter:SessionLanguageKey > "":U THEN DO:
            oRequest = NEW FetchDataRequest
                                ("eSmartMessage":U,
                                 SUBSTITUTE ("FOR EACH eSmartMessage WHERE eSmartMessage.MessageGroup = &1 AND eSmartMessage.MessageNumber = &2 AND eSmartMessage.LanguageGuid = &3":U,
                                             QUOTER (poGetMessageParameter:MessageGroup),
                                             poGetMessageParameter:MessageNumber,
                                             QUOTER (poGetMessageParameter:SessionLanguageKey)),
                                 1) .

            THIS-OBJECT:FetchData (oRequest) .

            IF CAN-FIND (FIRST eSmartMessage) THEN
                RETURN .
        END.

        /* now try the unspecified (default) language */
        oRequest = NEW FetchDataRequest
                            ("eSmartMessage":U,
                             SUBSTITUTE ("FOR EACH eSmartMessage WHERE eSmartMessage.MessageGroup = &1 AND eSmartMessage.MessageNumber = &2 AND eSmartMessage.LanguageGuid = &3":U,
                                         QUOTER (poGetMessageParameter:MessageGroup),
                                         poGetMessageParameter:MessageNumber,
                                         QUOTER ("":U)),
                             1) .

        THIS-OBJECT:FetchData (oRequest) .

        IF CAN-FIND (FIRST eSmartMessage) THEN
            RETURN .

        /* use any language as a fall back */
        oRequest = NEW FetchDataRequest
                            ("eSmartMessage":U,
                             SUBSTITUTE ("FOR EACH eSmartMessage WHERE eSmartMessage.MessageGroup = &1 AND eSmartMessage.MessageNumber = &2":U,
                                         QUOTER (poGetMessageParameter:MessageGroup),
                                         poGetMessageParameter:MessageNumber),
                             1) .

        THIS-OBJECT:FetchData (oRequest) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update
                 operations
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

        DEFINE VARIABLE oSecurityService AS ISecurityService NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Don't perform security check when importing data */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = TRUE THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 17.08.2014
           Messages need to have Message Group and non zero number */
        FOR EACH eSmartMessage:
            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartMessage:HANDLE,
                                                           "MessageGroup":U,
                                                           MessageFormatter:GetMessage ("VALMSG":U, 1, "Message Group"{&TRAN})) .

            Consultingwerk.OERA.Validate:IsNotUnknownOrZero (BUFFER eSmartMessage:HANDLE,
                                                             "MessageNumber":U,
                                                             MessageFormatter:GetMessage ("VALMSG":U, 4, "Message Group"{&TRAN})) .

        END.

        oSecurityService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Authorization.ISecurityService} .

        IF VALID-OBJECT (oSecurityService) THEN DO:
            FOR EACH eSmartMessage ON ERROR UNDO, THROW:

                Consultingwerk.OERA.Validate:IsNotRestricted (BUFFER eSmartMessage:HANDLE,
                                                              "LanguageGuid":U,
                                                              "SMT":U,
                                                              MessageFormatter:GetMessage ("SFR":U, 200)) .
            END.

            FOR EACH eSmartMessageBefore WHERE ROW-STATE (eSmartMessageBefore) = ROW-DELETED ON ERROR UNDO, THROW:
                IF oSecurityService:IsRestricted ("SMT":U, eSmartMessageBefore.LanguageGuid) THEN

                    DatasetHelper:AddErrorString (BUFFER eSmartMessageBefore:HANDLE,
                                                  MessageFormatter:GetMessage ("SFR":U, 201)) .
            END .
        END .

    END METHOD.

END CLASS.
