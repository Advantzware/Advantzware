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
    File        : SecurityAssignmentBusinessEntity
    Purpose     : Business Entity for SecurityAssignment
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 28.07.2013 17:49:49
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.Assertion.*                    FROM PROPATH .
USING Consultingwerk.Framework.Session.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityAssignmentBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/Authentication/dsUser.i }
    { Consultingwerk/SmartFramework/Authorization/dsGroup.i }
    { Consultingwerk/SmartFramework/Authorization/dsSecurityAssignment.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SecurityAssignmentBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityAssignmentBusinessEntity ():
        SUPER (DATASET dsSecurityAssignment:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Authorization.SecurityAssignmentDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Gets a group record using the business entity
        Notes:
        @param pcGroupGuid The group guid to be found
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetGroup (pcGroupGuid AS CHARACTER):

        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Authorization.GroupBusinessEntity":U,
                                    NEW FetchDataRequest ("eSmartGroup":U,
                                                          SUBSTITUTE ("FOR EACH eSmartGroup WHERE eSmartGroup.GroupGuid = &1":U,
                                                                      QUOTER (pcGroupGuid))),
                                    OUTPUT DATASET dsGroup) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets a user record using the business entity
        Notes:
        @param pcUserGuid The user guid to be found
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetUser (pcUserGuid AS CHARACTER):

        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                    NEW FetchDataRequest ("eSmartUser":U,
                                                          SUBSTITUTE ("FOR EACH eSmartUser WHERE eSmartUser.UserGuid = &1":U,
                                                                      QUOTER (pcUserGuid))),
                                    OUTPUT DATASET dsUser) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

        DEFINE VARIABLE oInfoService AS SecurityRealmInfoService NO-UNDO .

        oInfoService = CAST (ServiceManager:GetBusinessService ("Consultingwerk.SmartFramework.Authorization.SecurityRealmInfoService":U),
                             SecurityRealmInfoService) .

        FOR EACH eSmartSecurityAssignment ON ERROR UNDO, THROW:

            ASSIGN eSmartSecurityAssignment.SecurityRealmCode        = oInfoService:GetSecurityRealmCode (eSmartSecurityAssignment.SecurityRealmGuid)
                   eSmartSecurityAssignment.SecurityRealmDescription = oInfoService:GetSecurityRealmDescription (eSmartSecurityAssignment.SecurityRealmGuid)
                   eSmartSecurityAssignment.SecurityItemDescription  = oInfoService:GetSecurityItemDescription (eSmartSecurityAssignment.SecurityRealmGuid,
                                                                                                                eSmartSecurityAssignment.SecurityItemGuid)
                    .

            IF CharacterType:IsNotNullOrEmpty (eSmartSecurityAssignment.GroupGuid) AND
               CharacterType:IsNullOrEmpty (eSmartSecurityAssignment.UserGuid) THEN
                eSmartSecurityAssignment.OwnerType = "G":U .
            ELSE
                IF CharacterType:IsNotNullOrEmpty (eSmartSecurityAssignment.UserGuid) AND
                   CharacterType:IsNullOrEmpty (eSmartSecurityAssignment.GroupGuid) THEN
                    eSmartSecurityAssignment.OwnerType = "U":U .
                ELSE
                    eSmartSecurityAssignment.OwnerType = "":U .

            ASSIGN eSmartSecurityAssignment.OwnerName = "":U
                   eSmartSecurityAssignment.GroupName = "":U
                   eSmartSecurityAssignment.UserName  = "":U .

            IF CharacterType:IsNotNullOrEmpty (eSmartSecurityAssignment.GroupGuid) THEN DO:
                THIS-OBJECT:GetGroup (eSmartSecurityAssignment.GroupGuid) .

                FIND FIRST eSmartGroup NO-ERROR .

                IF AVAILABLE eSmartGroup THEN
                    ASSIGN eSmartSecurityAssignment.OwnerName = eSmartGroup.GroupName
                           eSmartSecurityAssignment.GroupName = eSmartGroup.GroupName .
            END.

            IF CharacterType:IsNotNullOrEmpty (eSmartSecurityAssignment.UserGuid) THEN DO:
                THIS-OBJECT:GetUser (eSmartSecurityAssignment.UserGuid) .

                FIND FIRST eSmartUser NO-ERROR .

                IF AVAILABLE eSmartUser THEN
                    ASSIGN eSmartSecurityAssignment.OwnerName = eSmartUser.UserName
                           eSmartSecurityAssignment.UserName  = eSmartUser.UserName .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update
                 operations
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

        DEFINE VARIABLE oInfoService AS SecurityRealmInfoService NO-UNDO .

        oInfoService = CAST (ServiceManager:GetBusinessService ("Consultingwerk.SmartFramework.Authorization.SecurityRealmInfoService":U),
                             SecurityRealmInfoService) .

        FOR EACH eSmartSecurityAssignment ON ERROR UNDO, THROW:
            IF CharacterType:IsNotNullOrEmpty (eSmartSecurityAssignment.GroupGuid) AND
               CharacterType:IsNotNullOrEmpty (eSmartSecurityAssignment.UserGuid) THEN

                DatasetHelper:SetErrorString (BUFFER eSmartSecurityAssignment:HANDLE,
                                              "Only User or Group may be specified for security assignment."{&TRAN}) .
            ELSE DO:
                IF CharacterType:IsNotNullOrEmpty (eSmartSecurityAssignment.GroupGuid) THEN DO:
                    THIS-OBJECT:GetGroup (eSmartSecurityAssignment.GroupGuid) .

                    IF NOT CAN-FIND (FIRST eSmartGroup) THEN
                        DatasetHelper:SetErrorString (BUFFER eSmartSecurityAssignment:HANDLE,
                                                      "Invalid user group specified."{&TRAN}) .
                END.
                ELSE IF CharacterType:IsNotNullOrEmpty (eSmartSecurityAssignment.UserGuid) THEN DO:
                    THIS-OBJECT:GetUser (eSmartSecurityAssignment.UserGuid) .

                    IF NOT CAN-FIND (FIRST eSmartUser) THEN
                        DatasetHelper:SetErrorString (BUFFER eSmartSecurityAssignment:HANDLE,
                                                      "Invalid user specified."{&TRAN}) .
                END.
            END.

            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartSecurityAssignment:HANDLE,
                                                           "SecurityRealmGuid":U,
                                                           "Security realm needs to be specified."{&TRAN}) .

            IF oInfoService:GetSecurityRealmEmptyKeyAllowed (eSmartSecurityAssignment.SecurityRealmGuid) = FALSE THEN
                Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartSecurityAssignment:HANDLE,
                                                               "SecurityItemGuid":U,
                                                               "Security item needs to be specified."{&TRAN}) .

            IF oInfoService:GetSecurityRealmCompanyDependent (eSmartSecurityAssignment.SecurityRealmGuid) = TRUE THEN DO:
                /* Mike Fechner, Consultingwerk Ltd. 04.09.2015
                   SCL-1007 - only assign login company guid for new records that are not already provided by the caller
                   with login company guid value */
                IF ROW-STATE (eSmartSecurityAssignment) = ROW-CREATED AND CharacterType:IsNullOrEmpty (eSmartSecurityAssignment.LoginCompanyGuid) THEN DO:
                    {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i SessionManager:LoginCompanyKey "'Login Company Key':U"} .

                    ASSIGN eSmartSecurityAssignment.LoginCompanyGuid = SessionManager:LoginCompanyKey .
                END.
            END.
            ELSE
                ASSIGN eSmartSecurityAssignment.LoginCompanyGuid = "":U .
        END.

    END METHOD.

END CLASS.
