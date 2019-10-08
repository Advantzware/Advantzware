/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartHybridRealm
    Purpose     : SmartFramework based IHybridRealm implementation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 08 14:23:56 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*                         FROM PROPATH .
USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.*      FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*              FROM PROPATH .
USING Consultingwerk.Web2.SmartFramework.Authentication.* FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .
USING Progress.Security.Realm.*                           FROM PROPATH .

CLASS Consultingwerk.Web2.SmartFramework.Authentication.SmartHybridRealm
    IMPLEMENTS IHybridRealm:

    /*------------------------------------------------------------------------------
        Purpose: This method retrieves an attribute for the specified user account.
        Notes:
        @param piUserId The integer user id
        @param pcAttrName The name of the attribute
        @return The attribute value or ""
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetAttribute (piUserid AS INTEGER,
                                          pcAttrName AS CHARACTER):

/*        LOG-MANAGER:WRITE-MESSAGE ("GetAttribute UID=" + STRING(piUserid) + " Attribute=" + pcAttrName, "OEREALM").*/

        CASE pcAttrName:
            WHEN "ATTR_ROLES":U THEN
                RETURN "PSCUser":U .

            WHEN "ATTR_ENABLED":U THEN
                RETURN "true":U .

        END CASE .

        RETURN "":U .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: This method retrieves the name assigned to the attributes
        Notes:
        @param piUserId The integer user id
        @return The array of attributes
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER EXTENT GetAttributeNames (piUserid AS INTEGER):

        DEFINE VARIABLE cNames AS CHARACTER EXTENT 1 NO-UNDO.

/*        LOG-MANAGER:WRITE-MESSAGE ("GetAttributeNames UID=" + STRING(piUserid), "OEREALM").*/

        /* we only support getting roles */
        cNames[1] = "ATTR_ROLES":U.

        RETURN cNames.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: This method retrieves all the user names associated with the user
                 account.
        Notes:   Not implemented (not mandatory method)
        @return The array of user names associated with the user account
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER EXTENT GetUserNames ():

        UNDO, THROW NEW Consultingwerk.Exceptions.NotImplementedException (PROGRAM-NAME (1)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: This method retrieves the user names that match the input query string.
        Notes:   Not implemented (not mandatory method)
        @param pcQueryString The query string to retrieve matching users
        @return The array of user names matching the query
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER EXTENT GetUserNamesByQuery (pcQueryString AS CHARACTER):

        UNDO, THROW NEW Consultingwerk.Exceptions.NotImplementedException (PROGRAM-NAME (1)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: This method deletes the value of the given attribute
        Notes:   Not implemented (not mandatory method)
        @param piUserId The integer user id
        @param pcAttrName The name of the attribute
        @return Logical value indicating the success of the method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RemoveAttribute (piUserid AS INTEGER,
                                           pcAttrName AS CHARACTER):

        UNDO, THROW NEW Consultingwerk.Exceptions.NotImplementedException (PROGRAM-NAME (1)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: This method assigns a new value to the given attribute.
        Notes:   Not implemented (not mandatory method)
        @param piUserId The integer user id
        @param pcAttrName The name of the attribute
        @param pcAttrValue The attribute value
        @return Logical value indicating the success of the method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL SetAttribute (piUserid AS INTEGER,
                                        pcAttrName AS CHARACTER,
                                        pcAttrValue AS CHARACTER):

        UNDO, THROW NEW Consultingwerk.Exceptions.NotImplementedException (PROGRAM-NAME (1)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Validates a users password
        Notes:   This method validates the client-supplied password of the user
                 account against the stored password
        @param piUserId The integer user id
        @param pcPassword The provided user password (plain text)
        @return Logical value indicating if the password is correct
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidatePassword (piUserid AS INTEGER,
                                            pcPassword AS CHARACTER):

        DEFINE VARIABLE oUser         AS UserDatasetModel         NO-UNDO .

        oUser = NEW UserDatasetModel () .
        oUser:SmartUser:Filter:UserSecurityId:EQ (piUserId).
        oUser:SmartUser:Filter:UserPassword:EQ (ENCODE (pcPassword)):Run () .

        RETURN oUser:SmartUser:Available .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs digest based password validation
        Notes:   Not implemented (not mandatory method)
        @param piUserId The integer user id
        @param pcDigest The password digest
        @param pcNonce The nonce value
        @param pcTimestamp The time stamp value
        @return Logical value indicating if the password is correct
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidatePassword (piUserid AS INTEGER,
                                            pcDigest AS CHARACTER,
                                            pcNonce AS CHARACTER,
                                            pcTimestamp AS CHARACTER):

        UNDO, THROW NEW Consultingwerk.Exceptions.NotImplementedException (PROGRAM-NAME (1)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Called by the REST Adapter to validate a user
        Notes:   This method searches for the user account, validates whether the
                 account domain is valid and enabled, and returns the account numeric ID.
        @param pcUserName The user name either as a simple user name or in the form of user@domain
        @return The integer user id
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER ValidateUser (pcUserName AS CHARACTER):

        DEFINE VARIABLE cName         AS CHARACTER                NO-UNDO.
        DEFINE VARIABLE cDomain       AS CHARACTER                NO-UNDO.

        DEFINE VARIABLE oLoginCompany AS LoginCompanyDatasetModel NO-UNDO .
        DEFINE VARIABLE oUser         AS UserDatasetModel         NO-UNDO .

        ASSIGN cName   = ENTRY (1, pcUserName, "@":U)
               cDomain = ListHelper:EntryWhenAvailable (2, pcUserName, "@":U) .

/*        LOG-MANAGER:WRITE-MESSAGE ("ValidateUser Full UserName="   + pcUserName , "OEREALM").*/
/*        LOG-MANAGER:WRITE-MESSAGE ("ValidateUser Name="   + cName      , "OEREALM").         */
/*        LOG-MANAGER:WRITE-MESSAGE ("ValidateUser Domain=" + cDomain    , "OEREALM").         */

        IF cDomain > "":U THEN DO:
            oLoginCompany = NEW LoginCompanyDatasetModel () .
            oLoginCompany:SmartLoginCompany:Filter:LoginCompanyTenantDomain:EQ (cDomain):Run () .

            IF NOT oLoginCompany:SmartLoginCompany:Available THEN
                UNDO, THROW NEW Exception (SUBSTITUTE ("Unable to validate user because no login company exists with the provided domain name: &1"{&TRAN}, cDomain), 0) .

            /* locate Domain specific user */
            oUser = NEW UserDatasetModel () .
            oUser:SmartUser:Filter:LoginCompanyGuid:EQ (oLoginCompany:SmartLoginCompany:LoginCompanyGuid) .
            oUser:SmartUser:Filter:UserName:EQ (cName):Run () .

            IF oUser:SmartUser:Available THEN
                RETURN oUser:SmartUser:UserSecurityId .
        END.

        /* locate domain un specific user */
        oUser = NEW UserDatasetModel () .
        oUser:SmartUser:Filter:LoginCompanyGuid:EQ ("":U) .
        oUser:SmartUser:Filter:UserName:EQ (cName):Run () .

        IF oUser:SmartUser:Available THEN
            RETURN oUser:SmartUser:UserSecurityId .

        UNDO, THROW NEW Exception ("Invalid name"{&TRAN}, 0) .

    END METHOD .

END CLASS.
