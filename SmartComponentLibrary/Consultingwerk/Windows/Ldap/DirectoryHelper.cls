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
    File        : DirectoryHelper
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Feb 12 14:00:52 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*         FROM PROPATH .
USING Consultingwerk.Windows.Ldap.* FROM PROPATH .
USING System.Collections.*          FROM ASSEMBLY .
USING System.DirectoryServices.*    FROM ASSEMBLY .
USING System.Security.Principal.*   FROM ASSEMBLY .
USING Progress.Lang.*               FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Ldap.DirectoryHelper:

    /*------------------------------------------------------------------------------
        Purpose: Returns the current authentication type
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY CurrentAuthenticationType AS CHARACTER NO-UNDO
    GET():
        DEFINE VARIABLE id AS WindowsIdentity NO-UNDO .

        id = WindowsIdentity:GetCurrent() .

        IF VALID-OBJECT(id) THEN
            RETURN id:AuthenticationType .
        ELSE RETURN ? .

        FINALLY:
            id:Dispose() .
        END FINALLY.
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the domain name of the current windows itentidy
        Notes:
    ------------------------------------------------------------------------------*/
     DEFINE PUBLIC STATIC PROPERTY CurrentUserDomain AS CHARACTER NO-UNDO
    GET():
        DEFINE VARIABLE id AS WindowsIdentity NO-UNDO .

        id = WindowsIdentity:GetCurrent() .

        IF VALID-OBJECT(id) THEN
            RETURN ENTRY(1, id:Name, "~\":U) .
        ELSE RETURN ? .

        FINALLY:
            id:Dispose() .
        END FINALLY.
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the user name of the current windows itentity
        Notes:
    ------------------------------------------------------------------------------*/
     DEFINE PUBLIC STATIC PROPERTY CurrentUserName AS CHARACTER NO-UNDO
    GET():
        DEFINE VARIABLE id AS WindowsIdentity NO-UNDO .

        id = WindowsIdentity:GetCurrent() .

        IF VALID-OBJECT(id) THEN
            RETURN ENTRY(2, id:Name, "~\":U) .
        ELSE RETURN ? .

        FINALLY:
            id:Dispose() .
        END FINALLY.
    END GET.

     /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE DirectoryHelper ():

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the distinguishedName or ObjectGUID of an object contained
                 in an ActiveDirectory.
                 If the returned value is ? means that no Object with the given
                 name has been found in the AD.
        @param poObjectClass Indicates what type of object is looked up. Possible values are "User, Group, Computer"
        @param cObjectName The object name (User Name)
        @param cLdapDomain This is the AD Domain for example "consultingwerk"
        @return The Objects Dsitinguished Name of ? when the object has not been found
    ------------------------------------------------------------------------------*/
    METHOD STATIC PUBLIC CHARACTER GetObjectGuid (poObjectClass AS ObjectClassEnum,
                                                  cObjectName AS CHARACTER,
                                                  cLdapDomain AS CHARACTER):

        DEFINE VARIABLE cConnectionPrefix  AS CHARACTER NO-UNDO INITIAL "LDAP://":U.

        DEFINE VARIABLE oSearcher          AS DirectorySearcher       NO-UNDO.
        DEFINE VARIABLE oSearchResult      AS SearchResult            NO-UNDO.
        DEFINE VARIABLE oEntry             AS DirectoryEntry          NO-UNDO.
        DEFINE VARIABLE oDirectoryEntry    AS DirectoryEntry          NO-UNDO.

        ASSIGN cConnectionPrefix = cConnectionPrefix + cLdapDomain.
        ASSIGN oEntry            = NEW DirectoryEntry (cConnectionPrefix).
        ASSIGN oSearcher         = NEW DirectorySearcher(oEntry).

        CASE poObjectClass:
            WHEN ObjectClassEnum:User THEN
                oSearcher:Filter = SUBSTITUTE ("(&&(objectClass=user)(|(cn=&1)(sAMAccountName=&1)))":U,
                                               cObjectName) .
            WHEN ObjectClassEnum:Group THEN
                oSearcher:Filter = SUBSTITUTE ("(&&(objectClass=group)(|(cn=&1)(sAMAccountName=&1)))":U,
                                               cObjectName) .
            WHEN ObjectClassEnum:Computer THEN
                oSearcher:Filter = SUBSTITUTE ("(&&(objectClass=computer)(|(cn=&1)(sAMAccountName=&1)))":U,
                                               cObjectName) .
        END CASE.

        oSearchResult = oSearcher:FindOne().

        /* If there is no valid Result for that search leave with unknown value. */
        IF NOT VALID-OBJECT(oSearchResult) THEN
            RETURN ?.

        oDirectoryEntry = oSearchResult:GetDirectoryEntry().

        RETURN oDirectoryEntry:Guid:ToString () .

        FINALLY:
            IF VALID-OBJECT (oEntry) THEN DO:
                oEntry:Close().
                oEntry:Dispose().
                GarbageCollectorHelper:DeleteObject (oEntry).
            END.

            IF VALID-OBJECT (oDirectoryEntry) THEN DO:
                oDirectoryEntry:Close().
                oDirectoryEntry:Dispose().
                GarbageCollectorHelper:DeleteObject (oDirectoryEntry).
            END.

            IF VALID-OBJECT (oSearcher) THEN DO:
                oSearcher:Dispose().
                GarbageCollectorHelper:DeleteObject (oSearcher).
            END.
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the distinguishedName or ObjectGUID of an object contained
                 in an ActiveDirectory.
                 If the returned value is ? means that no Object with the given
                 name has been found in the AD.
        @param poObjectClass Indicates what type of object is looked up. Possible values are "User, Group, Computer"
        @param cObjectName The object name (User Name)
        @param cLdapDomain This is the AD Domain for example "consultingwerk"
        @return The Objects Dsitinguished Name of ? when the object has not been found
    ------------------------------------------------------------------------------*/
    METHOD STATIC PUBLIC CHARACTER GetObjectDistinguishedName (poObjectClass AS ObjectClassEnum,
                                                               cObjectName AS CHARACTER,
                                                               cLdapDomain AS CHARACTER):

        DEFINE VARIABLE cDistinguishedName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cConnectionPrefix  AS CHARACTER NO-UNDO INITIAL "LDAP://":U.

        DEFINE VARIABLE oSearcher          AS DirectorySearcher       NO-UNDO.
        DEFINE VARIABLE oSearchResult      AS SearchResult            NO-UNDO.
        DEFINE VARIABLE oEntry             AS DirectoryEntry          NO-UNDO.
        DEFINE VARIABLE oDirectoryEntry    AS DirectoryEntry          NO-UNDO.
        DEFINE VARIABLE oValueCollection   AS PropertyValueCollection NO-UNDO .
        DEFINE VARIABLE en                 AS IEnumerator             NO-UNDO .

        ASSIGN cConnectionPrefix = cConnectionPrefix + cLdapDomain.
        ASSIGN oEntry            = NEW DirectoryEntry (cConnectionPrefix).
        ASSIGN oSearcher         = NEW DirectorySearcher(oEntry).

        CASE poObjectClass:
            WHEN ObjectClassEnum:User THEN
                oSearcher:Filter = SUBSTITUTE ("(&&(objectClass=user)(|(cn=&1)(sAMAccountName=&1)))":U,
                                               cObjectName) .
            WHEN ObjectClassEnum:Group THEN
                oSearcher:Filter = SUBSTITUTE ("(&&(objectClass=group)(|(cn=&1)(sAMAccountName=&1)))":U,
                                               cObjectName) .
            WHEN ObjectClassEnum:Computer THEN
                oSearcher:Filter = SUBSTITUTE ("(&&(objectClass=computer)(|(cn=&1)(sAMAccountName=&1)))":U,
                                               cObjectName) .
        END CASE.

        oSearchResult = oSearcher:FindOne().

        /* If there is no valid Result for that search leave with unknown value. */
        IF NOT VALID-OBJECT(oSearchResult) THEN
            RETURN ?.

        oDirectoryEntry = oSearchResult:GetDirectoryEntry().

        oValueCollection = oDirectoryEntry:Properties ["distinguishedName":U].
        en = oValueCollection:GetEnumerator().
        en:Reset () .

        DO WHILE en:MoveNext() :
            IF NOT VALID-OBJECT(en) THEN
                NEXT.
            ASSIGN cDistinguishedName = "LDAP://":U + en:Current:ToString().
        END.

        RETURN cDistinguishedName.

        FINALLY:
            IF VALID-OBJECT (oEntry) THEN DO:
                oEntry:Close().
                oEntry:Dispose().
                GarbageCollectorHelper:DeleteObject (oEntry).
            END.

            IF VALID-OBJECT (oDirectoryEntry) THEN DO:
                oDirectoryEntry:Close().
                oDirectoryEntry:Dispose().
                GarbageCollectorHelper:DeleteObject (oDirectoryEntry).
            END.

            IF VALID-OBJECT (oSearcher) THEN DO:
                oSearcher:Dispose().
                GarbageCollectorHelper:DeleteObject (oSearcher).
            END.
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a comma seperated list of groups the given user is
                 assigned to
        Notes:
        @param pcUserDN Use the Output of the method GetObjectDistinguishedName to be sure you have a working DN
        @return The comma delimited list of groups the user is assigned to
    ------------------------------------------------------------------------------*/
    METHOD STATIC PUBLIC CHARACTER GetGroups (INPUT pcUserDN AS CHARACTER):

        DEFINE VARIABLE oEntry             AS DirectoryEntry          NO-UNDO .
        DEFINE VARIABLE ValueCollection    AS PropertyValueCollection NO-UNDO .
        DEFINE VARIABLE en                 AS IEnumerator             NO-UNDO .
        DEFINE VARIABLE cGroups            AS CHARACTER               NO-UNDO .

        ASSIGN oEntry = NEW DirectoryEntry (pcUserDN).

        ASSIGN ValueCollection = oEntry:Properties ["memberOf":U].
        ASSIGN en = ValueCollection:GetEnumerator().

        en:Reset () .

        DO WHILE en:MoveNext() :
            IF NOT VALID-OBJECT(en) THEN
                NEXT.

            ASSIGN cGroups = cGroups + ENTRY(2, ENTRY(1, en:Current:ToString(), ",":U), "=":U) + ",":U.

        END.

        ASSIGN cGroups = TRIM(cGroups, ",":U).

        RETURN cGroups .

        FINALLY:
            IF VALID-OBJECT(oEntry) THEN DO:
                oEntry:Close().
                oEntry:Dispose().
                DELETE OBJECT(oEntry).
            END.
        END FINALLY.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a delimited-list of property value(s) for the given
                 Diretory Entry
        Notes:   For a list of properties, see http://www.selfadsi.de/user-attributes.htm
        @param poDirectoryEntry The directory entry
        @param pcPropertyName The name of the property to return the value from
        @param pcSeparator The list delimiter to use
        @return The property value
    ------------------------------------------------------------------------------*/
    METHOD STATIC PUBLIC CHARACTER GetPropertyValues (poDirectoryEntry AS DirectoryEntry,
                                                      pcPropertyName AS CHARACTER,
                                                      pcSeparator    AS CHARACTER):

        DEFINE VARIABLE cRet             AS CHARACTER               NO-UNDO.
        DEFINE VARIABLE oValueCollection AS PropertyValueCollection NO-UNDO .
        DEFINE VARIABLE oen              AS IEnumerator             NO-UNDO .

        oValueCollection = poDirectoryEntry:Properties[pcPropertyName].
        oen = oValueCollection:GetEnumerator().
        oen:Reset() .

        DO WHILE oen:MoveNext() :
            IF VALID-OBJECT(oen:Current) THEN
                ASSIGN cRet = cRet + pcSeparator  +
                              oen:Current:ToString().
        END.

        RETURN TRIM(cRet, pcSeparator) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a delimited-list of property value(s) for the given
                 Diretory Entry
        Notes:   Uses , as default delimiter
                 For a list of properties, see http://www.selfadsi.de/user-attributes.htm
        @param poDirectoryEntry The directory entry
        @param pcPropertyName The name of the property to return the value from
        @return The property value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetPropertyValues (poDirectoryEntry AS DirectoryEntry,
                                                      pcPropertyName AS CHARACTER):

        RETURN GetPropertyValues (poDirectoryEntry,
                                  pcPropertyName,
                                  ",":U) .
    END METHOD.

END CLASS.
