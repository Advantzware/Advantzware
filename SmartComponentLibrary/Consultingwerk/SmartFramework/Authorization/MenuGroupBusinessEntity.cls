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
    File        : MenuGroupBusinessEntity
    Purpose     : Business Entity for MenuGroup
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories
    Created     : 11.10.2012 00:37:21
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

@BusinessEntityTable (name="eSmartMenuGroup", MandatoryColumns="GroupName", DefaultSort="BY eSmartMenuGroup.GroupName") .

CLASS Consultingwerk.SmartFramework.Authorization.MenuGroupBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/Authorization/dsMenuGroup.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuGroupBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuGroupBusinessEntity ():
        SUPER (DATASET dsMenuGroup:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Authorization.MenuGroupDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of Menu Guid's for the list of user groups
        Notes:
        @param dsMenuGroup The Dataset with the menu groups
        @param poGroups The CharacterHolder class that contains the comma delimited list of user groups
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetUserGroupMenus (INPUT-OUTPUT DATASET dsMenuGroup,
                                          poGroups AS CharacterHolder):

        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE cQuery   AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER          NO-UNDO .

        DATASET dsMenuGroup:EMPTY-DATASET () .

        IF NUM-ENTRIES (poGroups:Value) = 0 THEN
            RETURN .

        ASSIGN cQuery = "FOR EACH eSmartMenuGroup WHERE ":U .

        DO i = 1 TO NUM-ENTRIES (poGroups:Value):

            IF i > 1 THEN
                cQuery = cQuery + " OR ":U .

            ASSIGN cQuery = cQuery + SUBSTITUTE ("eSmartMenuGroup.GroupGuid = &1":U,
                                                 QUOTER (ENTRY (i, poGroups:Value))) .
        END.

        oRequest = NEW FetchDataRequest ("eSmartMenuGroup":U,
                                         cQuery) .

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

    END METHOD.

END CLASS.
