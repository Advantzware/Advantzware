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
    File        : GroupBusinessEntity
    Purpose     : Business Entity for Group
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories
    Created     : 11.10.2012 00:36:35
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Exceptions.*    FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.Import.*  FROM PROPATH.
USING Progress.Lang.*                               FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .

@BusinessEntityTable (name="eSmartGroup", MandatoryColumns="GroupName", DefaultSort="BY eSmartGroup.GroupName") .

CLASS Consultingwerk.SmartFramework.Authorization.GroupBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/Authorization/dsGroup.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the GroupBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GroupBusinessEntity ():
        SUPER (DATASET dsGroup:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Authorization.GroupDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Gets groups parent groups
        Notes:
        @param dsGroup INPUT-OUTPUT The dataset of the group business entity
        @param poParameter The GetParentGroupsParameter with the data for this request
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetParentGroups (INPUT-OUTPUT DATASET dsGroup,
                                        poParameter AS GetParentGroupsParameter):

        DEFINE VARIABLE oGroup   AS GroupDatasetModel NO-UNDO .
        DEFINE VARIABLE cParent  AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE cParents AS CHARACTER         NO-UNDO .

        oGroup = NEW GroupDatasetModel (poParameter:GroupGuid) .
        oGroup:UseDedicatedBusinessEntity = TRUE .

        IF NOT oGroup:SmartGroup:Available THEN
            RETURN .

        cParent = oGroup:SmartGroup:ParentGroupGuid .

        DO WHILE cParent > "":U ON ERROR UNDO, THROW:

            oGroup = NEW GroupDatasetModel (cParent) .
            oGroup:UseDedicatedBusinessEntity = TRUE .

            IF NOT oGroup:SmartGroup:Available THEN
                LEAVE .

            ASSIGN cParent  = oGroup:SmartGroup:ParentGroupGuid
                   cParents = cParents + ",":U + oGroup:SmartGroup:ParentGroupGuid .

            IF ListHelper:EntryIsInList (poParameter:GroupGuid, cParent) THEN
                UNDO, THROW NEW CircularGroupReferenceException ("Detected a group which is it's own anchestor."{&TRAN}, 0) .

        END.

        ASSIGN poParameter:ParentGroupGuids = cParents .

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

        /* Mike Fechner, Consultingwerk Ltd. 09.05.2016
           Ignore validation while importing data */
        IF GenericDataImporter:Importing THEN
            RETURN .

        FOR EACH eSmartGroup ON ERROR UNDO, THROW:
            FIND eSmartGroupBefore WHERE ROWID(eSmartGroupBefore) = BUFFER eSmartGroup:BEFORE-ROWID NO-ERROR.

            IF AVAILABLE eSmartGroupBefore THEN DO:

                IF NUM-ENTRIES (eSmartGroup.GroupName, ",":U) > 1 THEN
                    Consultingwerk.Util.DatasetHelper:SetErrorString
                        (BUFFER eSmartGroup:HANDLE,
                         "Groupname":U + CHR (1) + "It is not allowed to use a comma in the group name!"{&TRAN}) .
            END.

            IF eSmartGroup.ParentGroupGuid > "":U AND eSmartGroup.GroupGuid = eSmartGroup.ParentGroupGuid THEN
                DatasetHelper:AddErrorString (BUFFER eSmartGroup:HANDLE,
                                              "A group cannot be it's own parent group."{&TRAN},
                                              "ParentGroupGuid":U) .
        END.

    END METHOD.

END CLASS.
