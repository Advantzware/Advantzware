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
    File        : RepositoryAssert
    Purpose     : Parameter validation routines for the Repository Routines
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 15 23:34:05 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Repository.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.*  FROM PROPATH.
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH.
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.RepositoryAssert:

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor for the RepositoryAssert class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED RepositoryAssert():

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Verifies the given object master guid
        Notes:
        @param pcObjectMasterGuid The object master guid to validate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ValidObjectMasterGuid (pcObjectMasterGuid AS CHARACTER):

        DEFINE VARIABLE oObjectMaster AS ObjectMasterDatasetModel NO-UNDO .

        oObjectMaster = NEW ObjectMasterDatasetModel() .

        IF NOT oObjectMaster:SmartObjectMaster:Filter:ObjectMasterGuid:EQ (pcObjectMasterGuid):CanFind() THEN

            UNDO, THROW NEW InvalidObjectMasterException (pcObjectMasterGuid) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Verifies the given object master name
        Notes:
        @param pcObjectMaster The object master name to validate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ValidObjectMasterName (pcObjectMaster AS CHARACTER):

        DEFINE VARIABLE oObjectMaster AS ObjectMasterDatasetModel NO-UNDO .

        oObjectMaster = NEW ObjectMasterDatasetModel() .

        IF NOT oObjectMaster:SmartObjectMaster:Filter:ObjectName:EQ (pcObjectMaster):CanFind() THEN

            UNDO, THROW NEW InvalidObjectMasterException (pcObjectMaster) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Verifies the given object type guid
        Notes:
        @param pcObjectTypeGuid The object type guid to validate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ValidObjectTypeGuid (pcObjectTypeGuid AS CHARACTER):

        DEFINE VARIABLE oObjectType AS ObjectTypeDatasetModel NO-UNDO .

        oObjectType = NEW ObjectTypeDatasetModel() .

        IF NOT oObjectType:SmartObjectType:Filter:ObjectTypeGuid:EQ (pcObjectTypeGuid):CanFind() THEN

            UNDO, THROW NEW InvalidObjectTypeException (pcObjectTypeGuid) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Verifies the given object type name
        Notes:
        @param pcObjectType The object type name to validate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ValidObjectTypeName (pcObjectType AS CHARACTER):

        DEFINE VARIABLE oObjectType AS ObjectTypeDatasetModel NO-UNDO .

        oObjectType = NEW ObjectTypeDatasetModel() .

        IF NOT oObjectType:SmartObjectType:Filter:ObjectTypeName:EQ (pcObjectType):CanFind() THEN

            UNDO, THROW NEW InvalidObjectTypeException (pcObjectType) .

    END METHOD.

END CLASS.
