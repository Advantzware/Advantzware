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
    File        : ObjectMaster
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 15/05/2016 23:17:24,078+02:00
    Notes       : Custom constructors that should not be replaced when the
                  DatasetModel class is regenerated, need to be annotated
                  using the @CustomConstructor. annotation
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.Repository.Object.ObjectMasterDatasetModel
    INHERITS ObjectMasterDatasetModel_Generated:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectMasterDatasetModel class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectMasterDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectMasterDatasetModel class
        Notes:   
        @param pObjectMasterGuid The filter value for the eSmartObjectMaster.ObjectMasterGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectMasterDatasetModel (pObjectMasterGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartObjectMaster:Filter:ObjectMasterGuid:EQ (pObjectMasterGuid) .

        THIS-OBJECT:SmartObjectMaster:Filter:Run () .

    END CONSTRUCTOR.

    @CustomConstructor.
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectMasterDatasetModel class
        Notes:   Retrieves data based on a named query
        @param pcTables The comma delimited list of the Table Names to retrieve
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectMasterDatasetModel (pcTables AS CHARACTER,
                                                 poNamedQuery AS INamedQueryParameter):
        SUPER ().

        THIS-OBJECT:RetrieveData (pcTables, poNamedQuery) .

    END CONSTRUCTOR.

    @CustomConstructor.
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectMasterDatasetModel class
        Notes:   Retrieves data based on a named query
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectMasterDatasetModel (poNamedQuery AS INamedQueryParameter):
        SUPER ().

        THIS-OBJECT:RetrieveData (poNamedQuery) .

    END CONSTRUCTOR.

END CLASS.
