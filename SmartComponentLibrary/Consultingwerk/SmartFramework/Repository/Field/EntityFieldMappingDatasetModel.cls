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
    File        : EntityFieldMapping
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05/11/2016 08:47:49,889+01:00
    Notes       : Custom constructors that should not be replaced when the
                  DatasetModel class is regenerated, need to be annotated
                  using the @CustomConstructor. annotation
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Field.* FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Field.EntityFieldMappingBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.Repository.Field.EntityFieldMappingDatasetModel
    INHERITS EntityFieldMappingDatasetModel_Generated:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EntityFieldMappingDatasetModel class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC EntityFieldMappingDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EntityFieldMappingDatasetModel class
        Notes:   
        @param pEntityFieldMappingGuid The filter value for the eSmartEntityFieldMapping.EntityFieldMappingGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC EntityFieldMappingDatasetModel (pEntityFieldMappingGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartEntityFieldMapping:Filter:EntityFieldMappingGuid:EQ (pEntityFieldMappingGuid) .

        THIS-OBJECT:SmartEntityFieldMapping:Filter:Run () .

    END CONSTRUCTOR.

    @CustomConstructor.
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EntityFieldMappingDatasetModel class
        Notes:   Retrieves data based on a named query
        @param pcTables The comma delimited list of the Table Names to retrieve
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC EntityFieldMappingDatasetModel (pcTables AS CHARACTER,
                                       poNamedQuery AS INamedQueryParameter):
        SUPER ().

        THIS-OBJECT:RetrieveData (pcTables, poNamedQuery) .

    END CONSTRUCTOR.

    @CustomConstructor.
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EntityFieldMappingDatasetModel class
        Notes:   Retrieves data based on a named query
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC EntityFieldMappingDatasetModel (poNamedQuery AS INamedQueryParameter):
        SUPER ().

        THIS-OBJECT:RetrieveData (poNamedQuery) .

    END CONSTRUCTOR.

END CLASS.
