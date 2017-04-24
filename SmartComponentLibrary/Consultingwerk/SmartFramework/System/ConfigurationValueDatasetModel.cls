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
    File        : ConfigurationValue
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 07/01/2017 12:05:55,715+01:00
    Notes       : Custom constructors that should not be replaced when the
                  DatasetModel class is regenerated, need to be annotated
                  using the @CustomConstructor. annotation
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.ConfigurationValueBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.System.ConfigurationValueDatasetModel
    INHERITS ConfigurationValueDatasetModel_Generated:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ConfigurationValueDatasetModel class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ConfigurationValueDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ConfigurationValueDatasetModel class
        Notes:
        @param pConfigurationValueGuid The filter value for the eSmartConfigurationValue.ConfigurationValueGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ConfigurationValueDatasetModel (pConfigurationValueGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartConfigurationValue:Filter:ConfigurationValueGuid:EQ (pConfigurationValueGuid) .

        THIS-OBJECT:SmartConfigurationValue:Filter:Run () .

    END CONSTRUCTOR.

    @CustomConstructor.
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ConfigurationValueDatasetModel class
        Notes:   Retrieves data based on a named query
        @param pcTables The comma delimited list of the Table Names to retrieve
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ConfigurationValueDatasetModel (pcTables AS CHARACTER,
                                                       poNamedQuery AS INamedQueryParameter):
        SUPER ().

        THIS-OBJECT:RetrieveData (pcTables, poNamedQuery) .

    END CONSTRUCTOR.

    @CustomConstructor.
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ConfigurationValueDatasetModel class
        Notes:   Retrieves data based on a named query
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ConfigurationValueDatasetModel (poNamedQuery AS INamedQueryParameter):
        SUPER ().

        THIS-OBJECT:RetrieveData (poNamedQuery) .

    END CONSTRUCTOR.

    /**
     * Purpose: Returns a ConfigurationValueDatasetModule filtered
     *          by the name vlaue
     * Notes:
     * @param pcName The name of the configuration value to return
     * @return The ConfigurationValueDatasetModel
     */
    METHOD PUBLIC STATIC ConfigurationValueDatasetModel ByName (pcName AS CHARACTER):

        DEFINE VARIABLE oModel AS ConfigurationValueDatasetModel NO-UNDO .

        oModel = NEW ConfigurationValueDatasetModel () .

        oModel:SmartConfigurationValue:Filter:ConfigurationValueName:EQ (pcName):Run () .

        RETURN oModel .

    END METHOD.

END CLASS.
