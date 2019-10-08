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
    File        : ModifiedState
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31/12/2016 00:59:18,677+01:00
    Notes       : Custom constructors that should not be replaced when the  
                  DatasetModel class is regenerated, need to be annotated 
                  using the @CustomConstructor. annotation
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.ModifiedStateBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.System.ModifiedStateDatasetModel
    INHERITS ModifiedStateDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ModifiedStateDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModifiedStateDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ModifiedStateDatasetModel class
        Notes:   
        @param pModifiedStateGuid The filter value for the eSmartModifiedState.ModifiedStateGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModifiedStateDatasetModel (pModifiedStateGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartModifiedState:Filter:ModifiedStateGuid:EQ (pModifiedStateGuid) .

        THIS-OBJECT:SmartModifiedState:Filter:Run () .

    END CONSTRUCTOR.

    @CustomConstructor.
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ModifiedStateDatasetModel class
        Notes:   Retrieves data based on a named query   
        @param pcTables The comma delimited list of the Table Names to retrieve
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModifiedStateDatasetModel (pcTables AS CHARACTER,
                                       poNamedQuery AS INamedQueryParameter):
        SUPER ().

        THIS-OBJECT:RetrieveData (pcTables, poNamedQuery) .

    END CONSTRUCTOR.

    @CustomConstructor.
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ModifiedStateDatasetModel class
        Notes:   Retrieves data based on a named query   
        @param poNamedQuery The INamedQueryParameter that defines a named query
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModifiedStateDatasetModel (poNamedQuery AS INamedQueryParameter):
        SUPER ().

        THIS-OBJECT:RetrieveData (poNamedQuery) .

    END CONSTRUCTOR.

END CLASS.
