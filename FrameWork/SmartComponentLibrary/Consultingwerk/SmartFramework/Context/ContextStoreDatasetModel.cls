/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ContextStore
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 25/02/2016 08:34:02,071+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Context.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Context.ContextStoreBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.Context.ContextStoreDatasetModel
    INHERITS ContextStoreDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ContextStoreDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ContextStoreDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ContextStoreDatasetModel class
        Notes:   
        @param pContextStoreGUID The filter value for the eSmartContextStore.ContextStoreGUID field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ContextStoreDatasetModel (pContextStoreGUID AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartContextStore:Filter:ContextStoreGUID:EQ (pContextStoreGUID) .

        THIS-OBJECT:SmartContextStore:Filter:Run () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a ContextStoreDatasetModel filtered by a SessionID
        Notes:   Alternative to the search construcor which is by ContextStoreGUID 
        @param pcSessionID The user name to search for
        @return A ContextStoreDatasetModel instance filtered by the SessionID  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ContextStoreDatasetModel FromSessionID (pcSessionID AS CHARACTER):
        
        DEFINE VARIABLE oModel AS ContextStoreDatasetModel NO-UNDO . 
        
        oModel = NEW ContextStoreDatasetModel () . 
        
        oModel:SmartContextStore:Filter:SessionID:EQ (pcSessionID) .
        oModel:SmartContextStore:Filter:Run () .

        RETURN oModel .

    END METHOD.

END CLASS.
