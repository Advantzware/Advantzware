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
    File        : Relation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:49:03,542+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.RelationDatasetModel
    INHERITS RelationDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RelationDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RelationDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RelationDatasetModel class
        Notes:   
        @param pRelationGuid The filter value for the eSmartRelation.RelationGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RelationDatasetModel (pRelationGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartRelation:Filter:RelationGuid:EQ (pRelationGuid) .

        THIS-OBJECT:SmartRelation:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
