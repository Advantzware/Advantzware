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
    File        : ObjectDefNames
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 03/11/2013 13:19:56,321+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.RollbaseAdapter.MetaData.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.RollbaseAdapter.MetaData.ObjectDefNamesDatasetModel
    INHERITS ObjectDefNamesDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectDefNamesDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectDefNamesDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectDefNamesDatasetModel class
        Notes:   
        @param pObjectDefName The filter value for the eObjectDefName.ObjectDefName field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectDefNamesDatasetModel (pObjectDefName AS CHARACTER):
        SUPER ().

        THIS-OBJECT:ObjectDefName:Filter:ObjectDefName:EQ (pObjectDefName) .

        THIS-OBJECT:ObjectDefName:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
