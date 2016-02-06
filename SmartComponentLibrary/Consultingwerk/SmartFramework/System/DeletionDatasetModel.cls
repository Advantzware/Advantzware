/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : Deletion
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05/04/2015 21:35:53,072+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.DeletionBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.System.DeletionDatasetModel
    INHERITS DeletionDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DeletionDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DeletionDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DeletionDatasetModel class
        Notes:   
        @param pDeletionguid The filter value for the eSmartDeletion.Deletionguid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DeletionDatasetModel (pDeletionguid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartDeletion:Filter:Deletionguid:EQ (pDeletionguid) .

        THIS-OBJECT:SmartDeletion:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
