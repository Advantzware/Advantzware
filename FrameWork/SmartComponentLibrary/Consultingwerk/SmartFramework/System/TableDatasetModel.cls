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
    File        : Table
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:49:51,253+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.TableDatasetModel
    INHERITS TableDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TableDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableDatasetModel class
        Notes:   
        @param pTableGUID The filter value for the eSmartTable.TableGUID field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TableDatasetModel (pTableGUID AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartTable:Filter:TableGUID:EQ (pTableGUID) .

        THIS-OBJECT:SmartTable:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
