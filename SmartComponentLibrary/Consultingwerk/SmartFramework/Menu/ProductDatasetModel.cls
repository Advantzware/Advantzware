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
    File        : Product
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 06/04/2014 11:15:39,496+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.ProductDatasetModel
    INHERITS ProductDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProductDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProductDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProductDatasetModel class
        Notes:   
        @param pProductGuid The filter value for the eSmartProduct.ProductGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProductDatasetModel (pProductGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartProduct:Filter:ProductGuid:EQ (pProductGuid) .

        THIS-OBJECT:SmartProduct:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
