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
    File        : SmartProductTableModelFilter
    Purpose     : Filter class for the SmartProductTableModel 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:24,576+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                                  FROM PROPATH . 
USING Consultingwerk.Assertion.*                                        FROM PROPATH . 
USING Consultingwerk.OERA.*                                             FROM PROPATH . 
USING Consultingwerk.OERA.ModelFilter.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.*                                                   FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Menu.ProductBusinessEntity", type="TableModelFilter") .

CLASS Consultingwerk.SmartFramework.Menu.SmartProductTableModelFilter
    INHERITS TableModelFilter: 

    DEFINE PRIVATE VARIABLE oTableModel AS SmartProductTableModel_Generated NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the ProductGuid field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ProductGuid AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("ProductGuid":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the ProductCode field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ProductCode AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("ProductCode":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the ProductName field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ProductName AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("ProductName":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the LogicalFilter for the ProductInstalled field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ProductInstalled AS LogicalFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewLogicalFilter ("ProductInstalled":U, THIS-OBJECT) .
    END GET.



    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartProductTableModelFilter class
        Notes:   
        @param poTableModel The reference to the TableModel for this Filter class
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartProductTableModelFilter (poTableModel AS SmartProductTableModel_Generated):
        SUPER (poTableModel).
        
        ObjectAssert:IsValid (poTableModel, "TableModel":U) .
        
        ASSIGN oTableModel = poTableModel.
        
    END CONSTRUCTOR.

END CLASS.
