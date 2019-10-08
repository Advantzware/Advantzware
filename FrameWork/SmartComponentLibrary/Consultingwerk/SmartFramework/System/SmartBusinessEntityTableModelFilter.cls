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
    File        : SmartBusinessEntityTableModelFilter
    Purpose     : Filter class for the SmartBusinessEntityTableModel 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03/09/2015 14:11:07,086+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                                  FROM PROPATH . 
USING Consultingwerk.Assertion.*                                        FROM PROPATH . 
USING Consultingwerk.OERA.*                                             FROM PROPATH . 
USING Consultingwerk.OERA.ModelFilter.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.*                                                   FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity", type="TableModelFilter") .

CLASS Consultingwerk.SmartFramework.System.SmartBusinessEntityTableModelFilter
    INHERITS TableModelFilter: 

    DEFINE PRIVATE VARIABLE oTableModel AS SmartBusinessEntityTableModel_Generated NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the BusinessEntityGuid field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityGuid AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("BusinessEntityGuid":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the BusinessEntityPackage field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityPackage AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("BusinessEntityPackage":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the BusinessEntityName field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("BusinessEntityName":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the BusinessEntityPurpose field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityPurpose AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("BusinessEntityPurpose":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the LogicalFilter for the DBRequired field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DBRequired AS LogicalFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewLogicalFilter ("DBRequired":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the LogicalFilter for the TrackDeletions field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TrackDeletions AS LogicalFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewLogicalFilter ("TrackDeletions":U, THIS-OBJECT) .
    END GET.



    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityTableModelFilter class
        Notes:   
        @param poTableModel The reference to the TableModel for this Filter class
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityTableModelFilter (poTableModel AS SmartBusinessEntityTableModel_Generated):
        SUPER (poTableModel).
        
        ObjectAssert:IsValid (poTableModel, "TableModel":U) .
        
        ASSIGN oTableModel = poTableModel.
        
    END CONSTRUCTOR.

END CLASS.
