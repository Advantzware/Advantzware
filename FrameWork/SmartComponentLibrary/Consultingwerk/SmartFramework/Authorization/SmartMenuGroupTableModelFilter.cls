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
    File        : SmartMenuGroupTableModelFilter
    Purpose     : Filter class for the SmartMenuGroupTableModel 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:18,846+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                                  FROM PROPATH . 
USING Consultingwerk.Assertion.*                                        FROM PROPATH . 
USING Consultingwerk.OERA.*                                             FROM PROPATH . 
USING Consultingwerk.OERA.ModelFilter.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.*                                                   FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authorization.MenuGroupBusinessEntity", type="TableModelFilter") .

CLASS Consultingwerk.SmartFramework.Authorization.SmartMenuGroupTableModelFilter
    INHERITS TableModelFilter: 

    DEFINE PRIVATE VARIABLE oTableModel AS SmartMenuGroupTableModel_Generated NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the MenuGroupGuid field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuGroupGuid AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("MenuGroupGuid":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the MenuGuid field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuGuid AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("MenuGuid":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the GroupGuid field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY GroupGuid AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("GroupGuid":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the GroupName field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY GroupName AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("GroupName":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the LoginCompanyName field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyName AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("LoginCompanyName":U, THIS-OBJECT) .
    END GET.



    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartMenuGroupTableModelFilter class
        Notes:   
        @param poTableModel The reference to the TableModel for this Filter class
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenuGroupTableModelFilter (poTableModel AS SmartMenuGroupTableModel_Generated):
        SUPER (poTableModel).
        
        ObjectAssert:IsValid (poTableModel, "TableModel":U) .
        
        ASSIGN oTableModel = poTableModel.
        
    END CONSTRUCTOR.

END CLASS.
