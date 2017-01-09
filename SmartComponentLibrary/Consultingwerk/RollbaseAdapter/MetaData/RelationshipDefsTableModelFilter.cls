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
    File        : RelationshipDefsTableModelFilter
    Purpose     : Filter class for the RelationshipDefsTableModel 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05/09/2014 11:58:35,112+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                                  FROM PROPATH . 
USING Consultingwerk.Assertion.*                                        FROM PROPATH . 
USING Consultingwerk.OERA.*                                             FROM PROPATH . 
USING Consultingwerk.OERA.ModelFilter.*                                 FROM PROPATH .
USING Consultingwerk.RollbaseAdapter.MetaData.* FROM PROPATH .  
USING Progress.Lang.*                                                   FROM PROPATH .

CLASS Consultingwerk.RollbaseAdapter.MetaData.RelationshipDefsTableModelFilter
    INHERITS TableModelFilter: 

    DEFINE PRIVATE VARIABLE oTableModel AS RelationshipDefsTableModel_Generated NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the parentid field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY parentid AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("parentid":U, THIS-OBJECT) .
    END GET.



    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RelationshipDefsTableModelFilter class
        Notes:   
        @param poTableModel The reference to the TableModel for this Filter class
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RelationshipDefsTableModelFilter (poTableModel AS RelationshipDefsTableModel_Generated):
        SUPER (poTableModel).
        
        ObjectAssert:IsValid (poTableModel, "TableModel":U) .
        
        ASSIGN oTableModel = poTableModel.
        
    END CONSTRUCTOR.

END CLASS.
