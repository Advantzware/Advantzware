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
    File        : SmartUiTranslationTableModelFilter
    Purpose     : Filter class for the SmartUiTranslationTableModel 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:31,068+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                                  FROM PROPATH . 
USING Consultingwerk.Assertion.*                                        FROM PROPATH . 
USING Consultingwerk.OERA.*                                             FROM PROPATH . 
USING Consultingwerk.OERA.ModelFilter.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.*                                                   FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.UiTranslationBusinessEntity", type="TableModelFilter") .

CLASS Consultingwerk.SmartFramework.System.SmartUiTranslationTableModelFilter
    INHERITS TableModelFilter: 

    DEFINE PRIVATE VARIABLE oTableModel AS SmartUiTranslationTableModel_Generated NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the UiTranslationGuid field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UiTranslationGuid AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("UiTranslationGuid":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the LanguageGuid field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LanguageGuid AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("LanguageGuid":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the ObjectName field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ObjectName AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("ObjectName":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the ResourceKey field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ResourceKey AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("ResourceKey":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the OriginalString field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OriginalString AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("OriginalString":U, THIS-OBJECT) .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterFilter for the TranslatedString field
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TranslatedString AS CharacterFilter NO-UNDO 
    GET:
        RETURN oTableModel:NewCharacterFilter ("TranslatedString":U, THIS-OBJECT) .
    END GET.



    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartUiTranslationTableModelFilter class
        Notes:   
        @param poTableModel The reference to the TableModel for this Filter class
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartUiTranslationTableModelFilter (poTableModel AS SmartUiTranslationTableModel_Generated):
        SUPER (poTableModel).
        
        ObjectAssert:IsValid (poTableModel, "TableModel":U) .
        
        ASSIGN oTableModel = poTableModel.
        
    END CONSTRUCTOR.

END CLASS.
