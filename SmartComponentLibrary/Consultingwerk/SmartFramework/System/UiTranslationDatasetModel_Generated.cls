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
    File        : UiTranslation_Generated
    Purpose     : Auto Generated portion of the UiTranslation class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:31,471+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.UiTranslationBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.UiTranslationDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/System/dsUiTranslation.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartUiTranslation table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartUiTranslation AS SmartUiTranslationTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.System.UiTranslationBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UiTranslationDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UiTranslationDatasetModel_Generated ():
        SUPER (DATASET dsUiTranslation:HANDLE).

        THIS-OBJECT:SmartUiTranslation = NEW SmartUiTranslationTableModel (THIS-OBJECT, BUFFER eSmartUiTranslation:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartUiTranslation) .

    
    END CONSTRUCTOR.

  

END CLASS.
