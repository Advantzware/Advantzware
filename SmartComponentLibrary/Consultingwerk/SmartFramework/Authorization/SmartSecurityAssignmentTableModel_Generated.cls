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
    File        : SmartSecurityAssignmentTableModel_Generated
    Purpose     : Auto Generated portion of the SmartSecurityAssignmentTableModel class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:19,071+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authorization.SecurityAssignmentBusinessEntity", type="TableModelGenerated") .

CLASS Consultingwerk.SmartFramework.Authorization.SmartSecurityAssignmentTableModel_Generated 
    INHERITS TableModel ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SecurityAssignmentGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityAssignmentGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SecurityAssignmentGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SecurityAssignmentGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SecurityRealmGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityRealmGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SecurityRealmGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SecurityRealmGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SecurityItemGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityItemGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SecurityItemGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SecurityItemGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current GroupGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY GroupGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("GroupGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("GroupGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current UserGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UserGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("UserGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("UserGuid":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current Restricted value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Restricted AS LOGICAL NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetLogicalFieldValue ("Restricted":U) .
    END GET . 
    PUBLIC SET (arg AS LOGICAL):
        THIS-OBJECT:SetLogicalFieldValue ("Restricted":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SecurityRealmCode value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityRealmCode AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SecurityRealmCode":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SecurityRealmCode":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SecurityRealmDescription value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityRealmDescription AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SecurityRealmDescription":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SecurityRealmDescription":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current SecurityItemDescription value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityItemDescription AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("SecurityItemDescription":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("SecurityItemDescription":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current OwnerName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OwnerName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("OwnerName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("OwnerName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current OwnerType value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OwnerType AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("OwnerType":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("OwnerType":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current GroupName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY GroupName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("GroupName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("GroupName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current UserName value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UserName AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("UserName":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("UserName":U, arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current LoginCompanyGuid value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LoginCompanyGuid AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:GetCharacterFieldValue ("LoginCompanyGuid":U) .
    END GET . 
    PUBLIC SET (arg AS CHARACTER):
        THIS-OBJECT:SetCharacterFieldValue ("LoginCompanyGuid":U, arg) .
    END SET .



    /*------------------------------------------------------------------------------
        Purpose: Returns the Filter Class for this TableModel
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filter AS SmartSecurityAssignmentTableModelFilter NO-UNDO 
    GET:
        DEFINE VARIABLE oFilter AS SmartSecurityAssignmentTableModelFilter NO-UNDO.
        
        oFilter = NEW SmartSecurityAssignmentTableModelFilter (THIS-OBJECT) .
        
        &IF PROVERSION BEGINS "10" &THEN
        ASSIGN THIS-OBJECT:SavedFilterReference = oFilter .
        &ENDIF
        
        RETURN oFilter .      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSecurityAssignmentTableModel_Generated class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSecurityAssignmentTableModel_Generated (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSecurityAssignmentTableModel_Generated class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartSecurityAssignmentTableModel on which this SmartSecurityAssignmentTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED SmartSecurityAssignmentTableModel_Generated (poTableModel AS SmartSecurityAssignmentTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartSecurityAssignmentTableModel as a view model for the current record
       Notes:
       @return The SmartSecurityAssignmentTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartSecurityAssignmentTableModel CreateCurrentRecordView ():
        
        RETURN CAST (THIS-OBJECT:CreateCurrentRecordViewInternal (), 
                     SmartSecurityAssignmentTableModel) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Creates a new instance of the SmartSecurityAssignmentTableModel as a view model
       Notes:
       @return The SmartSecurityAssignmentTableModel as the view model
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartSecurityAssignmentTableModel CreateView ():
        
        RETURN CAST (THIS-OBJECT:CreateViewInternal (), 
                     SmartSecurityAssignmentTableModel) . 
        
    END METHOD.
    
END CLASS.
