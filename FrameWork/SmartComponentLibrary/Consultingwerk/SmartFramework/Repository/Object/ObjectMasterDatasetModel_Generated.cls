/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ObjectMaster_Generated
    Purpose     : Auto Generated portion of the ObjectMaster class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 26/05/2016 09:09:31,339+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Repository.Object.ObjectMasterDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Repository/Object/dsObjectMaster.i &ACCESS=PROTECTED }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartLink table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartLink AS SmartLinkTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartObjectInstance table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartObjectInstance AS SmartObjectInstanceTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartObjectMaster table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartObjectMaster AS SmartObjectMasterTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartPage table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartPage AS SmartPageTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectMasterDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectMasterDatasetModel_Generated ():
        SUPER (DATASET dsObjectMaster:HANDLE).

        THIS-OBJECT:SmartLink = NEW SmartLinkTableModel (THIS-OBJECT, BUFFER eSmartLink:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartLink) .

        THIS-OBJECT:SmartObjectInstance = NEW SmartObjectInstanceTableModel (THIS-OBJECT, BUFFER eSmartObjectInstance:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartObjectInstance) .

        THIS-OBJECT:SmartObjectMaster = NEW SmartObjectMasterTableModel (THIS-OBJECT, BUFFER eSmartObjectMaster:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartObjectMaster) .

        THIS-OBJECT:SmartPage = NEW SmartPageTableModel (THIS-OBJECT, BUFFER eSmartPage:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartPage) .

    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Business Entity Method ObjectIsA  
        Notes:   Receives the dataset ObjectIsA from the backend
        @param poParameter The Parameter Object for this method 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ObjectIsA (poParameter AS Consultingwerk.SmartFramework.Repository.Object.ObjectIsAParameter):
        
        DATASET dsObjectMaster:EMPTY-DATASET () .
        
        IF THIS-OBJECT:UseInterface = Consultingwerk.OERA.UseInterfaceEnum:ServiceAdapter THEN 
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                                                    THIS-OBJECT:EntityName,
                                                                                    "ObjectIsA":U,
                                                                                    INPUT-OUTPUT DATASET dsObjectMaster,
                                                                                    poParameter) .
        ELSE 
            Consultingwerk.OERA.ServiceInterface:InvokeMethod (THIS-OBJECT:EntityName,
                                                               "ObjectIsA":U,
                                                               INPUT-OUTPUT DATASET dsObjectMaster,
                                                               poParameter) .

        {Consultingwerk/foreachABL.i Consultingwerk.OERA.TableModel oTable in THIS-OBJECT:TableModels}
            IF VALID-HANDLE (oTable:QueryHandle) THEN DO:
                oTable:QueryHandle:QUERY-OPEN () .
                oTable:QueryHandle:GET-FIRST () .
            END. 
        END.
        
    END METHOD .

  

END CLASS.
