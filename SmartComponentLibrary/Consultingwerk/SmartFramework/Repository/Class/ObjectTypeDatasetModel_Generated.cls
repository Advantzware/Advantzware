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
    File        : ObjectType_Generated
    Purpose     : Auto Generated portion of the ObjectType class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 16/10/2015 20:01:10,932+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Repository.Class.ObjectTypeDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Repository/Class/dsObjectType.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartObjectType table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartObjectType AS SmartObjectTypeTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectTypeDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectTypeDatasetModel_Generated ():
        SUPER (DATASET dsObjectType:HANDLE).

        THIS-OBJECT:SmartObjectType = NEW SmartObjectTypeTableModel (THIS-OBJECT, BUFFER eSmartObjectType:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartObjectType) .

    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Business Entity Method GetParentClasses  
        Notes:   Receives the dataset GetParentClasses from the backend
        @param poParameter The Parameter Object for this method 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetParentClasses (poParameter AS GetParentClassesParameter):
        
        DATASET dsObjectType:EMPTY-DATASET () .
        
        IF THIS-OBJECT:UseInterface = Consultingwerk.OERA.UseInterfaceEnum:ServiceAdapter THEN 
            Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                                                    THIS-OBJECT:EntityName,
                                                                                    "GetParentClasses":U,
                                                                                    INPUT-OUTPUT DATASET dsObjectType,
                                                                                    poParameter) .
        ELSE 
            Consultingwerk.OERA.ServiceInterface:InvokeMethod (THIS-OBJECT:EntityName,
                                                               "GetParentClasses":U,
                                                               INPUT-OUTPUT DATASET dsObjectType,
                                                               poParameter) .

        {Consultingwerk/foreachABL.i Consultingwerk.OERA.TableModel oTable in THIS-OBJECT:TableModels}
            IF VALID-HANDLE (oTable:QueryHandle) THEN DO:
                oTable:QueryHandle:QUERY-OPEN () .
                oTable:QueryHandle:GET-FIRST () .
            END. 
        END.
        
    END METHOD .

  

END CLASS.
