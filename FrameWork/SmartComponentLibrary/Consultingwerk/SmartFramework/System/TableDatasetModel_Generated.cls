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
    File        : Table_Generated
    Purpose     : Auto Generated portion of the Table class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03/10/2015 11:25:58,896+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.TableBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.TableDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/System/dsTable.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartTable table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartTable AS SmartTableTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.System.TableBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TableDatasetModel_Generated ():
        SUPER (DATASET dsTable:HANDLE).

        THIS-OBJECT:SmartTable = NEW SmartTableTableModel (THIS-OBJECT, BUFFER eSmartTable:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartTable) .

    
    END CONSTRUCTOR.

  

END CLASS.
