/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ObjectDefNames_Generated
    Purpose     : Auto Generated portion of the ObjectDefNames class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 03/11/2013 13:19:56,319+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.RollbaseAdapter.MetaData.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.RollbaseAdapter.MetaData.ObjectDefNamesDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/RollbaseAdapter/MetaData/dsObjectDefNames.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the ObjectDefName table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ObjectDefName AS ObjectDefNameTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.RollbaseAdapter.MetaData.ObjectDefNamesBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectDefNamesDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectDefNamesDatasetModel_Generated ():
        SUPER (DATASET dsObjectDefNames:HANDLE).

        THIS-OBJECT:ObjectDefName = NEW ObjectDefNameTableModel (THIS-OBJECT, BUFFER eObjectDefName:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:ObjectDefName) .

       
    END CONSTRUCTOR.

END CLASS.
