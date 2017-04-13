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
    File        : ListProcedureParameterObject
    Purpose     : List of ProcedureParameterObjects
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Mon May 30 08:00:48 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.* FROM PROPATH.
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.ListProcedureParameterObject
    INHERITS GenericList:

    {Consultingwerk/Framework/Base/GenericList.i Consultingwerk.SmartFramework.Scheduler.ProcedureParameterObject}

    /*------------------------------------------------------------------------------
        Purpose: Adds and returns a ProcedureParameterObject instance to the list based on
                 the passed parameter values
        Notes:
        @param piSortOrder  The order/position of the Parameter
        @param pcParamType  The DataType of the Parameter
        @param pcParamValue The Value of the Parameter
        @return The resulting ProcedureParameterObject instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ProcedureParameterObject Add (piSortOrder  AS INTEGER,
                                                pcParamType  AS CHARACTER,
                                                pcParamValue AS CHARACTER):

        DEFINE VARIABLE oProcedureParameterObject AS ProcedureParameterObject NO-UNDO.

        oProcedureParameterObject = NEW ProcedureParameterObject () .

        ASSIGN oProcedureParameterObject:SortOrder  = piSortOrder
               oProcedureParameterObject:ParamType  = pcParamType
               oProcedureParameterObject:ParamValue = pcParamValue .

        THIS-OBJECT:Add (oProcedureParameterObject) .

        RETURN oProcedureParameterObject .

    END METHOD .

END CLASS.

