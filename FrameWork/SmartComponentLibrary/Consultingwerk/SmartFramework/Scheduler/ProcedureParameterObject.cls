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
    File        : ProcedureParameterObject
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Fri May 27 14:59:44 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Scheduler.ProcedureParameterObject
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    :

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the parameter's sort order
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i SortOrder  INTEGER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the data type
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i ParamType  CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the the value of the parameter
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i ParamValue CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InvokeStaticMethodCallParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProcedureParameterObject ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

END CLASS.
