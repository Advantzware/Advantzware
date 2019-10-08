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
    File        : ProcedureSchedulerJobCommand
    Purpose     : Executes a Procedure from the Scheduler
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 16:51:43 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH.
USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Exceptions.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Scheduler.ProcedureSchedulerJobCommand
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS ISchedulerJobCommand
    USE-WIDGET-POOL:

    {Consultingwerk/JsonSerializableProperty.i ProcedureFileName  CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ProcedureParameter Consultingwerk.SmartFramework.Scheduler.ListProcedureParameterObject} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Editor Control used by the Function maintenance
                 Viewer
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY EditorControl AS CHARACTER NO-UNDO INIT "Consultingwerk.Windows.Framework.Scheduler.ProcedureSchedulerJobCommandControl":U
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the image representing this Function Call Parameter
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageKey AS CHARACTER INITIAL "Consultingwerk/Windows/Framework/Images/SchedulerJobCommand/code_colored.png":U NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InvokeStaticMethodCallParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProcedureSchedulerJobCommand ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        THIS-OBJECT:ProcedureParameter = NEW Consultingwerk.SmartFramework.Scheduler.ListProcedureParameterObject () .

    END CONSTRUCTOR.

    /**
     * Purpose: Starts the Scheduler Job Command
     * Notes:
     * @param poParameter The parmeter object for the job execution (from the SmartSchedulerJobStatus table)
     */
    METHOD PUBLIC VOID Start (poParameter AS Progress.Lang.Object):

        DEFINE VARIABLE hCall      AS HANDLE NO-UNDO.
        DEFINE VARIABLE iParameter AS INTEGER NO-UNDO.

        IF THIS-OBJECT:ProcedureParameter:Count > 0 THEN DO:

            CREATE CALL hCall .

            ASSIGN hCall:CALL-NAME  = THIS-OBJECT:ProcedureFileName
                   hCall:CALL-TYPE  = PROCEDURE-CALL-TYPE
                   hCall:PERSISTENT = FALSE

                   hCall:NUM-PARAMETERS = THIS-OBJECT:ProcedureParameter:Count .

            DO iParameter = 1 TO THIS-OBJECT:ProcedureParameter:Count:
                CASE THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType:

                    WHEN DataTypeEnum:CHARACTER THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue) .
                    WHEN DataTypeEnum:DATE THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, DATE (THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue)) .
                    WHEN DataTypeEnum:DATETIME THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, DATETIME (THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue)) .
                    WHEN DataTypeEnum:DATETIME-TZ THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, DATETIME-TZ (THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue)) .
                    WHEN DataTypeEnum:DECIMAL THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, DECIMAL (THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue)) .
                    WHEN DataTypeEnum:INT64 THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, INT64 (THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue)) .
                    WHEN DataTypeEnum:INTEGER THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, INTEGER (THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue)) .
                    WHEN DataTypeEnum:LOGICAL THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, DataTypeHelper:ToLogical (THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue)) .
                    WHEN "LONGCHAR":U THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue) .
                    WHEN DataTypeEnum:RECID THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, INTEGER (THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue)) .
                    WHEN DataTypeEnum:ROWID THEN
                        hCall:SET-PARAMETER (iParameter, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType, "INPUT":U, TO-ROWID (THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamValue)) .
                    OTHERWISE
                        UNDO, THROW NEW SchedulerException (SUBSTITUTE ("Invalid parameter datatype: &1"{&TRAN}, THIS-OBJECT:ProcedureParameter:GetItem(iParameter):ParamType), 0) .
                END CASE.
            END.

            hCall:INVOKE.
        END.

        ELSE
            RUN VALUE (THIS-OBJECT:ProcedureFileName) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hCall) .
        END FINALLY.

    END METHOD .

END CLASS.
