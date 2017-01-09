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
    File        : JobCommandControlFactory
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 11:25:17 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Framework.Scheduler.* FROM PROPATH.
USING Consultingwerk.SmartFramework.Scheduler.*    FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Scheduler.JobCommandControlFactory
    IMPLEMENTS IJobCommandControlFactory:

    /*------------------------------------------------------------------------------
        Purpose: Returns an instance of a JobCommandControl based on the type of
                 the JobCommand
        Notes:
        @param poJobCommandType The class reference of the JobCommand for which a UserControl should be provided
        @return The specialized JobCommandControl instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Windows.Forms.UserControl GetJobCommandControl (INPUT poJobCommandType AS Progress.Lang.Class):

        DEFINE VARIABLE cUserControlClassName AS CHARACTER NO-UNDO.

        Consultingwerk.Assertion.ObjectAssert:IsValid (poJobCommandType) .

&IF NOT PROVERSION BEGINS "10.2":U &THEN
        ASSIGN cUserControlClassName = poJobCommandType:GetPropertyValue ("EditorControl":U) .
&ELSE
        CASE poJobCommandType:TypeName:
            WHEN "Consultingwerk.SmartFramework.Scheduler.ShellCommandSchedulerJobCommand":U  THEN
                ASSIGN cUserControlClassName = Consultingwerk.SmartFramework.Scheduler.ShellCommandSchedulerJobCommand:EditorControl .
            WHEN "Consultingwerk.SmartFramework.Scheduler.BusinessTaskSchedulerJobCommand":U  THEN
                ASSIGN cUserControlClassName = Consultingwerk.SmartFramework.Scheduler.BusinessTaskSchedulerJobCommand:EditorControl .
            WHEN "Consultingwerk.SmartFramework.Scheduler.ProcedureSchedulerJobCommand":U     THEN
                ASSIGN cUserControlClassName = Consultingwerk.SmartFramework.Scheduler.ProcedureSchedulerJobCommand:EditorControl .
        END CASE .
&ENDIF

        Consultingwerk.Assertion.Assert:NotNullOrEmpty (cUserControlClassName, "User Control Class Name":U) .

        RETURN InternalGetControl (cUserControlClassName).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Internal Method to handle the creation an caching of control instances
        Notes:
        @param pcUserControlClassName The full type name of the user control class naem
        @return The CallParameterControl instance
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE System.Windows.Forms.UserControl InternalGetControl (pcUserControlClassName AS CHARACTER):

        DEFINE VARIABLE oUserControl AS System.Windows.Forms.UserControl NO-UNDO.

        oUserControl = DYNAMIC-NEW (pcUserControlClassName) ().

        RETURN oUserControl.

    END METHOD .

END CLASS.
