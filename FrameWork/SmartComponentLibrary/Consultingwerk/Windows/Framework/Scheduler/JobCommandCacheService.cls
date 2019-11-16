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
    File        : JobCommandCacheService
    Purpose     : Caches the list of available ISchedulerJobCommand
                  implementations
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 14:12:42 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Util.*                        FROM PROPATH.
USING Consultingwerk.Windows.Framework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Scheduler.JobCommandCacheService
    IMPLEMENTS IJobCommandCacheService:

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE"}

    DEFINE PRIVATE TEMP-TABLE ttValue NO-UNDO
        FIELD EntryKey    AS CHARACTER
        FIELD DisplayText AS CHARACTER
        FIELD ImageKey    AS CHARACTER
        INDEX idxMain IS PRIMARY UNIQUE DisplayText
        .

    /*------------------------------------------------------------------------------
        Purpose: ValueList of JobCommands
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY JobCommands AS Infragistics.Win.ValueList NO-UNDO
    GET .
    PRIVATE SET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FunctionCallCacheService class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC JobCommandCacheService ():
        SUPER ().

        InitializeService ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Populates the ValueList property of this class
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeService ():

        DEFINE VARIABLE cImageKey AS CHARACTER NO-UNDO.

        IF VALID-OBJECT (Consultingwerk.Framework.FrameworkSettings:WaitStateManager) THEN
            Consultingwerk.Framework.FrameworkSettings:WaitStateManager:SetWaitState ("Caching Job Commands":U).

        UserInterfaceHelper:ProcessEvents() .

        ClassHelper:GetClassNamesInClassPath ("Consultingwerk.SmartFramework.Scheduler.ISchedulerJobCommand":U,
                                              OUTPUT TABLE ttClassNames BY-REFERENCE).

        GarbageCollectorHelper:DeleteObject (THIS-OBJECT:JobCommands).

        FOR EACH ttClassNames NO-LOCK
            ON ERROR UNDO, THROW:

            CREATE ttValue.

            ASSIGN
                ttValue.EntryKey  = ttClassNames.ClassName
                ttValue.DisplayText = ENTRY (NUM-ENTRIES (ttClassNames.ClassName, ".":U), ttClassNames.ClassName, ".":U)
                .

            &IF NOT PROVERSION BEGINS "10":U &THEN
            DO ON ERROR UNDO, THROW:
                cImageKey = DYNAMIC-PROPERTY (ttClassNames.ClassName, "ImageKey":U).

                ttValue.ImageKey = cImageKey .

                CATCH ple AS Progress.Lang.Error :
                    /* Marko Rüterbories, Consultingwerk Ltd. 14.01.2014
                       Ignore the error if the property does not exist.
                       Could not find property ImageKey in class Consultingwerk.Statics.StaticsClass. (16548) */
                    IF ple:GetMessageNum(1) <> 16548 THEN
                        Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
                END CATCH.
            END.
            &ENDIF
        END.

        THIS-OBJECT:JobCommands = ValueListHelper:FromTempTableAndImageProvider (TEMP-TABLE ttValue:HANDLE,
                                                                                 "EntryKey":U,
                                                                                 "DisplayText":U,
                                                                                 "&1":U,
                                                                                 "ImageKey":U,
                                                                                 ?,
                                                                                 ?) .

        FINALLY:
            EMPTY TEMP-TABLE ttClassNames.
            EMPTY TEMP-TABLE ttValue.

            IF VALID-OBJECT (Consultingwerk.Framework.FrameworkSettings:WaitStateManager) THEN
                Consultingwerk.Framework.FrameworkSettings:WaitStateManager:ClearWaitState ("Caching Job Commands":U).
        END FINALLY.

    END METHOD .

END CLASS.
