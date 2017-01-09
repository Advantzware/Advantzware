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
    File        : FunctionCallCacheService
    Purpose     : Caches the FunctionCallParameter list to be usable more
                  easy
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Jan 14 16:32:06 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.* FROM PROPATH .
USING Consultingwerk.Util.*           FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.SmartFramework.FunctionCallCacheService 
    IMPLEMENTS IFunctionCallCacheService: 

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE"}

    DEFINE PRIVATE TEMP-TABLE ttValue NO-UNDO 
        FIELD EntryKey    AS CHARACTER 
        FIELD DisplayText AS CHARACTER 
        FIELD ImageKey    AS CHARACTER 
        INDEX idxMain IS PRIMARY UNIQUE DisplayText
        .
    
    /*------------------------------------------------------------------------------
        Purpose: ValueList of FunctionCallParameters
        Notes:   
    ------------------------------------------------------------------------------*/
    &IF DEFINED (DotNetAccessible) NE 0 &THEN
    DEFINE PUBLIC PROPERTY FunctionCallTypes AS Infragistics.Win.ValueList NO-UNDO
    GET .
    PRIVATE SET .
    &ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FunctionCallCacheService class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FunctionCallCacheService ():
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
		    Consultingwerk.Framework.FrameworkSettings:WaitStateManager:SetWaitState ("Caching Function Call Parameters":U).
		
		&IF DEFINED (DotNetAccessible) NE 0 &THEN
		UserInterfaceHelper:ProcessEvents() .
		
        ClassHelper:GetClassNamesInClassPath ("Consultingwerk.SmartFramework.IFunctionCallParameter":U, 
                                              OUTPUT TABLE ttClassNames BY-REFERENCE).
                
        GarbageCollectorHelper:DeleteObject (THIS-OBJECT:FunctionCallTypes).
        &ENDIF
        
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
        
        &IF DEFINED (DotNetAccessible) NE 0 &THEN
        THIS-OBJECT:FunctionCallTypes = ValueListHelper:FromTempTableAndImageProvider (TEMP-TABLE ttValue:HANDLE, 
                                                                                       "EntryKey":U, 
                                                                                       "DisplayText":U, 
                                                                                       "&1":U,
                                                                                       "ImageKey":U,
                                                                                       ?,
                                                                                       ?) . 
        &ENDIF

        FINALLY:
            EMPTY TEMP-TABLE ttClassNames.
            EMPTY TEMP-TABLE ttValue.

            IF VALID-OBJECT (Consultingwerk.Framework.FrameworkSettings:WaitStateManager) THEN 
                Consultingwerk.Framework.FrameworkSettings:WaitStateManager:ClearWaitState ("Caching Function Call Parameters":U).
        END FINALLY.
	END METHOD .

END CLASS.
