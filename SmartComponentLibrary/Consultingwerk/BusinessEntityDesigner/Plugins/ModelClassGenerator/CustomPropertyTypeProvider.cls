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
    File        : CustomPropertyTypeProvider
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Feb 07 01:53:55 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.Plugins.ModelClassGenerator.* FROM PROPATH .  
USING Consultingwerk.Framework.*                                          FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                              FROM PROPATH .
USING Consultingwerk.Util.*                                               FROM PROPATH .
USING Progress.Lang.*                                                     FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.ModelClassGenerator.CustomPropertyTypeProvider: 

    {Consultingwerk/Util/TempTables/ttClassNames.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CustomPropertyTypeProvider class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC CustomPropertyTypeProvider ():
		SUPER ().
		
		IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN 
		    FrameworkSettings:WaitStateManager:SetWaitState() .

        ClassHelper:GetClassNames ("Consultingwerk.Enum":U, OUTPUT TABLE ttClassNames) .

        &IF DEFINED (NativeEnums) NE 0 &THEN        
        ClassHelper:GetClassNames ("Progress.Lang.Enum":U, OUTPUT TABLE ttClassNames APPEND) .
        &ENDIF

        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN 
            FrameworkSettings:WaitStateManager:ClearWaitState() .

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds the Enum types to the provided CharacterList 
        Notes:   
        @param poList The CharacterList to add property values to
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID AddPropertyValues (poList AS CharacterList):
		
		FOR EACH ttClassNames ON ERROR UNDO, THROW:

            poList:Add (ttClassNames.ClassName) .

        END.

	END METHOD .

END CLASS.
