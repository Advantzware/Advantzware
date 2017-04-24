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
    File        : MenuStructureProvider
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 07 17:56:42 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                        FROM PROPATH .
USING Consultingwerk.Exceptions.*             FROM PROPATH .
USING Consultingwerk.Framework.*              FROM PROPATH .
USING Consultingwerk.SmartFramework.*         FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.*    FROM PROPATH .
USING Consultingwerk.Windows.Framework.*      FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.* FROM PROPATH .
USING Progress.Lang.*                         FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.MenuStructureProvider
    IMPLEMENTS IMenuStructureProvider:

    {Consultingwerk/SmartFramework/Menu/dsMenu.i}
    {Consultingwerk/Windows/Framework/ttMenuStructure.i}

    /*------------------------------------------------------------------------------
        Purpose: Returns the Menu Items referenced by the givens keys
        Notes:   Used by the Menu Function LRU
        @param pcKeys The CHARACTER Array with the menu item keys
        @param ttMenuStructure OUTOUT Temp-Table with the Menu Structure data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetMenuItems (pcKeys AS CHARACTER EXTENT,
                                     OUTPUT TABLE ttMenuStructure).

        DEFINE VARIABLE oCharacterHolder AS Consultingwerk.CharacterHolder NO-UNDO .
        DEFINE VARIABLE cValues          AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER                        NO-UNDO .

        EMPTY TEMP-TABLE ttMenuStructure .

        DO i = 1 TO EXTENT (pcKeys):
            IF pcKeys [i] > "":U THEN
                cValues = cValues + CHR(1) + pcKeys [i] .
        END.

        oCharacterHolder = NEW Consultingwerk.CharacterHolder (TRIM (cValues, CHR (1))) .

        /* Mike Fechner, Consultingwerk Ltd. 15.01.2014
           Indicate WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState ("InvokeMethod GetMenuItems":U, THIS-OBJECT) .

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U,
                                                       "GetMenuItems":U,
                                                       INPUT-OUTPUT DATASET dsMenu,
                                                       oCharacterHolder) .

        FOR EACH eSmartFunction ON ERROR UNDO, THROW:
            CREATE ttMenuStructure .
            ASSIGN ttMenuStructure.MenuKey           = eSmartFunction.FunctionGuid
                   ttMenuStructure.MenuName          = eSmartFunction.FunctionName
                   ttMenuStructure.MenuSmallImage    = eSmartFunction.FunctionSmallImage
                   ttMenuStructure.MenuLargeImage    = eSmartFunction.FunctionLargeImage
                   ttMenuStructure.FunctionKey       = eSmartFunction.FunctionGuid .
        END.

        FINALLY:
            DATASET dsMenu:EMPTY-DATASET () .

            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("InvokeMethod GetMenuItems":U, THIS-OBJECT) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Menu Structure for a given root key
        Notes:
        @param oGetMenuStructureParameter The IGetMenuStructureParameter with the data for the request
        @param ttMenuStructure OUTOUT Temp-Table with the Menu Structure data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetMenuStructure (oGetMenuStructureParameter AS IGetMenuStructureParameter,
                                         OUTPUT TABLE ttMenuStructure):

        EMPTY TEMP-TABLE ttMenuStructure .

        IF NOT TYPE-OF (oGetMenuStructureParameter,
                        ISerializable) THEN UNDO, THROW NEW InvalidParameterValueException ("oGetMenuStructureParameter":U,
                                                                                            oGetMenuStructureParameter:GetClass():TypeName,
                                                                                            "Consultingwerk.Windows.Framework.MenuStructureProvider":U) .

        /* Mike Fechner, Consultingwerk Ltd. 15.01.2014
           Indicate WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState ("InvokeMethod GetMenuStructure":U, THIS-OBJECT) .

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U,
                                                       "GetMenuStructure":U,
                                                       INPUT-OUTPUT DATASET dsMenu,
                                                       CAST (oGetMenuStructureParameter, ISerializable)) .

        FOR EACH eSmartMenu ON ERROR UNDO, THROW:
            CREATE ttMenuStructure .
            ASSIGN ttMenuStructure.MenuKey           = eSmartMenu.MenuGuid
                   ttMenuStructure.ParentMenuKey     = eSmartMenu.ParentMenuGuid
                   ttMenuStructure.MenuName          = eSmartMenu.MenuName
                   ttMenuStructure.MenuStructureType = eSmartMenu.MenuStructureType
                   ttMenuStructure.MenuBeginsAGroup  = eSmartMenu.MenuBeginsAGroup
                   ttMenuStructure.MenuSmallImage    = eSmartMenu.MenuSmallImage
                   ttMenuStructure.MenuLargeImage    = eSmartMenu.MenuLargeImage
                   ttMenuStructure.MenuSequence      = eSmartMenu.MenuSequence
                   ttMenuStructure.FunctionKey       = eSmartMenu.FunctionGuid
                   ttMenuStructure.HasChild          = eSmartMenu.HasChild .
        END.

        FINALLY:
            DATASET dsMenu:EMPTY-DATASET () .

            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("InvokeMethod GetMenuStructure":U, THIS-OBJECT) .
        END FINALLY.

    END METHOD .

END CLASS.
