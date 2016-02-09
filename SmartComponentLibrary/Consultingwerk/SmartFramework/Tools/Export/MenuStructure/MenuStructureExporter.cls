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
    File        : MenuStructureExporter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 12 12:32:48 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                           FROM PROPATH . 
USING Consultingwerk.SmartFramework.Tools.Export.MenuStructure.* FROM PROPATH .
USING Consultingwerk.Util.*                                      FROM PROPATH .   
USING Progress.Lang.*                                            FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Tools.Export.MenuStructure.MenuStructureExporter: 

    {Consultingwerk/SmartFramework/Menu/dsMenu.i}

    /*------------------------------------------------------------------------------
        Purpose: Copies a single SmartMenu record and the related SmartFunction 
                 record to the eSmartMenu temp-table and calls itself for all child 
                 records
        Notes:   
        @param pcMenuGuid The Menu Guid to export
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ExportMenu (pcMenuGuid AS CHARACTER):
		
        DEFINE BUFFER eSmartMenu     FOR eSmartMenu . 
        DEFINE BUFFER eSmartFunction FOR eSmartFunction . 

        FIND SmartMenu WHERE SmartMenu.MenuGuid = pcMenuGuid NO-LOCK .

        CREATE eSmartMenu .
        BUFFER-COPY SmartMenu TO eSmartMenu .

        IF SmartMenu.FunctionGuid > "":U THEN DO ON ERROR UNDO, THROW:
            FIND SmartFunction WHERE SmartFunction.FunctionGuid = SmartMenu.FunctionGuid NO-LOCK NO-ERROR .
            IF AVAILABLE SmartFunction THEN DO ON ERROR UNDO, THROW:
                
                FIND eSmartFunction WHERE eSmartFunction.FunctionGuid = SmartMenu.FunctionGuid NO-LOCK NO-ERROR .
                
                IF NOT AVAILABLE eSmartFunction THEN DO ON ERROR UNDO, THROW:
                    CREATE eSmartFunction . 
                    BUFFER-COPY SmartFunction TO eSmartFunction .
                END.
            END.
        END.

        FOR EACH SmartMenu WHERE SmartMenu.ParentMenuGuid = pcMenuGuid NO-LOCK ON ERROR UNDO, THROW:
            THIS-OBJECT:ExportMenu (SmartMenu.MenuGuid) .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Exports a menu structure to the given file name 
        Notes:   
        @param pcParentMenuGuids The comma delimited list of menu guids to export including all children
        @param pcTargetFile The target file to write to
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ExportMenuStructure (pcParentMenuGuids AS CHARACTER,
	                                        pcTargetFile AS CHARACTER):
		
		DEFINE VARIABLE cParentMenuGuid AS CHARACTER NO-UNDO .
		DEFINE VARIABLE i               AS INTEGER   NO-UNDO .

        DATASET dsMenu:EMPTY-DATASET () .
		
		DEFINE BUFFER SmartMenu FOR SmartMenu . 
		
		DO i = 1 TO NUM-ENTRIES (pcParentMenuGuids) ON ERROR UNDO, THROW:
		
		    ASSIGN cParentMenuGuid = ENTRY (i, pcParentMenuGuids) .
		
		    FOR EACH SmartMenu WHERE SmartMenu.MenuGuid = cParentMenuGuid NO-LOCK ON ERROR UNDO, THROW . 

                THIS-OBJECT:ExportMenu (SmartMenu.MenuGuid) .                

            END .

        END .

        DATASET dsMenu:WRITE-XML ("file":U, 
                                  pcTargetFile,
                                  TRUE) .

        FINALLY:
            DATASET dsMenu:EMPTY-DATASET () . 		
        END FINALLY.

	END METHOD .

END CLASS.
