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
    File        : MenuStructureImporter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 12 13:50:37 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Tools.Import.MenuStructure.* FROM PROPATH .  
USING Progress.Lang.*                                            FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Tools.Import.MenuStructure.MenuStructureImporter: 

    {Consultingwerk/SmartFramework/Menu/dsMenu.i}

    /*------------------------------------------------------------------------------
        Purpose: Imports SmartMenu and SmartFunction records from an XML file
        Notes:   
        @param pcFileName The name of the file to import from
        @param plOverwriteExisting Overwrite existing records, by GUID
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ImportMenuStructure (pcFileName AS CHARACTER, 
	                                        plOverwriteExisting AS LOGICAL):
		
		DEFINE BUFFER SmartMenu     FOR SmartMenu .
		DEFINE BUFFER SmartFunction FOR SmartFunction . 

        DATASET dsMenu:READ-XML ("file":U, 
                                  pcFileName,
                                  "empty":U,
                                  ?,
                                  FALSE) .

        menuLoop: 
        FOR EACH eSmartMenu ON ERROR UNDO, THROW:
            
            FIND SmartMenu WHERE SmartMenu.MenuGuid = eSmartMenu.MenuGuid EXCLUSIVE NO-ERROR .
            
            IF NOT AVAILABLE SmartMenu THEN  
                CREATE SmartMenu. 
            ELSE 
                IF plOverwriteExisting = FALSE THEN 
                    NEXT menuLoop .
            
            BUFFER-COPY eSmartMenu TO SmartMenu . 
        END.

        functionLoop: 
        FOR EACH eSmartFunction ON ERROR UNDO, THROW:
            
            FIND SmartFunction WHERE SmartFunction.FunctionGuid = eSmartFunction.FunctionGuid EXCLUSIVE NO-ERROR .
            
            IF NOT AVAILABLE SmartFunction THEN  
                CREATE SmartFunction. 
            ELSE 
                IF plOverwriteExisting = FALSE THEN 
                    NEXT functionLoop .
            
            BUFFER-COPY eSmartFunction TO SmartFunction . 
        END.


        FINALLY:
            DATASET dsMenu:EMPTY-DATASET () .       
        END FINALLY.


	END METHOD .

END CLASS.
