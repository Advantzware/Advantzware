/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : DatabaseLoginPreferences
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 20 08:17:56 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.*             FROM PROPATH . 
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Windows.OpenEdgeLogin.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Windows.OpenEdgeLogin.DatabaseLoginPreferences IMPLEMENTS IDatabaseLoginPreferences: 

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList of all database opted-out from the login dialog
        Notes:
        @return The CharacterList of all database opted-out from the login dialog
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CharacterList GetAllOptOutedDbParam ():
		
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        DEFINE VARIABLE oList            AS CharacterList    NO-UNDO .  
        DEFINE VARIABLE i                AS INTEGER          NO-UNDO .
        DEFINE VARIABLE cValue           AS CHARACTER        NO-UNDO .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        oList = NEW CharacterList ("":U, CHR (1)) . 

        IF NOT VALID-OBJECT (oSettingsService) THEN 
            RETURN oList . 

        REPEAT:
            i = i + 1 . 
            
            cValue = oSettingsService:GetSetting ("Database Login OptOut":U, 
                                                  SUBSTITUTE ("DB_&1":U, i)) .
                                                  
            IF cValue > "":U THEN 
                oList:Add (cValue) .
            ELSE 
                RETURN oList .                                                    
        END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE when the user has opted-out from the DB Connection
                 dialog for the given database 
        Notes:
        @param pcDbParam The DBPARAM value for the DB to opt out from
        @return Logical value indicating if the user has opted out from the given DB
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL IsDisabled (pcDbParam AS CHARACTER):
		
        DEFINE VARIABLE oList  AS CharacterList NO-UNDO . 
        
        oList = GetAllOptOutedDbParam () .
        
        RETURN oList:ContainsValue (pcDbParam) .
         
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opts in for the given database connection
        Notes:   Based on DBPARAM () function
        @param pcDbParam The DBPARAM value for the DB to opt in for
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID OptIn (pcDbParam AS CHARACTER):
		
        DEFINE VARIABLE oList  AS CharacterList NO-UNDO . 
        DEFINE VARIABLE iEntry AS INTEGER       NO-UNDO .
        
        oList = GetAllOptOutedDbParam () .
        
        IF oList:ContainsValue (pcDbParam) THEN DO:
            iEntry = oList:IndexOf (pcDbParam) .
            
            IF iEntry > 0 THEN DO: 
                oList:Remove (iEntry) .
            
                THIS-OBJECT:WriteSettings (oList) .
            END.
        END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opts out for the given database connection
        Notes:   Based on DBPARAM () function
        @param pcDbParam The DBPARAM value for the DB to opt out for
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID OptOut (pcDbParam AS CHARACTER):
		
		DEFINE VARIABLE oList AS CharacterList NO-UNDO . 
		
		oList = GetAllOptOutedDbParam () .
		
		IF NOT oList:ContainsValue (pcDbParam) THEN DO:
		    oList:Add (pcDbParam) .
		    
		    THIS-OBJECT:WriteSettings (oList) .
		END.

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Writes the List of Database Connection Profiles back to the 
	             Settings Service
	    Notes:
	    @param poList The CharacterList of disabled database connection profiles
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID WriteSettings (poList AS CharacterList):
		
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO . 
        DEFINE VARIABLE i                AS INTEGER          NO-UNDO .
        DEFINE VARIABLE j                AS INTEGER          NO-UNDO .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF NOT VALID-OBJECT (oSettingsService) THEN 
            RETURN . 

        {Consultingwerk/foreachPrimitiveList.i Character cDbParam in poList}
            ASSIGN i = i + 1 .

            oSettingsService:StoreSetting ("Database Login OptOut":U, 
                                           SUBSTITUTE ("DB_&1":U, i), 
                                           cDbParam) . 
        END . 

        DO j = 1 TO 5:
            oSettingsService:StoreSetting ("Database Login OptOut":U, 
                                           SUBSTITUTE ("DB_&1":U, i + j), 
                                           "":U) .
        END.
        
	END METHOD.

END CLASS.
