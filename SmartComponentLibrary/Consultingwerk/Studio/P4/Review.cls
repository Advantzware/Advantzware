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
    File        : Review
    Purpose     : Wrapper to the P4 Review command supporting ABL based
                  Review Daemon implementations 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : Fri Jun 20 10:17:27 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Studio.P4.*             FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Studio.P4.Review: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacterList with the change list numbers to review  
        Notes:   
        @return The CharacterList with the change list numbers to review 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CharacterList GetReviewChangelists ():
		
		DEFINE VARIABLE oChanges AS CharacterList NO-UNDO .
        DEFINE VARIABLE cLine    AS CHARACTER     NO-UNDO .

        oChanges = NEW CharacterList () .

        INPUT THROUGH VALUE("p4 review -t review":U) .

        REPEAT:
            IMPORT UNFORMATTED cLine . 

            IF NUM-ENTRIES (cLine, " ":U) > 1 AND ENTRY (1, cLine, " ":U) = "Change":U THEN 
                oChanges:Add (ENTRY (2, cLine, " ":U)) .
        END.
        
        INPUT CLOSE . 

		RETURN oChanges .

	END METHOD .

END CLASS.
