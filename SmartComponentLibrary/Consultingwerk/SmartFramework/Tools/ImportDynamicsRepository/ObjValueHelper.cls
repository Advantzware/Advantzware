/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ObjValueHelper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 15 19:32:33 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Tools.ImportDynamicsRepository.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH .
USING Consultingwerk.* FROM PROPATH.
USING Consultingwerk.Util.* FROM PROPATH.

CLASS Consultingwerk.SmartFramework.Tools.ImportDynamicsRepository.ObjValueHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjValueHelper class
        Notes:   Disallow instance creation 
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PROTECTED ObjValueHelper ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Converts a Dynamics _obj Value into a Character Key
        Notes:   
        @param pdeObjValue The decimal _obj value to be converted
        @return The character key
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC CHARACTER ObjToId (pdeObjValue AS DECIMAL):
		
		DEFINE VARIABLE oNumericFormat AS NumericFormat NO-UNDO . 
		
		oNumericFormat = SessionHelper:GetNumericFormat() .
		
		SessionHelper:SetDefaultNumericFormat() .
		
		RETURN STRING (pdeObjValue) .
		
		FINALLY:
            SessionHelper:SetNumericFormat (oNumericFormat) .		
        END FINALLY.

	END METHOD .

END CLASS.
