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
    File        : ListTableIOTargetControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 04 09:43:04 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Base.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.ListTableIOTargetControl
    INHERITS GenericList: 

    { Consultingwerk/Framework/Base/GenericList.i TableIOTargetControl }

    /*------------------------------------------------------------------------------
        Purpose: Returns the TableIOTargetControl instance for the given TableIOTarget
        Notes:
        @param poTableIOTarget The reference to the TableIOTarget
        @return The TableIOTargetControl instance for the given TableIOTarget
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableIOTargetControl FindByTableIOTarget (poTableIOTarget AS ISmartTableIOTarget):
        
        {Consultingwerk/foreachABL.i TableIOTargetControl oControl in THIS-OBJECT}
            IF oControl:SmartTableIOTarget = poTableIOTarget THEN 
                RETURN oControl . 
        END.
        
        RETURN ? . 

    END METHOD.

END CLASS . 
   