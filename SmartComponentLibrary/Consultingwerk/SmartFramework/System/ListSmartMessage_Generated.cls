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
    File        : ListSmartMessage_Generated
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 28.04.2013 00:33:34 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.GenericList FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.ListSmartMessage_Generated 
    INHERITS GenericList
    ABSTRACT: 

    { Consultingwerk/Framework/Base/GenericList.i SmartMessage }

    /*------------------------------------------------------------------------------
        Purpose: Creates a New Row, Initializes it and adds it to the List 
        Notes:   
        @return The reference to the new row instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED SmartMessage NewRow ():
        
        DEFINE VARIABLE oSmartMessage AS SmartMessage NO-UNDO . 
        
        oSmartMessage = NEW SmartMessage () . 
        oSmartMessage:InitializeNewRow () . 
        
        THIS-OBJECT:Add (oSmartMessage) . 
        
        RETURN oSmartMessage .
    END.

END CLASS.
