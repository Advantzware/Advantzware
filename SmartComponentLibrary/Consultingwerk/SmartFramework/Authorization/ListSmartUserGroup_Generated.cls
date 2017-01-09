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
    File        : ListSmartUserGroup_Generated
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 28.04.2013 00:33:34 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.GenericList FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.ListSmartUserGroup_Generated 
    INHERITS GenericList
    ABSTRACT: 

    { Consultingwerk/Framework/Base/GenericList.i SmartUserGroup }

    /*------------------------------------------------------------------------------
        Purpose: Creates a New Row, Initializes it and adds it to the List 
        Notes:   
        @return The reference to the new row instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED SmartUserGroup NewRow ():
        
        DEFINE VARIABLE oSmartUserGroup AS SmartUserGroup NO-UNDO . 
        
        oSmartUserGroup = NEW SmartUserGroup () . 
        oSmartUserGroup:InitializeNewRow () . 
        
        THIS-OBJECT:Add (oSmartUserGroup) . 
        
        RETURN oSmartUserGroup .
    END.

END CLASS.
