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
    File        : ListSmartSecurityAssignment_Generated
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 28.07.2013 17:49:52
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.GenericList FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.ListSmartSecurityAssignment_Generated
    INHERITS GenericList
    ABSTRACT:

    { Consultingwerk/Framework/Base/GenericList.i SmartSecurityAssignment }

    /*------------------------------------------------------------------------------
        Purpose: Creates a New Row, Initializes it and adds it to the List
        Notes:
        @return The reference to the new row instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED SmartSecurityAssignment NewRow ():

        DEFINE VARIABLE oSmartSecurityAssignment AS SmartSecurityAssignment NO-UNDO .

        oSmartSecurityAssignment = NEW SmartSecurityAssignment () .
        oSmartSecurityAssignment:InitializeNewRow () .

        THIS-OBJECT:Add (oSmartSecurityAssignment) .

        RETURN oSmartSecurityAssignment .
    END METHOD .

END CLASS.
