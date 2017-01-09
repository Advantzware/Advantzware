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
    File        : ListSmartUiTranslation_Generated
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 12.05.2013 18:40:16
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.GenericList FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.ListSmartUiTranslation_Generated
    INHERITS GenericList
    ABSTRACT:

    { Consultingwerk/Framework/Base/GenericList.i SmartUiTranslation }

    /*------------------------------------------------------------------------------
        Purpose: Creates a New Row, Initializes it and adds it to the List
        Notes:
        @return The reference to the new row instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED SmartUiTranslation NewRow ():

        DEFINE VARIABLE oSmartUiTranslation AS SmartUiTranslation NO-UNDO .

        oSmartUiTranslation = NEW SmartUiTranslation () .
        oSmartUiTranslation:InitializeNewRow () .

        THIS-OBJECT:Add (oSmartUiTranslation) .

        RETURN oSmartUiTranslation .
    END METHOD .

END CLASS.
