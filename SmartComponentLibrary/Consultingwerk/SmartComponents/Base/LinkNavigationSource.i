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
    File        : LinkNavigationSource.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 27 00:41:46 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartNavigationSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
   ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkNavigationSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartNavigationSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource) THEN 
            RETURN CAST (THIS-OBJECT:SmartNavigationSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource) . 
    END GET . 
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartNavigationSource) THEN 
            ASSIGN THIS-OBJECT:SmartNavigationSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartNavigationSource) .         
    END.