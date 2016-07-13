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
    File        : LinkTableIOSource.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 27 00:40:19 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartTableIOSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
   ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkTableIOSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartTableIOSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource) THEN 
            RETURN CAST (THIS-OBJECT:SmartTableIOSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource) . 
    END GET . 
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartTableIOSource) THEN 
            ASSIGN THIS-OBJECT:SmartTableIOSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartTableIOSource) .         
    END.