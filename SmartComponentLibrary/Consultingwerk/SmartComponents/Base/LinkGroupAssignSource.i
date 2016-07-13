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
    File        : LinkGroupAssignSource.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 27 00:39:05 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartGroupAssignSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
   ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkGroupAssignSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupAssignSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartGroupAssignSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupAssignSource) THEN 
            RETURN CAST (THIS-OBJECT:SmartGroupAssignSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupAssignSource) . 
    END GET . 
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupAssignSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartGroupAssignSource) THEN 
            ASSIGN THIS-OBJECT:SmartGroupAssignSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartGroupAssignSource) .         
    END.  