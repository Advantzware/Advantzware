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
    File        : LinkGroupCreateSource.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 02 13:44:12 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartGroupCreateSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
   ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkGroupCreateSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupCreateSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartGroupCreateSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupCreateSource) THEN 
            RETURN CAST (THIS-OBJECT:SmartGroupCreateSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupCreateSource) . 
    END GET . 
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupCreateSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartGroupCreateSource) THEN 
            ASSIGN THIS-OBJECT:SmartGroupCreateSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartGroupCreateSource) .         
    END.  