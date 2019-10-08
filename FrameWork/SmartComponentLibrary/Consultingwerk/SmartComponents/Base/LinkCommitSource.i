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
    File        : LinkCommitSource.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 01 23:14:27 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartCommitSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkCommitSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartCommitSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource) THEN
            RETURN CAST (THIS-OBJECT:SmartCommitSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource) .
    END GET .
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartCommitSource) THEN
            ASSIGN THIS-OBJECT:SmartCommitSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartCommitSource) .
    END.