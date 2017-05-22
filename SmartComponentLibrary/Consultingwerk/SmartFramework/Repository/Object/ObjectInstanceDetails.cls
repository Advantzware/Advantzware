/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ObjectInstanceDetails
    Purpose     : Structure for details about an object instance
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 27 14:52:00 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Object.ObjectInstanceDetails:

    /**
     * Purpose: Gets and sets the name of the object instance
     * Notes:
     */
    DEFINE PUBLIC PROPERTY InstanceName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the page number of the object instance
     * Notes:
     */
    DEFINE PUBLIC PROPERTY InstancePageNumber AS INTEGER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the ObjectInstanceDetails class
     * Notes:
     */
    CONSTRUCTOR PUBLIC ObjectInstanceDetails ():
    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the ObjectInstanceDetails class
     * Notes:
     * @param pcInstanceName The name of the object instnace
     * @param piInstancePageNumber The page number of the object instance
     */
    CONSTRUCTOR PUBLIC ObjectInstanceDetails (pcInstanceName AS CHARACTER,
                                              piInstancePageNumber AS INTEGER):

        ASSIGN THIS-OBJECT:InstanceName       = pcInstanceName
               THIS-OBJECT:InstancePageNumber = piInstancePageNumber .

    END CONSTRUCTOR.

END CLASS.
