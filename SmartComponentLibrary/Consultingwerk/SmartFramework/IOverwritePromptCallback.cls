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
    File        : IOverwritePromptCallback
    Purpose     : Callback which allows a UI component to prompt the user
                  if a certain resource should be overwritten
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Dec 10 15:47:16 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.IOverwritePromptCallback:

    /**
     * Purpose: Prompts the user if a resource should be overwritten
     * Notes:
     * @param pcObjectName The name of the resources that
     * @return Logical value indicating if an existing object should be overwritten
     */
    METHOD PUBLIC LOGICAL PromptForOverwrite (pcObjectName AS CHARACTER) .

END INTERFACE .
