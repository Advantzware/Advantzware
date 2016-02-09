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
    File        : getclassnamesonserver.p
    Purpose     : Stub to get Class Names (especially Business Entity 
                  Class names on the AppServer)

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 04 14:54:26 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/Util/TempTables/ttClassNames.i}

DEFINE INPUT  PARAMETER pcBaseType              AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER plIncludeAbstract       AS LOGICAL   NO-UNDO .
DEFINE OUTPUT PARAMETER TABLE FOR ttClassNames .

/* ***************************  Main Block  *************************** */

Consultingwerk.Util.ClassHelper:GetClassNamesInClassPathNoDotNet (pcBaseType, 
                                                                  plIncludeAbstract,
                                                                  OUTPUT TABLE ttClassNames) .

                                                                  