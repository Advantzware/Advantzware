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
    File        : gen-xsd.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 21 12:17:00 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/Util/TempTables/dsDockManagerSettings.i}
{Consultingwerk/Util/TempTables/ttClassNames.i}
{Consultingwerk/Util/TempTables/ttClassPath.i}

/* ***************************  Main Block  *************************** */

DATASET dsDockManagerSettings:WRITE-XMLSCHEMA ("FILE":U, 
                                               "Consultingwerk/Util/TempTables/dsDockManagerSettings.xsd":U,
                                               TRUE) . 

TEMP-TABLE ttClassNames:WRITE-XMLSCHEMA ("FILE":U, 
                                         "Consultingwerk/Util/TempTables/ttClassNames.xsd":U,
                                         TRUE) . 
                                               
TEMP-TABLE ttClassPath:WRITE-XMLSCHEMA ("FILE":U, 
                                     "Consultingwerk/Util/TempTables/ttClassPath.xsd":U,
                                     TRUE) .
                                      