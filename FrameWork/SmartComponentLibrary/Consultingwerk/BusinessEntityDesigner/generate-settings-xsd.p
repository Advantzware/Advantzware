/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : generate-settings-xsd.p
    Purpose     : Writes the XSD file for the Business Entity Designer
                  Temp-Table

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Fri Jun 06 20:57:19 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/BusinessEntityDesigner/eBusinessEntityDesignerSettings.i}

/* ***************************  Main Block  *************************** */

TEMP-TABLE eBusinessEntityDesignerSettings:WRITE-XMLSCHEMA ("file":U,
                                                            "Consultingwerk/BusinessEntityDesigner/eBusinessEntityDesignerSettings.xsd":U,
                                                            TRUE) .
                                                            