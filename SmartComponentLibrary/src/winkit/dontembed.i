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
    File        : dontembed.i
    Purpose     : Method Library for Windows that shall not be embedded
                  The method library defines the variables used by the 
                  other include files so that code accessing those 
                  variables does not cause compile issues

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 22 12:02:19 CEST 2009
    Notes       : Either this or embedwindow.i should be used as a 
                  method libary - not both!
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Mike Fechner, Consultingwerk Ltd. 08.07.2009
   Conditional compile rule: Will compile in OpenEdge release 10.2A and above,
   This rule will break when release 40.x will be reached. Note PROVERSION
   returns a string, so this is alpha comparison, not numeric */
&IF NOT PROVERSION GE "4" AND PROVERSION GE "10.2A" &THEN
DEFINE VARIABLE oForm AS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm NO-UNDO .
DEFINE VARIABLE oFormControl AS System.Windows.Forms.Form NO-UNDO .
&ENDIF
