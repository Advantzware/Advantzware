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
    File        : winkitactive.i
    Purpose     : Conditional compile rule: Will compile in OpenEdge 
                  release 10.2A and above. 

    Syntax      :

    Description : The intention of this is to make sure, that the legacy 
                  windows remain compilable and runnable on Progress 
                  Versions 8.x, 9.x and 10.0x, 10.1x 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 26 13:32:30 CEST 2012
    Notes       : This rule will break when release 40.x will be reached. 
                  Note PROVERSION returns a string, so this is alpha comparison, not numeric.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF NOT PROVERSION GE "4" AND PROVERSION GE "10.2A" &THEN
&GLOBAL-DEFINE winkitactive
&ENDIF
