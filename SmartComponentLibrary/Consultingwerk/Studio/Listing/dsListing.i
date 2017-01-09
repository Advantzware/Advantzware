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
    File        : dsListing.i
    Purpose     : Business Entity for Listing

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 26.06.2011 21:29:58
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

{ Consultingwerk/Studio/Listing/eBlockInfo.i }
{ Consultingwerk/Studio/Listing/eListing.i }


DEFINE {&ACCESS} DATASET dsListing {&REFERENCE-ONLY} FOR eBlockInfo, eListing 

    .    
