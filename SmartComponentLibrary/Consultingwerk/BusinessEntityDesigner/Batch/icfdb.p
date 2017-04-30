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
    File        : icfdb.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 02 16:08:28 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Batch.* .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE oGenerator AS BatchGenerator NO-UNDO .

oGenerator = NEW BatchGenerator () .

oGenerator:GenerateBusinessEntities ("Consultingwerk.SmartComponentsDemo.OERA.IcfDb.Generated":U, /* Package */
                                     "&1_BusinessEntity":U,                                       /* BE Name Pattern */
                                     "&1_DataAccess":U,                                           /* DA Name Pattern */
                                     "&1_DatasetController":U,                                    /* DSC Name Pattern */
                                     FALSE,                                                       /* Force Uppercase first Name of Table */
                                     "icfdb":U,                                                   /* DB Name */
                                     "*":U) .                                                     /* Table Filter (CAN-DO), i.e. "Cu*,Or*" or "*" or "Customer" */ 

CATCH err AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
END CATCH.
