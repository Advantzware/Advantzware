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
    File        : _idestart.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 27 20:21:25 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH . 

{adecomm/oeideservice.i}
{Consultingwerk/products.i}

/* ***************************  Main Block  *************************** */

Consultingwerk.Util.ClassHelper:UseExternalRuntimeForGetClassNames = TRUE . 

/* Mike Fechner, Consultingwerk Ltd. 03.03.2014
   Ability to create a logfile for the ClassHelper:GetClassName calls */
/*Consultingwerk.Util.ClassHelper:LogfileName = SESSION:TEMP-DIRECTORY + "getclasses.log":U .*/

/* Mike Fechner, Consultingwerk Ltd. 03.01.2012
   Register OEA network port in Registry. This is done by writing the 
   project path, project name and the OEA_PORT for this project to the 
   Windows registry. The purpose of writing these parameters to the 
   registry is to support tools that use the OpenEdgeArchitectHelper
   to send commands to the IDE (e.g. opening a file) */
   
DEFINE VARIABLE cCurrentPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProjectID   AS CHARACTER NO-UNDO.

FILE-INFO:FILE-NAME = ".":U .
ASSIGN cCurrentPath = FILE-INFO:FULL-PATHNAME 
       cProjectID   = SUBSTITUTE ("&1 &2":U, 
                                  DYNAMIC-FUNCTION ("getProjectName":U),
                                  OS-GETENV("OEA_PORT":U)). 

&IF DEFINED (DotNetAccessible) NE 0 &THEN
Consultingwerk.Framework.Registry:SetRegistryValue ("USER":U, 
                                                    "Software\Consultingwerk Ltd.\IDE Support":U,
                                                    cCurrentPath,
                                                    cProjectID) .
&ENDIF


/* Mike Fechner, Consultingwerk Ltd. 04.09.2013
   Activate the following code when AppServer support is required for
   getting the list of Business Entities: Business Entities compiled
   in a different version of OpenEdge (GUI 11.x, Backend 10.2B) or
   the Business Entities reside on a UNIX server during development.
   Make required adjustments to connecting to the Business Entity */
/* AppServer searches for Business Entities using the .classpath file
   found first in the AppServers propath */
   
/*Consultingwerk.SmartComponents.Support.BusinessEntityDesignerSupport:SearchBusinessEntititiesOnAppServer = TRUE .*/
/*                                                                                                                 */
/*/* Mike Fechner, Consultingwerk Ltd. 01.07.2015                                                                  */
/*   SCL-887 - disconnect from AppServer after each request to search                                              */
/*   for business entities. */                                                                                     */
/*Consultingwerk.SmartComponents.Support.BusinessEntityDesignerSupport:DisconnectAppServerAfterEachRequest = TRUE .*/
/*                                                                                                                 */
/*FrameworkSettings:AppServerServiceManager = NEW AppServerServiceManager () .                                     */
/*                                                                                                                 */
/*/* Mike Fechner, Consultingwerk Ltd. 20.07.2011                                                                  */
/*   New way of defining a default partition for AppServer calls */                                                */
/*CAST (FrameworkSettings:AppServerServiceManager,                                                                 */
/*      AppServerServiceManager):DefaultPartition = "Default":U .                                                  */

/* Mike Fechner, Consultingwerk Ltd. 01.07.2015
   It's actually not required to connect to the AppServer at this point (SCL-887) */
/*FrameworkSettings:AppServerServiceManager:ConnectService ("Default":U) .                                         */
