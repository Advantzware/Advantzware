/*-----------------------------------------------------------------------
  File : QueryLib.i
  Desc : Definitions + start of library for query functions
  ------------------------------------------------------------------------*/

{dsQuery.i &reference-only={&reference-only}}
{dsSchema.i} /* dataset for schema */
           
/* Forward definitions */
FUNCTION canHaveRelation RETURNS LOGICAL
  ( pcParent AS CHARACTER
  , pcChild AS CHARACTER) IN SUPER.
  
FUNCTION getLightGray RETURNS INTEGER
  ( /* no parameters */ ) IN SUPER.

FUNCTION getNewQueryNr RETURNS INTEGER
  ( /* parameter-definitions */ )  IN SUPER.

FUNCTION getReadableQuery RETURNS CHARACTER
  ( INPUT pcQuery AS CHARACTER )   IN SUPER.
  
FUNCTION getUserId RETURNS CHARACTER
  ( /* parameter-definitions */ )  IN SUPER.

FUNCTION getUserType RETURNS CHARACTER
  ( /* parameter-definitions */ )  IN SUPER.

FUNCTION setUserType RETURNS LOGICAL
  ( pcUserType AS CHARACTER )  IN SUPER.
  
FUNCTION isQueryChanged RETURNS LOGICAL
  ( INPUT DATASET dsQuery ) IN SUPER.

/* Start super if not yet done (not from within the super itself)
*/
&IF DEFINED(InSuper) = 0 &THEN
  DEFINE VARIABLE ghQueryLib AS HANDLE NO-UNDO.

  PUBLISH 'QueryLib' (OUTPUT ghQueryLib).
  IF NOT VALID-HANDLE(ghQueryLib) THEN
  DO:
    RUN QueryLib.p PERSISTENT SET ghQueryLib.
    SESSION:ADD-SUPER-PROCEDURE(ghQueryLib).
  END.
&ENDIF
