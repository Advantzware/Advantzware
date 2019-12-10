/* Create a new database if it does not already exist
*/
DEFINE VARIABLE cDatabase AS CHARACTER NO-UNDO INITIAL 'qb'.
DEFINE VARIABLE lReplace  AS LOGICAL   NO-UNDO.

IF SEARCH(cDatabase + '.db') <> ? THEN 
DO:
  FILE-INFO:FILE-NAME = cDatabase + '.db'.
  MESSAGE "Database" FILE-INFO:FULL-PATHNAME "already exists." SKIP(1) "Do you want to replace it?"
          VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE lReplace.
  IF NOT lReplace THEN QUIT.
END.

/* Create new sports db */
CREATE DATABASE 'sports.db' FROM OS-GETENV('dlc') + '\sports2000.db' REPLACE.

/* Create new qb db */
CREATE DATABASE cDatabase FROM OS-GETENV('dlc') + '\empty8.db' REPLACE.
ASSIGN FILE-INFO:FILE-NAME = cDatabase.
CONNECT VALUE(cDatabase) -1.
RUN prodict/load_df.p (INPUT cDatabase + '.df').

FILE-INFO:FILE-NAME = cDatabase + '.db'.
IF SEARCH(cDatabase + '.db') <> ? THEN 
  MESSAGE "Database" cDatabase "created" VIEW-AS ALERT-BOX INFO.
ELSE   
  MESSAGE "Failed to create database" cDatabase VIEW-AS ALERT-BOX INFO.
  
QUIT.
