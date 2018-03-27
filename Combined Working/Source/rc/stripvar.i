/* stripvar.i  Strip Menu Variables, used with strip.p
**
**          Path:           include/stripvar.i
**          Used by:        lib/strip.p and programs that call it
**
** Parameters:   {1}   - "NEW" or null
*/

def {1} shared var strip-help-row    as int no-undo.
                                     /* ROW # FOR HELP PROMPT MESSAGES */

def {1} shared var strip-menu-row    as int no-undo.
                                     /* ROW NUMBER FOR MENU    */

def {1} shared var strip-name      as char format "X(12)" no-undo.
                                     /* NAME OF MINI-MENU RECORD TO USE */

def {1} shared var strip-depth     as int no-undo.
                                     /* MENU LEVEL (1-10)     */

def {1} shared var strip-list      as char no-undo.
                                     /* LITERAL VALUE LIST    */

def {1} shared var strip-prompt    as char format "X(76)" no-undo.
                                     /* PROMPT FOR LIST  */

def {1} shared var strip-sel as char format "X(40)" extent 10 no-undo.
                                     /* CHOICE(S) SELECTED COME BACK HERE  */

def {1} shared var strip-F4        as logi no-undo.
                                     /* YES = USER HIT F4     */


if "{1}" <> "" then
   do:
       assign strip-menu-row = screen-lines + 1
              strip-help-row = screen-lines + 3
              strip-name     = ""
              strip-list     = ""
              strip-prompt   = ""
              strip-sel      = ""
              strip-depth    = 1.
   end.
