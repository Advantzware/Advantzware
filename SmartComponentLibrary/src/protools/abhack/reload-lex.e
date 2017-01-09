/*
 (c) Jice 14/3/2014
 Purpose: Reload the user lexer
 _clex_load('C:\TEMP\OpenEdge\WRK\proedit\user.vlx');
*/

#include 'slick.sh'

defmain() {
    _str lexer_filename;
    lexer_filename = _config_path():+USER_LEXER_FILENAME;
    /*_message_box(lexer_filename);*/
    _clex_load(lexer_filename);
}
