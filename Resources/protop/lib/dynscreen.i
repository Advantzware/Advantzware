/* lib/dynscreen.i
 *
 */

/*
function dynViewerInit  returns logical ( f as handle, dcName as character, ttName as character, evtName as character, dispOrder as integer ) in super.
function dynBrowserInit returns logical ( f as handle, dcName as character, ttName as character, evtName as character, dispOrder as integer ) in super.
 */

function dynScreenUpdate returns logical ( data as longchar ) in super.
function rowDisplay returns logical ( z as handle ) in super.

function getBrowseColumnByName returns handle ( b as handle, n as character ) in super.

