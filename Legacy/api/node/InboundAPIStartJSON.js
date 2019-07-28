const express = require('express')
const app = express()
//this is common functions library 
var lib = require("./lib.js");
// the port which Node can listen on
const port = 3000
// this should be test to production
app.set("env", "production");
// InboundAPIRoutesJSON.js has all JSON based routes definitions
var routes = require('./InboundAPIRoutesJSON.js');
app.use('/api', routes);
const dateTime = lib.getDateString() + ' ' + lib.getTimeString();
// this makes Node listen on the port 
app.listen(port, () => console.log(`${dateTime}   Node server started on port ${port}!`));
