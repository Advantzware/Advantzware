const express = require('express')
const app = express()
//this is common functions library 
var lib = require("./lib.js");
const {config} = require("./config.js");
// the port which Node can listen on
const port = config.portJSON
// the ip address of server on which Node runs
const ip = config.IPJSON;
// this should be test to production
app.set("env", "production");
//console.log("USER_ID:" + USER_ID + " | USER_KEY:" + USER_KEY); 
// InboundAPIRoutesJSON.js has all JSON based routes definitions
var routes = require('./InboundAPIRoutesJSON.js');
app.use('/api', routes);
const dateTime = lib.getDateString() + ' ' + lib.getTimeString();
// this makes Node listen on the port 
app.listen(port,ip, () => console.log(`${dateTime}   Node server started on port ${port}!`));
