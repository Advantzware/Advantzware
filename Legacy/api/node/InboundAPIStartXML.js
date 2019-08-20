const express = require('express');
const app = express();
//this is common functions library 
var lib = require("./lib.js");
const {config} = require("./config.js");
// the port which Node can listen on
const port = config.portXML;
// the ip address of server on which Node runs
const ip = config.IPXML;
// this should be test to production
app.set("env", "production");
// InboundAPIRoutesXML.js has all XML based routes definitions
var routes = require('./InboundAPIRoutesXML.js');
app.use('/api', routes);
const dateTime = lib.getDateString() + ' ' + lib.getTimeString();
// this makes Node listen on the port 
app.listen(port, ip, () => console.log(`${dateTime}   Node server started on port ${port}!`));
