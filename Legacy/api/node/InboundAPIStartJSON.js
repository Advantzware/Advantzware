const express = require('express')
const app = express()

// the port which Node can listen on
const port = 8083

// this should be test to production
app.set("env", "production");

// this is required to parse JSON request data */
var bodyParser = require('body-parser');
app.use(bodyParser.urlencoded({ extended: true }));
app.use(bodyParser.json());
app.use(function(err, req, res, next) {
  if (err instanceof SyntaxError && err.status === 400 && "body" in err) {
    res.status(400).send({ response_code: 400, response_message: "bad request" });
  } else next();
});

// InboundAPIRoutes.js has all routes defined 
var routes = require('./InboundAPIRoutes.js');
app.use('/api', routes);

// this makes Node listen on the port 
app.listen(port, () => console.log(`Node server listening on port ${port}!`))