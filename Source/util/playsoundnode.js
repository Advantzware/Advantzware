try {
  const path = require('path')

  const sound = require('sound-play')
  var myArgs = process.argv.slice(1);
  sound.play( myArgs[1])
} catch (err) {
  console.log(err)
}

