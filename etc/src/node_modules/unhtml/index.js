var entities = require('html5-entities')
  , quotemeta = require('quotemeta')

function convertEntityKeys(entity) {
  var lowercase = entity.toLowerCase().slice(0, -1)
    , value = entities[entity]

  delete entities[entity]
  entities[lowercase] = value

  return lowercase
}

var characterExp = Object.keys(entities)
    .map(convertEntityKeys)
    .map(quotemeta)
    .join('|')

characterExp = new RegExp('\\&(' + characterExp + ')\\;', 'gi')

function unhtml(string) {
  string = String(string || '')
    // remove HTML tags
    .replace(/<\/?[^<>]*>/gi, '')
    // Convert &#number; codes
    .replace(/\&\#(\d+);/g, function(whole, code) {
      return String.fromCharCode(code)
    })
    // Convert &amp; and co.
    .replace(characterExp, function(match, html) {
      return entities[html.toLowerCase()]
    })

  return string
}

module.exports = unhtml
module.exports.entities = entities
