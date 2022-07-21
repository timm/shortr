var test = require('tape')
  , unhtml = require('./')

test('Converts entity-based escapes', function(t) {
  t.plan(3)
  t.equal(unhtml('Hello &amp; World'), 'Hello & World')
  t.equal(unhtml('Hello &copy; World'), 'Hello © World')
  t.equal(unhtml('Hello &copy; World &amp;&amp;'), 'Hello © World &&')
})

test('Case-insensitive', function(t) {
  t.plan(3)
  t.equal(unhtml('Hello &AMP; World'), 'Hello & World')
  t.equal(unhtml('Hello &AmP; World'), 'Hello & World')
  t.equal(unhtml('Hello &amp; World'), 'Hello & World')
})

test('Converts numerical escapes', function(t) {
  t.plan(2)
  t.equal(unhtml('Hello &#169; World'), 'Hello © World')
  t.equal(unhtml('Hello &#169;&#38; World'), 'Hello ©& World')
})

test('Removes HTML tags', function(t) {
  t.plan(1)
  t.equal(unhtml('<p>Hello<br/>World</p>'), 'HelloWorld')
})

test('All of the above', function(t) {
  t.plan(1)
  t.equal(unhtml('<p>Hello &amp; World &#169;</p>'), 'Hello & World ©')
})
