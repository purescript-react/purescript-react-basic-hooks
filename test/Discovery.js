'use strict';

if (typeof require !== 'function') {
  throw new Error('Sorry, purescript-spec-discovery only supports NodeJS environments!');
}

var fs = require('fs');
var path = require('path');

function getMatchingModules(pattern) {
  var directories = fs.readdirSync(path.join(__dirname, '..'));
  return directories.filter(function (directory) {
    return (new RegExp(pattern).test(directory));
  }).map(function (name) {
    var module = require(path.join(__dirname, '..', name));
    return (module && typeof module.spec !== 'undefined') ? module.spec : null;
  }).filter(function (x) { return x; });
}

exports.getSpecs = function (pattern) {
  return function () {
    return getMatchingModules(pattern);
  };
};