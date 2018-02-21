// Generated by BUCKLESCRIPT VERSION 2.2.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");

function mk(s) {
  return /* record */[
          /* newState */s,
          /* events : [] */0
        ];
}

function mapState(f, r) {
  return /* record */[
          /* newState */Curry._1(f, r[/* newState */0]),
          /* events */r[/* events */1]
        ];
}

function mapEvent(f, r) {
  return /* record */[
          /* newState */r[/* newState */0],
          /* events */List.map(f, r[/* events */1])
        ];
}

function mapAction(_, r) {
  return /* record */[
          /* newState */r[/* newState */0],
          /* events */r[/* events */1]
        ];
}

function combine(r1, r2) {
  var allEvents = List.append(List.map((function (x) {
              return /* Left */Block.__(0, [x]);
            }), r1[/* events */1]), List.map((function (x) {
              return /* Right */Block.__(1, [x]);
            }), r2[/* events */1]));
  return /* record */[
          /* newState : tuple */[
            r1[/* newState */0],
            r2[/* newState */0]
          ],
          /* events */allEvents
        ];
}

exports.mk = mk;
exports.mapState = mapState;
exports.mapEvent = mapEvent;
exports.mapAction = mapAction;
exports.combine = combine;
/* No side effect */