// Generated by BUCKLESCRIPT VERSION 2.2.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Result2$UiLibrary = require("./Result2.bs.js");

function getBlockInfo(block, result) {
  return /* record */[
          /* state */result[/* newState */0],
          /* value */Curry._1(block[/* getValue */3], result[/* newState */0]),
          /* events */result[/* events */1]
        ];
}

function applyReaction(reaction, block, result) {
  switch (reaction.tag | 0) {
    case 0 : 
        return Curry._1(block[/* initialize */0], reaction[0]);
    case 1 : 
        var s = reaction[0];
        return Result2$UiLibrary.mapState((function () {
                      return s;
                    }), result);
    case 2 : 
        var result2 = Curry._2(block[/* handle */1], result[/* newState */0], reaction[0]);
        return /* record */[
                /* newState */result2[/* newState */0],
                /* events */List.append(result[/* events */1], result2[/* events */1])
              ];
    case 3 : 
        return /* record */[
                /* newState */result[/* newState */0],
                /* events */List.append(result[/* events */1], /* :: */[
                      reaction[0],
                      /* [] */0
                    ])
              ];
    
  }
}

function mkBlock(getReactions, block) {
  var handle = function (state, action) {
    var result = Curry._2(block[/* handle */1], state, action);
    var blockInfo = getBlockInfo(block, result);
    var _result = result;
    var _reactions = $$Array.to_list(Curry._1(getReactions, /* tuple */[
              action,
              blockInfo
            ]));
    while(true) {
      var reactions = _reactions;
      var result$1 = _result;
      if (reactions) {
        var newResult = applyReaction(reactions[0], block, result$1);
        _reactions = reactions[1];
        _result = newResult;
        continue ;
        
      } else {
        return result$1;
      }
    };
  };
  return /* record */[
          /* initialize */block[/* initialize */0],
          /* handle */handle,
          /* viewModel */block[/* viewModel */2],
          /* getValue */block[/* getValue */3]
        ];
}

function mkBlock2(getReactions, param) {
  var block2 = param[1];
  var block1 = param[0];
  var mergeResultsIntoOne = function (param) {
    var result2 = param[1];
    var result1 = param[0];
    var newState_000 = result1[/* newState */0];
    var newState_001 = result2[/* newState */0];
    var newState = /* tuple */[
      newState_000,
      newState_001
    ];
    var events = List.concat(/* :: */[
          List.map((function (x) {
                  return /* Left */Block.__(0, [x]);
                }), result1[/* events */1]),
          /* :: */[
            List.map((function (x) {
                    return /* Right */Block.__(1, [x]);
                  }), result2[/* events */1]),
            /* [] */0
          ]
        ]);
    return /* record */[
            /* newState */newState,
            /* events */events
          ];
  };
  var initialize = function (param) {
    var result1 = Curry._1(block1[/* initialize */0], param[0]);
    var result2 = Curry._1(block2[/* initialize */0], param[1]);
    return mergeResultsIntoOne(/* tuple */[
                result1,
                result2
              ]);
  };
  var handle = function (param, action) {
    var state2 = param[1];
    var state1 = param[0];
    var match;
    if (action.tag) {
      var result1 = /* record */[
        /* newState */state1,
        /* events : [] */0
      ];
      var result2 = Curry._2(block2[/* handle */1], state2, action[0]);
      match = /* tuple */[
        result1,
        result2
      ];
    } else {
      var result1$1 = Curry._2(block1[/* handle */1], state1, action[0]);
      var result2$1 = /* record */[
        /* newState */state2,
        /* events : [] */0
      ];
      match = /* tuple */[
        result1$1,
        result2$1
      ];
    }
    var result2$2 = match[1];
    var result1$2 = match[0];
    var applyReactions = function (_param, _reactions) {
      while(true) {
        var param = _param;
        var reactions = _reactions;
        var result2 = param[1];
        var result1 = param[0];
        if (reactions) {
          var head = reactions[0];
          var newResults;
          newResults = head.tag ? /* tuple */[
              result1,
              applyReaction(head[0], block2, result2)
            ] : /* tuple */[
              applyReaction(head[0], block1, result1),
              result2
            ];
          _reactions = reactions[1];
          _param = newResults;
          continue ;
          
        } else {
          return /* tuple */[
                  result1,
                  result2
                ];
        }
      };
    };
    var blockInfo1 = getBlockInfo(block1, result1$2);
    var blockInfo2 = getBlockInfo(block2, result2$2);
    return mergeResultsIntoOne(applyReactions(/* tuple */[
                    result1$2,
                    result2$2
                  ], Curry._1(getReactions, /* tuple */[
                        action,
                        blockInfo1,
                        blockInfo2
                      ])));
  };
  var viewModel = function (param, dispatch) {
    var model1 = Curry._2(block1[/* viewModel */2], param[0], (function (x) {
            return Curry._1(dispatch, /* Left */Block.__(0, [x]));
          }));
    var model2 = Curry._2(block2[/* viewModel */2], param[1], (function (x) {
            return Curry._1(dispatch, /* Right */Block.__(1, [x]));
          }));
    return /* tuple */[
            model1,
            model2
          ];
  };
  var getValue = function (state) {
    return /* tuple */[
            Curry._1(block1[/* getValue */3], state[0]),
            Curry._1(block2[/* getValue */3], state[1])
          ];
  };
  return /* record */[
          /* initialize */initialize,
          /* handle */handle,
          /* viewModel */viewModel,
          /* getValue */getValue
        ];
}

exports.getBlockInfo = getBlockInfo;
exports.applyReaction = applyReaction;
exports.mkBlock = mkBlock;
exports.mkBlock2 = mkBlock2;
/* No side effect */