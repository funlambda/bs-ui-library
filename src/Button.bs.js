// Generated by BUCKLESCRIPT VERSION 2.2.0, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function initialize() {
  return /* record */[
          /* newState : record */[/* isHovered : false */0],
          /* events : [] */0
        ];
}

function handle(state, action) {
  switch (action) {
    case 0 : 
        return /* record */[
                /* newState */state,
                /* events : :: */[
                  /* Clicked */0,
                  /* [] */0
                ]
              ];
    case 1 : 
        return /* record */[
                /* newState : record */[/* isHovered : true */1],
                /* events : [] */0
              ];
    case 2 : 
        return /* record */[
                /* newState : record */[/* isHovered : false */0],
                /* events : [] */0
              ];
    
  }
}

function viewModel(state, dispatch) {
  return {
          isHovered: state[/* isHovered */0],
          onClick: (function () {
              return Curry._1(dispatch, /* OnClick */0);
            }),
          onMouseEnter: (function () {
              return Curry._1(dispatch, /* OnMouseEnter */1);
            }),
          onMouseLeave: (function () {
              return Curry._1(dispatch, /* OnMouseLeave */2);
            })
        };
}

function getValue() {
  return /* () */0;
}

var block = /* record */[
  /* initialize */initialize,
  /* handle */handle,
  /* viewModel */viewModel,
  /* getValue */getValue
];

exports.initialize = initialize;
exports.handle = handle;
exports.viewModel = viewModel;
exports.getValue = getValue;
exports.block = block;
/* No side effect */