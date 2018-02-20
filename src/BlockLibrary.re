open Shared;

module Result2 = {
  open LeftRight;

  type t('state, 'action, 'event) = {
    newState: 'state,
    events: list('event)
  };

  let combine = (r1: t('state1, 'action1, 'event1), r2: t('state2, 'action2, 'event2)) => {
    let allEvents = List.append(r1.events |> List.map(x => Left(x)), r2.events |> List.map(x => Right(x)));
    { newState: (r1.newState, r2.newState),
      events: allEvents }
  };

  let mapState = (f: ('state1 => 'state2)) => (r: t('state1, 'action1, 'event1)) => {
    { newState: f(r.newState),
      events: r.events }
  };

  let mapEvent = (f: ('event1 => 'event2)) => (r: t('state1, 'action1, 'event1)) => {
    { newState: r.newState,
      events: r.events |> List.map(f) }
  };

  let mapAction = (f: ('action1 => 'action2)) => (r: t('state1, 'action1, 'event1)): t('state1, 'action2, 'event1) => {
    { newState: r.newState,
      events: r.events }
  };  
};

module Block2 = {
  type t('init, 'state, 'action, 'event, 'model, 'value) = {
    initialize: 'init => Result2.t('state, 'action, 'event),
    handle: 'state => 'action => Result2.t('state, 'action, 'event),
    viewModel: 'state => ('action => unit) => 'model,
    getValue: 'state => 'value
  };

  let mapInit = (f: ('init1 => 'init2)) => (block: t('init2,_,_,_,_,_)) => {
    let initialize = (init: 'init1) => block.initialize(f(init));
    /* let initialize2 = f @@ block.initialize; */
    { initialize, handle: block.handle, viewModel: block.viewModel, getValue: block.getValue }
  };

  let mapValue = (f: ('value1 => 'value2), block: t(_,'state,_,_,_,'value1)) => {
    let getValue = (s: 'state) => block.getValue(s) |> f;
    /* let getValue = block.getValue @@ f; */
    { initialize: block.initialize, handle: block.handle, viewModel: block.viewModel, getValue }
  };

  let combine = (b1: t('init1,'state1,'action1,'event1,'model1,'value1), 
                 b2: t('init2,'state2,'action2,'event2,'model2,'value2)) => {
    let initialize ((a,b)) = {
      let res1 = b1.initialize(a);
      let res2 = b2.initialize(b);
      Result2.combine(res1, res2)
    };
    let handle = (state: ('state1, 'state2)) => (action: LeftRight.t('action1, 'action2)) => {
      switch (action) {
      | LeftRight.Left(a) => 
        let res = b1.handle(fst(state), a);
        { Result2.newState: (res.newState, snd(state)), 
          events: (res.events |> List.map (x => LeftRight.Left(x))) }
      | LeftRight.Right(a) => 
        let res = b1.handle(snd(state), a);
        { Result2.newState: (fst(state), res.newState), 
          events: (res.events |> List.map (x => LeftRight.Right(x))) }
      }
    };
    let viewModel = ((state1: 'state1, state2: 'state2)) => (dispatch: LeftRight.t('action1, 'action2) => unit) => {
      let vm1 = b1.viewModel(state1, a => dispatch(Left(a)));
      let vm2 = b2.viewModel(state2, a => dispatch(Right(a)));
      (vm1,vm2)
    };
    let getValue = ((state1: 'state1, state2: 'state2)) => {
      (b1.getValue(state1), b2.getValue(state2))
    };

    { initialize, handle, viewModel, getValue }
  };

  let either = (b1: t('init1,'state1,'action1,'event1,'model1,'value1), 
                 b2: t('init2,'state2,'action2,'event2,'model2,'value2)) => {
    let initialize (init: LeftRight.t('init1, 'init2)) = {
      switch (init){
      | LeftRight.Left (init) => 
        b1.initialize(init) 
        |> Result2.mapAction (x => LeftRight.Left(x))
        |> Result2.mapState (x => LeftRight.Left(x))
        |> Result2.mapEvent (x => LeftRight.Left(x))
      | LeftRight.Right (init) =>
        b2.initialize(init) 
        |> Result2.mapAction (x => LeftRight.Right(x))
        |> Result2.mapState (x => LeftRight.Right(x))
        |> Result2.mapEvent (x => LeftRight.Right(x))
      }
    };

    let handle = (state: LeftRight.t('state1, 'state2)) => (action: LeftRight.t('action1, 'action2)) => {
      switch (action) {
      | LeftRight.Left(a) => 
        switch (state) {
        | LeftRight.Left(s) =>
          b1.handle(s, a)
          |> Result2.mapAction (x => LeftRight.Left(x))
          |> Result2.mapState (x => LeftRight.Left(x))
          |> Result2.mapEvent (x => LeftRight.Left(x))
        | LeftRight.Right(_s) =>
          { newState: state, events: [] }
        }
      | LeftRight.Right(a) => 
        switch (state) {
        | LeftRight.Right(s) =>
          b1.handle(s, a)
          |> Result2.mapAction (x => LeftRight.Right(x))
          |> Result2.mapState (x => LeftRight.Right(x))
          |> Result2.mapEvent (x => LeftRight.Right(x))
        | LeftRight.Left(_s) =>
          { newState: state, events: [] }
        }
      }
    };

    let viewModel = (state: LeftRight.t('state1, 'state2)) => (dispatch: LeftRight.t('action1, 'action2) => unit) => {
      switch (state) {
      | LeftRight.Left(s) => 
        b1.viewModel(s, a => dispatch(Left(a)))
      | LeftRight.Right(s) => 
        b2.viewModel(s, a => dispatch(Right(a)))
      }
    };

    let getValue = (state: LeftRight.t('state1, 'state2)) => {
      switch (state) {
        | LeftRight.Left(s) => 
          b1.getValue(s) |> (x => LeftRight.Left(x))
        | LeftRight.Right(s) => 
          b2.getValue(s) |> (x => LeftRight.Right(x))
        }
      };

    { initialize, handle, viewModel, getValue }
  };
};
