# des

A discrete-event system based on the DEVS formalism.

## Usage

### Models are defined as a map with the following fields and values:

* :input-schema - Not currently used.

* :output-schema - Not currently used.

* :state - The initial state of the model.

* :ext-update - A function that takes 3 arguments: the current state
  of the model; the time elapsed since the current state began; and
  the new value arriving on an external port, which is a vector of two
  values: a symbol identifying the port receiving the input, and the
  input value. Returns a new state.

* :int-update - A function that takes a state and returns a new state.

* :output - A function that takes a state and returns a vector of two
  values: a symbol representing the output port and the output value.

* :wait - A function that takes a state and returns a number
  indicating the number of milliseconds remaining until the next state
  transition.

Note that :output is called just before :int-update. For example, if
:wait returns 1000, then it is expected that 1 second later :output
will be called, immediately followed by :int-update.

### Coupled models are defined as a map with the following fields and values:

* :input-schema - Not currently used.

* :output-schema - Not currently used.

* :components - A map from identifier (e.g., a symbol) to a model or
  coupled model.

* :ext-input-coupling - A set of pairs of the form [input-port-id [component-id component-input-port-id]].

* :int-input-coupling - A set of pairs of the form [[component-id component-output-port-id] output-port-id].

* :int-coupling - A set of pairs of the form [[component1-id component1-output-port-id] [component2-id component2-input-port-id]].

* :select - A comparator that takes two component ids and returns a value indicating which has a higher priority.

## Example

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
