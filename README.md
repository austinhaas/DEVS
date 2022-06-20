# DEVS

A discrete-event simulation system based on the DEVS formalism.

This is my personal implementation of Dynamic Parallel DEVS with Ports. It is
based on my interpretation of the literature. I've tried to make it functional
and efficient.

## Usage

This is intended to be used as a library and SDK.

See the docstrings for documentation.

See the tests for example usage.

See [this namespace](./src/pettomato/devs/examples.cljc) for example models.

See [these notes](./doc/notes.org) for more information on the implementation.

### Logging system

A flexible logging system can be used to trace a simulation.

See [pettomato.devs.trace](./src/pettomato/devs/trace.cljc) and [pettomato.devs.trace-format](./src/pettomato/devs/trace_format.cljc).

## Development

Run `make` to display available commands.

## Reference

* [DEVS on Wikipedia](https://en.wikipedia.org/wiki/DEVS)
* Zeigler, Bernard P., Tag Gon Kim, and Herbert Praehofer. "Theory of Modeling and Simulation." (2000).
* Zeigler, Bernard P., Alexandre Muzy, and Ernesto Kofman. Theory of modeling and simulation: discrete event & iterative system computational foundations. Academic press, 2018.
* Barros, Fernando J. "Modeling formalisms for dynamic structure systems." ACM Transactions on Modeling and Computer Simulation (TOMACS) 7.4 (1997): 501-515.
* Barros, Fernando J. "Abstract simulators for the DSDE formalism." 1998 Winter Simulation Conference. Proceedings (Cat. No. 98CH36274). Vol. 1. IEEE, 1998.
* Barros, Fernando J. "Representation of dynamic structure discrete event models: A systems theory approach." Discrete Event Modeling and Simulation Technologies. Springer, New York, NY, 2001. 167-185.
* Uhrmacher, Adelinde M. "Dynamic structures in modeling and simulation: a reflective approach." ACM Transactions on Modeling and Computer Simulation (TOMACS) 11.2 (2001): 206-232.
* Cho, Young Kwan, Xiaolin Hu, and Bernard P. Zeigler. "The RTDEVS/CORBA environment for simulation-based design of distributed real-time systems." Simulation 79.4 (2003): 197-210.
* Himmelspach, Jan, and Adelinde M. Uhrmacher. "Processing dynamic PDEVS models." The IEEE Computer Society's 12th Annual International Symposium on Modeling, Analysis, and Simulation of Computer and Telecommunications Systems, 2004.(MASCOTS 2004). Proceedings.. IEEE, 2004.
* Hu, Xiaolin, et al. "Variable structure in DEVS component-based modeling and simulation." Simulation 81.2 (2005): 91-102.
* Zeigler, Bernard P., and Hessam S. Sarjoughian. "Introduction to devs modeling and simulation with java: Developing component-based simulation models." Technical Document, University of Arizona (2003).
* Shang, Hui, and Gabriel Wainer. "A simulation algorithm for dynamic structure DEVS modeling." Proceedings of the 2006 Winter Simulation Conference. IEEE, 2006.
* Shang, Hui, and Gabriel A. Wainer. "A flexible dynamic structure DEVS algorithm towards real-time systems." SCSC. 2007.

## License

Copyright © 2022 Austin Haas

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
