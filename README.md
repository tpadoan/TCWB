# TCWB

True Concurrency Workbench is a tool for model-checking true concurrency properties, expressed in a logic corresponding to history-preserving bisimilarity, on safe Petri nets.

The implemented algorithm is based on a automata-theoretic technique, which reduces the satisfaction of formulae on models to the non-emptiness of parity tree automata. 

The tool is still a prototype. Many optimizations, functionalities, and quality of life improvements are missing and some are on the way. So please be patient. Hopefully in the future it will be upgraded with many more functions.

## Getting started

Here you can find how to install the tool and what you need for it.

### Prerequisites

While the executables for some operating systems are already provided, the tool can be compiled using an Haskell compiler.

If you just want the executable go ahead and use one of those provided, assuming that there is one for your system.

Otherwise, you need to download the source code, located in the *src* folder, which includes five files:
* *Model.hs*
* *Logic.hs*
* *MC.hs*
* *Commands.hs*
* *TCWB.hs*

Then, you need an Haskell compiler. The standard Haskell compiler is the *Glasgow Haskell Compiler* (GHC).

Note also that to compile the tool you need to have already installed the Haskell package [Aeson](https://hackage.haskell.org/package/aeson) (and all of its dependencies).

### Installing

If you downloaded one of the precompiled executables, then you just need to run it from a console.

Instead, if you have the source files, you can compile them using the Haskell compiler. E.g., the easiest way to do it, assuming you have installed the GHC, is by using the command
```
ghc [-O] --make “TCWB.hs”
```
from the folder where you put the source files. The *-O* flag is an optimising package of compiler flags, but you do not actually need it. The file “TCWB.hs” contains the *main* of the program. Once completed you will obtain an executable to run from a console.

It is recommended to place the executable and the files containing your input models (as will be explained later) in the same directory, or close to each other.

## Source code modules

Currently the source code is split into five modules:
* **Main** - *TCWB.hs* - contains the *main* of the program;
* **Model** - *Model.hs* - contains the functions required to read, interpret, and manipulate Petri nets (their representation);
* **Logic** - *Logic.hs* - contains the functions to parse logic formulae in input, check their syntax and other correctness requirement (explained below);
* **MC** - *MC.hs* - contains the model-checking algorithm;
* **Commands** - *Commands.hs* - contains the functions implementing the commands that can be used to interact with the tool.

## User interface

The tool provides a simple command line interface, where you can input commands to be executed.

To see the list of available commands you can use the command *help*. To get more information about a specific command just type *help \<command\>*.

Using commands you can assign properties and models to names, then use those names in model-checking or other functions.

## Usage guidelines

In the following is described the syntax to be used in order to specify the properties and the models to model-check.

Note that, while formulae are given in input directly, the specification for a Petri net must be saved in an external file. Then the tool will read it from the file path given in input.

### Syntax of the logic

Formulae are input directly during the execution.

The syntax of the logic is the following:
```
f ::= nu prop(Vars).f | mu prop(Vars).f | Modf Boolf
Boolf ::= & Modf | | Modf | epsilon
Modf ::= {Depends < act var} Modf | [Depends < act var] Modf | T | F | prop(Vars) | (f)
Vars ::= var Vars | epsilon
Depends ::= var Depends | !var Depends | epsilon
```
where *var* is a variable, e.g.: x, *act* is a label, e.g.: a, and *prop* is a proposition, e.g.: Z. As for usual grammars, *epsilon* means empty/nothing.

In *Depends* just variables names represent causal requirements, while names with a ‘!’ before, e.g., !x, represent independence requirements. There is no order on the variables in *Depends*, you can write, e.g., x !y z, where only y is an independence requirement.

Some symbols can be omitted in certain circumstances. For instance, after propositions the parenthesis are not need if *Vars* is empty. Another example is that you do not need to put the symbol ‘<’ after empty *Depends*. Also, when specifying the label for an action in modal connectives, a useful wildcard is the symbol ‘_’ which means *any label*.

It is fundamental to comply with the grammar, in particular to correctly parenthesize formulae.

Some examples of formulae are:
```
mu X. {a z} T | [_ z] X
{a x} (nu Y(x). {x < a y} Y(y))
{a x} {!x < b y} (nu X(x y). {!x y < b z} X(x z))
```

All formulae must also satisfy the following requirements:
* formulae must be closed (they have no free variables or proposition);
* fixpoint formulae must have exactly all the variables used (free) in their inner subformula declared after their proposition;
* propositions must always appear together with the number of variables they had when introduced, e.g., the formula<br/>nu X(x y). \[_ z\] X(z) is wrong.

### Specifying Petri nets

Petri nets are specified in separate files, one each. The files must be written following the JSON format.

A safe Petri net is defined by three things:
* a set of **places**,
* a set of **transitions**,
* an **initial marking**.

Those are also the three things to specify in the JSON file.

Here is a simple example:
```
{
  "places": [{"id": 0}, {"id": 1}, {"id": 2}, {"id": 3}],
  "transitions": [{"id": 0, "label": "a", "pre": [{"id": 0}], "post": [{"id": 3}]},
                  {"id": 1, "label": "b", "pre": [{"id": 1}], "post": [{"id": 1}]},
                  {"id": 2, "label": "a", "pre": [{"id": 1}], "post": [{"id": 2}]}],
  "initmarking": [{"id": 0}, {"id": 1}]
}
```

The set of *places* is just an array of items identified by an ID which must be a unique number.

Similarly the set of *transitions* is also an array. Each transition has four fields:
* a numeric unique ID;
* a string label, representing the label of the action corresponding to the transition;
* a list of places, namely pre-conditions, which are required for the transition to be fired, and consumed after it has fired;
* a list of places, namely post-conditions, which are produced by the transition after it has fired.

Finally, the initial marking, *initmarking* for short, is another array containing the places which are already available at the start, before any transitions has been fired. 

In all the lists of places, the places are identified using their ID, defined at the beginning.

Several examples of safe Petri nets can be found in the *nets* forlder of this repository.

## Authors

* **Tommaso Padoan** - University of Padova, Department of Mathematics

The theoretical technique, on which the tool is based, has been developed by **Paolo Baldan** and **Tommaso Padoan**, from the University of Padova.
